template <int N>
// FIXME: newTmpVarName has to be on the HEAP!!
struct TmpVarName {
    char c[N];
    TmpVarName(int num, char c) {
        snprintf(&buf.c, N - 1, "_%c%d", c, num);
        c[N - 1] = 0;
    }
    operator const char*() { return c; }
};

class Optimizer {
    // static TmpVarName newTmpVarName(int num, char c) {
    //     // char buf[8];
    //     chars8 buf;
    //     int l = snprintf(&buf.c, 7, "_%c%d", c, num);
    //     buf.c return buf;
    // }

    Expr* findPromotionCandidate(Expr* expr) {
        assert(expr);
        Expr* ret;

        // what about func args?
        switch (expr->kind) {
        case tkFunctionCallResolved:
            // promote innermost first, so check args
            if (expr->left and (ret = findPromotionCandidate(expr->left)))
                return ret;
            else if (mustPromote(expr->func->selector))
                return expr;
            break;

        case tkSubscriptResolved:
            // TODO: here see if the subscript itself needs to be promoted up
            return findPromotionCandidate(expr->left);

        case tkSubscript: return findPromotionCandidate(expr->left);

        case tkKeyword_if:
        case tkKeyword_for:
        case tkKeyword_else:
        case tkKeyword_elif:
        case tkKeyword_while:
            if (expr->left) return findPromotionCandidate(expr->left);
            // body will be handled by parent scope

        case tkVarAssign:
            if ((ret = findPromotionCandidate(expr->var->init))) return ret;
            break;

        case tkFunctionCall: // unresolved
            // assert(0);
            unreachable("unresolved call %s\n", expr->string);
            if ((ret = findPromotionCandidate(expr->left))) return ret;
            break;

        default:
            if (expr->prec) {
                if (expr->right and (ret = findPromotionCandidate(expr->right)))
                    return ret;
                if (!expr->unary)
                    if ((ret = findPromotionCandidate(expr->left))) return ret;
            }
        }
        return NULL;
    }
    static void lowerElementalOps(Scope* scope) {
        foreach (Expr*, stmt, scope->stmts) {

            if (isCtrlExpr(stmt) and stmt->body) lowerElementalOps(stmt->body);

            if (!stmt->elemental) continue;

            // wrap it in an empty block (or use if true)
            Expr* ifblk = NEW(Expr);
            ifblk->kind = tkKeyword_if;
            ifblk->left = NEW(Expr);
            ifblk->left->kind = tkNumber;
            ifblk->string = "1";

            // look top-down for subscripts. if you encounter a node with
            // elemental=false, don't process it further even if it may have
            // ranges inside. e.g.
            // vec[7:9] = arr2[6:8] + sin(arr2[-6:-1:-4]) + test[[8,6,5]]
            // + 3 + count(vec[vec < 5]) + M ** x[-8:-1:-4]
            // the matmul above is not
            // elemental, but the range inside it is.
            // the Array_count_filter will be promoted and isnt elemental
            // (unless you plan to set elemental op on boolean subscripts.)
            // Even so, count is a reduce op and will unset elemental.
            // --
            // as you find each subscript, add 2 local vars to the ifblk body
            // so then you might have for the above example :
            // T* vec_p1 = vec->start + 7;
            // // ^ this func could be membptr(a,i) -> i<0 ? a->end-i :
            // a->start+i #define vec_1 *vec_p1 // these could be  Vars with
            // an isCMacro flag T2* arr2_p1 = membptr(arr2, 6); #define arr2_1
            // *arr2_p1 T3* arr2_p2 = membptr(arr2, -6); #define arr2_2 *arr2_p2
            // ...
            // // now add vars for each slice end and delta
            // const T* const vec_e1 = vec->start + 9; // use membptr
            // const T2* const arr2_e1 = arr2->start + 8; // membptr
            // const T3* const arr2_e2 = membptr(arr2, -4);
            // what about test[[8,6,5]]?
            // const T* const vec_d1 = 1;
            // const T2* const arr2_d1 = 1;
            // const T3* const arr2_d2 = -1;
            // ...
            // // the ends (and starts) could be used for BC.
            // ...
            // // now add a check / separate checks for count match and bounds
            // check_span1deq(vec_e1,vec_p1,arr2_e1,arr2_p1,col1,col2,"vec[7:9]","arr2[6:8]",__FILE__,__LINE__);
            // check_span1deq(arr2_e1,arr2_p1,arr2_e2,arr2_p2,col1,col2,"arr2[6:8]","arr2[-6:-4]",__FILE__,__LINE__);
            // check_inbounds1d(vec, vec_p1, vec_e1,col1,
            // "vec[7:9]",__FILE__,__LINE__) check_inbounds1d(arr2, arr2_p1,
            // arr2_e1,col1, "arr2[6:8]",__FILE__,__LINE__) now change the
            // subscripts in the stmt to unresolved idents, and change the ident
            // by appending _1, _2 etc. based on their position. so when they
            // are generated they will refer to the current item of that array.
            // then wrap the stmt in a for expr 'forblk'. put the for expr in
            // ifblk. the active scope is now the for's body. generate the stmt.
            // it should come out in Number form if all went well.. add
            // increments for each ptr. vec_p1 += vec_d1; arr2_p1 += arr2_d1;
            // arr2_p2 += arr2_d2;
            // ...
            // all done, at the end put the ifblk at the spot of stmt in the
            // original scope. stmt is already inside ifblk inside forblk.
        }
    }
    void promoteCandidates(Scope* scope) {
        int tmpCount = 0;
        Expr* pc = NULL;
        List(Expr)* prev = NULL;
        for (Expr& stmt : scope->stmts) {
            // TODO:
            // if (! stmt->promote) {prev=stmts;continue;}

            if (stmt.isCtrlExpr() and stmt->body) promoteCandidates(stmt->body);

        startloop:

            if (!(pc = findPromotionCandidate(stmt))) { // most likely
                prev = stmts;
                continue;
            }
            if (pc == stmt) {
                // possible, less likely: stmt already at toplevel.
                // TODO: in this case, you still have to add the extra arg.
                prev = stmts;
                continue;
            }

            Expr* pcClone = NEW(Expr);
            *pcClone = *pc;

            // 1. add a temp var to the scope
            Var* tmpvar = NEW(Var);

            tmpvar->name = newTmpVarName(++tmpCount, 'p');
            tmpvar->typeSpec = NEW(TypeSpec);
            //        tmpvar->typeSpec->typeType = TYReal64; // FIXME
            // TODO: setup tmpvar->typeSpec
            PtrList_append(&scope->locals, tmpvar);

            // 2. change the original to an ident
            pc->kind = tkIdentifierResolved;
            pc->prec = 0;
            pc->var = tmpvar;

            // 3. insert the tmp var as an additional argument into the call

            if (!pcClone->left)
                pcClone->left = pc;
            else if (pcClone->left->kind != tkOpComma) {
                // single arg
                Expr* com = NEW(Expr);
                // TODO: really should have an  expr ctor
                com->prec = getPrecedence(tkOpComma);
                com->kind = tkOpComma;
                com->left = pcClone->left;
                com->right = pc;
                pcClone->left = com;
            } else {
                Expr* argn = pcClone->left;
                while (
                    argn->kind == tkOpComma and argn->right->kind == tkOpComma)
                    argn = argn->right;
                Expr* com = NEW(Expr);
                // TODO: really should have an  expr ctor
                com->prec = getPrecedence(tkOpComma);
                com->kind = tkOpComma;
                com->left = argn->right;
                com->right = pc;
                argn->right = com;
            }

            // 4. insert the promoted expr BEFORE the current stmt
            //        PtrList_append(prev ? &prev : &self->stmts, pcClone);
            //        PtrList* tmp = prev->next;
            // THIS SHOULD BE in PtrList as insertAfter method
            if (!prev) {
                scope->stmts = PtrList_with(pcClone);
                scope->stmts->next = stmts;
                prev = scope->stmts;
            } else {
                prev->next = PtrList_with(pcClone);
                prev->next->next = stmts;
                prev = prev->next;
            } // List( Expr)* insertionPos = prev ? prev->next : self->stmts;
              //  insertionPos
            //  = insertionPos;
            goto startloop; // it will continue there if no more promotions are
                            // needed

            prev = stmts;
        }
    }
    // static char* newTmpVarName(int num, char c) {
    //     char buf[8];
    //     int l = snprintf(buf, 8, "_%c%d", c, num);
    //     return CString_pndup(buf, l);
    // }
};