
// Extraction scan & Extraction happens AFTER resolving functions!
static Expr* expr_getExtractable(Expr* expr) {
  assert(expr);
  Expr* ret;

  // what about func args?
  switch (expr->kind) {
  case tkFuncCallR:
    // promote innermost first, so check args
    if (expr->left && (ret = expr_getExtractable(expr->left)))
      return ret;
    else if (expr->extract)
      return expr;
    break;

  case tkSubscriptR:
    return expr_getExtractable(expr->left);
    // TODO: here see if the subscript itself needs to be promoted up

  case tkSubscript: return expr_getExtractable(expr->left);

  case tkIf:
  case tkFor:
  case tkElse:
  case tkElif:
  case tkWhile:
  case tkCase:
    if (expr->left) return expr_getExtractable(expr->left);
    // body will be handled by parent scope

  case tkVarDefn:
    if ((ret = expr_getExtractable(expr->var->init))) return ret;
    break;

  case tkFuncCall: // unresolved
    // assert(0);
    unreachable("unresolved call %s\n", expr->str);
    if ((ret = expr_getExtractable(expr->left))) return ret;
    break;

  default:
    if (expr->prec) {
      if (expr->right && (ret = expr_getExtractable(expr->right)))
        return ret;
      if (!expr->unary)
        if ((ret = expr_getExtractable(expr->left))) return ret;
    }
  }
  return NULL;
}

static char* newTmpVarName(int num, char c) {
  char buf[8];
  int l = snprintf(buf, 8, "_%c%d", c, num);
  return cstr_pndup(buf, l);
}

static void scope_scalarize(Scope* scope) {
  foreach (Expr*, stmt, scope->stmts) {

    if (isCtrlExpr(stmt) && stmt->body) scope_scalarize(stmt->body);

    if (!stmt->elemental) continue;

    // wrap it in an empty block (or use if true)
    Expr* ifblk = NEW(Expr);
    ifblk->kind = tkIf;
    ifblk->left = NEW(Expr);
    ifblk->left->kind = tkNumber;
    ifblk->str = "1";

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
    // a->start+i #define vec_1 *vec_p1 // these could be Vars with
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

// MERGE THESE TWO FUNCS!!! promoteVars & extract Vars
// static void scope_promoteVars(Scope* scope) {

//   foreachn(Expr*, stmt, stmts, scope->stmts) if (isCtrlExpr(stmt)
//       && stmt->body) scope_promoteVars(stmt->body);

// }

static void scope_lower(Scope* scope) {
  int tmpCount = 0;
  Expr* pc = NULL;
  List(Expr)* prev = NULL;

  foreachn(Var*, var, var_li, scope->locals) {
    if (!var->promote) continue;
    // you need to know how far up the var should go!
    int up = var->promote;

    Scope* targetScope = scope;
    while (up--
        && (targetScope = targetScope->isLoop ? NULL : targetScope->parent))
      ;
    if (!targetScope) continue;

    // move
    var_li->item = var_li->next;
    li_shift(&targetScope->locals, var);
    var->promote = 0;
  }

  foreachn(Expr*, stmt, stmts, scope->stmts) {
    // TODO:
    // if (! stmt->extract) {prev=stmts;continue;}

    if (isCtrlExpr(stmt) && stmt->body) scope_lower(stmt->body);

  startloop:

    if (!(pc = expr_getExtractable(stmt))) { // most likely
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
    tmpvar->spec = NEW(TypeSpec);
    //        tmpvar->spec->typeType = TYReal64; // FIXME
    // TODO: setup tmpvar->spec
    li_push(&scope->locals, tmpvar);

    // 2. change the original to an ident
    pc->kind = tkIdentR;
    pc->prec = 0;
    pc->var = tmpvar;

    // 3. insert the tmp var as an additional argument into the call

    if (!pcClone->left)
      pcClone->left = pc;
    else if (pcClone->left->kind != tkComma) {
      // single arg
      Expr* com = NEW(Expr);
      // TODO: really should have an astexpr ctor
      com->prec = TokenKind_getPrecedence(tkComma);
      com->kind = tkComma;
      com->left = pcClone->left;
      com->right = pc;
      pcClone->left = com;
    } else {
      Expr* argn = pcClone->left;
      while (argn->kind == tkComma && argn->right->kind == tkComma)
        argn = argn->right;
      Expr* com = NEW(Expr);
      // TODO: really should have an astexpr ctor
      com->prec = TokenKind_getPrecedence(tkComma);
      com->kind = tkComma;
      com->left = argn->right;
      com->right = pc;
      argn->right = com;
    }

    // 4. insert the promoted expr BEFORE the current stmt
    //        li_push(prev ? &prev : &self->stmts, pcClone);
    //        PtrList* tmp = prev->next;
    // THIS SHOULD BE in PtrList as insertAfter method
    if (!prev) {
      scope->stmts = li_with(pcClone);
      scope->stmts->next = stmts;
      prev = scope->stmts;
    } else {
      prev->next = li_with(pcClone);
      prev->next->next = stmts;
      prev = prev->next;
    }
    goto startloop; // it will continue there if no more Extractions are
                    // needed

    prev = stmts;
  }
}
