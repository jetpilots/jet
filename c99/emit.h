#define genLineNumbers 0
#define genCoverage 1
#define genLineProfile 1

///////////////////////////////////////////////////////////////////////////
static void emit_import(ast_import_t* import, int level) {
    char* alias = import->aliasOffset + import->name;
    CString_tr_ip(import->name, '.', '_', 0);
    printf("\n#include \"%s.h\"\n", import->name);
    if (alias) printf("#define %s %s\n", alias, import->name);
    // TODO: remove #defines! There could be a field of any
    // struct with the same name that will get clobbered
    CString_tr_ip(import->name, '_', '.', 0);
}

///////////////////////////////////////////////////////////////////////////
static void undefc_import(ast_import_t* import) {
    // if (import->alias) printf("#undef %s\n", import->alias);
}

///////////////////////////////////////////////////////////////////////////
static void ast_typespec_emit(
    ast_typespec_t* typespec, int level, bool isconst) {
    if (isconst) printf("const ");
    // TODO: actually this depends on the collectionType. In general
    // Array is the default, but in other cases it may be SArray, Array64,
    // whatever
    if (typespec->dims) {
        if (typespec->dims > 1)
            // TODO: this should be TensorND, without type params?
            // well actually there isn't a TensorND, since its not always
            // double thats in a tensor but can be Complex, Range,
            // Reciprocal, Rational, whatever
            // -- sure, but double (and float) should be enough since
            // the other types are rarely needed in a tensor form
            printf("SArray%dD(", typespec->dims);
        else
            printf("SArray(");
    }

    switch (typespec->typeType) {
    case ty_object:
        // objects are always T* const, if meant to be r/o they are
        // const T* const. Later we may have a byval flag to embed structs
        // or pass around by value.
        // leaving it as is for now
        printf("%s", typespec->type->name);
        break;
    case ty_unresolved:
        unreachable("unresolved: '%s' at %d:%d", typespec->name, typespec->line,
            typespec->col);
        printf("%s", *typespec->name ? typespec->name : "Error_Type");
        break;
    default: printf("%s", typetype_e_name(typespec->typeType)); break;
    }

    //     if (isconst ) printf(" const"); // only if a ptr type
    if (typespec->dims /*or typespec->typeType == ty_object*/)
        printf("%s", ")");
    //        if (status == TSDimensionedNumber) {
    //            genc(units, level);
    //        }
}

static void emit_expr(ast_expr_t* expr, int level);

///////////////////////////////////////////////////////////////////////////
static void emit_var(ast_var_t* var, int level, bool isconst) {
    // for C the variables go at the top of the block, without init
    printf("%.*s", level, spaces);
    if (var->typespec) ast_typespec_emit(var->typespec, level + STEP, isconst);
    printf(" %s", var->name);
}

///////////////////////////////////////////////////////////////////////////
// Functions like Array_any_filter, Array_count_filter etc.
// are macros and don't return a value but may set one. For these
// and other such funcs, the call must be moved to before the
// containing statement, and in place of the original call you
// should place a temporary holding the value that would have been
// "returned".
static bool mustPromote(const char* name) {
    // TODO: at some point these should go into a dict or trie or MPH
    // whatever
    if (!strcmp(name, "Array_any_filter")) return true;
    if (!strcmp(name, "Array_all_filter")) return true;
    if (!strcmp(name, "Array_count_filter")) return true;
    if (!strcmp(name, "Array_write_filter")) return true;
    if (!strcmp(name, "Strs_print_filter")) return true;
    return false;
}

///////////////////////////////////////////////////////////////////////////
static void unmarkVisited_expr(ast_expr_t* expr) {
    switch (expr->kind) {
    case tk_identifierResolved:
    case tk_varAssign: expr->var->visited = false; break;
    case tk_functionCallResolved:
    case tk_functionCall: // shouldnt happen
    case tk_subscriptResolved:
    case tk_subscript:
    case tk_keyword_if:
    case tk_keyword_for:
    case tk_keyword_else:
    case tk_keyword_while: unmarkVisited_expr(expr->left); break;
    default:
        if (expr->prec) {
            if (!expr->unary) unmarkVisited_expr(expr->left);
            unmarkVisited_expr(expr->right);
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// given an expr, generate code to print all the resolved vars in it (only
// scalars). for example in f(x + 4) + m + y[5:6], the following should be
// generated
// printf("x = %?\n", x);
// printf("m = %?\n", m);
// checks will print the vars involved in the check expr, if the check
// fails. This routine will be used there.
static void genPrintVars_expr(ast_expr_t* expr, int level) {
    assert(expr);
    // what about func args?
    switch (expr->kind) {
    case tk_identifierResolved:
    case tk_varAssign:
        if (expr->var->visited) break;
        printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level, spaces,
            expr->var->name, typetype_e_format(expr->typeType, true),
            expr->var->name);
        expr->var->visited = true;
        break;

    case tk_period:
        //        {
        //            ast_expr_t* e = expr->right;
        //            while (e->kind==tk_period) e=e->right;
        ////            if (e->var->visited) break;
        //            printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level,
        //            spaces,
        //                   expr->var->name, typetype_e_format(e->typeType,
        //                   true), expr->var->name);
        //        }
        break;

    case tk_functionCallResolved:
    case tk_functionCall: // shouldnt happen
    case tk_subscriptResolved:
    case tk_subscript:
    case tk_keyword_if:
    case tk_keyword_else:
    case tk_keyword_for:
    case tk_keyword_while: genPrintVars_expr(expr->left, level); break;

    default:
        if (expr->prec) {
            if (!expr->unary) genPrintVars_expr(expr->left, level);
            genPrintVars_expr(expr->right, level);
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// Promotion scan & promotion happens AFTER resolving functions!
static ast_expr_t* findPromotionCandidate_expr(ast_expr_t* expr) {
    assert(expr);
    ast_expr_t* ret;

    // what about func args?
    switch (expr->kind) {
    case tk_functionCallResolved:
        // promote innermost first, so check args
        if (expr->left && (ret = findPromotionCandidate_expr(expr->left)))
            return ret;
        else if (mustPromote(expr->func->selector))
            return expr;
        break;

    case tk_subscriptResolved:
        // TODO: here see if the subscript itself needs to be promoted up
        return findPromotionCandidate_expr(expr->left);

    case tk_subscript: return findPromotionCandidate_expr(expr->left);

    case tk_keyword_if:
    case tk_keyword_for:
    case tk_keyword_else:
    case tk_keyword_elif:
    case tk_keyword_while:
        if (expr->left) return findPromotionCandidate_expr(expr->left);
        // body will be handled by parent scope

    case tk_varAssign:
        if ((ret = findPromotionCandidate_expr(expr->var->init))) return ret;
        break;

    case tk_functionCall: // unresolved
        // assert(0);
        unreachable("unresolved call %s\n", expr->string);
        if ((ret = findPromotionCandidate_expr(expr->left))) return ret;
        break;

    default:
        if (expr->prec) {
            if (expr->right && (ret = findPromotionCandidate_expr(expr->right)))
                return ret;
            if (!expr->unary)
                if ((ret = findPromotionCandidate_expr(expr->left))) return ret;
        }
    }
    return NULL;
}

static char* newTmpVarName(int num, char c) {
    char buf[8];
    int l = snprintf(buf, 8, "_%c%d", c, num);
    return CString_pndup(buf, l);
}

///////////////////////////////////////////////////////////////////////////
static bool isCtrlExpr(ast_expr_t* expr) {
    return expr->kind == tk_keyword_if //
        || expr->kind == tk_keyword_for //
        || expr->kind == tk_keyword_while //
        || expr->kind == tk_keyword_else;
}

static bool isLiteralExpr(ast_expr_t* expr) { return false; }
static bool isComparatorExpr(ast_expr_t* expr) { return false; }

///////////////////////////////////////////////////////////////////////////
static void lowerElementalOps_scope(ast_scope_t* scope) {
    foreach (ast_expr_t*, stmt, scope->stmts) {

        if (isCtrlExpr(stmt) && stmt->body) lowerElementalOps_scope(stmt->body);

        if (!stmt->elemental) continue;

        // wrap it in an empty block (or use if true)
        ast_expr_t* ifblk = NEW(ast_expr_t);
        ifblk->kind = tk_keyword_if;
        ifblk->left = NEW(ast_expr_t);
        ifblk->left->kind = tk_number;
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
        // a->start+i #define vec_1 *vec_p1 // these could be ast_vars_t with
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

///////////////////////////////////////////////////////////////////////////
static void promoteCandidates_scope(ast_scope_t* scope) {
    int tmpCount = 0;
    ast_expr_t* pc = NULL;
    list_t(ast_expr)* prev = NULL;
    foreachn(ast_expr_t*, stmt, stmts, scope->stmts) {
        // TODO:
        // if (! stmt->promote) {prev=stmts;continue;}

        if (isCtrlExpr(stmt) && stmt->body) promoteCandidates_scope(stmt->body);

    startloop:

        if (!(pc = findPromotionCandidate_expr(stmt))) { // most likely
            prev = stmts;
            continue;
        }
        if (pc == stmt) {
            // possible, less likely: stmt already at toplevel.
            // TODO: in this case, you still have to add the extra arg.
            prev = stmts;
            continue;
        }

        ast_expr_t* pcClone = NEW(ast_expr_t);
        *pcClone = *pc;

        // 1. add a temp var to the scope
        ast_var_t* tmpvar = NEW(ast_var_t);
        tmpvar->name = newTmpVarName(++tmpCount, 'p');
        tmpvar->typespec = NEW(ast_typespec_t);
        //        tmpvar->typespec->typeType = ty_real64; // FIXME
        // TODO: setup tmpvar->typespec
        list_append(&scope->locals, tmpvar);

        // 2. change the original to an ident
        pc->kind = tk_identifierResolved;
        pc->prec = 0;
        pc->var = tmpvar;

        // 3. insert the tmp var as an additional argument into the call

        if (!pcClone->left)
            pcClone->left = pc;
        else if (pcClone->left->kind != tk_opComma) {
            // single arg
            ast_expr_t* com = NEW(ast_expr_t);
            // TODO: really should have an ast_expr_t ctor
            com->prec = tokenkind_e_getPrecedence(tk_opComma);
            com->kind = tk_opComma;
            com->left = pcClone->left;
            com->right = pc;
            pcClone->left = com;
        } else {
            ast_expr_t* argn = pcClone->left;
            while (argn->kind == tk_opComma && argn->right->kind == tk_opComma)
                argn = argn->right;
            ast_expr_t* com = NEW(ast_expr_t);
            // TODO: really should have an ast_expr_t ctor
            com->prec = tokenkind_e_getPrecedence(tk_opComma);
            com->kind = tk_opComma;
            com->left = argn->right;
            com->right = pc;
            argn->right = com;
        }

        // 4. insert the promoted expr BEFORE the current stmt
        //        list_append(prev ? &prev : &self->stmts, pcClone);
        //        PtrList* tmp = prev->next;
        // THIS SHOULD BE in PtrList as insertAfter method
        if (!prev) {
            scope->stmts = list_with(pcClone);
            scope->stmts->next = stmts;
            prev = scope->stmts;
        } else {
            prev->next = list_with(pcClone);
            prev->next->next = stmts;
            prev = prev->next;
        } // list_t(ast_expr)* insertionPos = prev ? prev->next : self->stmts;
          //  insertionPos
        //  = insertionPos;
        goto startloop; // it will continue there if no more promotions are
                        // needed

        prev = stmts;
    }
}

///////////////////////////////////////////////////////////////////////////
static void emit_scope(ast_scope_t* scope, int level) {
    foreach (ast_var_t*, local, scope->locals)
        if (local->used) {
            emit_var(local, level, false);
            puts(";");
        } // these will be declared at top and defined within the expr list
    foreach (ast_expr_t*, stmt, scope->stmts) {
        if (stmt->kind == tk_lineComment) continue;

        if (genLineNumbers) printf("#line %d\n", stmt->line);
        // if (genCoverage) printf("    _cov_[%d]++;\n", stmt->line - 1);
        // if (genLineProfile) {
        //     printf("    _lprof_tmp_ = getticks();\n");
        //     printf("    _lprof_[%d] += (_lprof_tmp_-_lprof_last__)/100;\n",
        //         stmt->line - 1);
        //     printf("    _lprof_last__ = _lprof_tmp_;\n");
        // }

        // You need to know if the emit_expr will in fact generate
        // something. This is true in general unless it is an unused var init.
        if (stmt->kind != tk_varAssign || stmt->var->used) {

            // if (genCoverage || genLineProfile) //
            //     printf("    /************/ ");
            if (genCoverage)
                printf(
                    "%.*sJET_COVERAGE_UP(%d); \n", level, spaces, stmt->line);
            if (genLineProfile) {
                // printf("    _lprof_tmp_ = getticks();\n");
                printf(
                    "%.*sJET_PROFILE_LINE(%d);\n", level, spaces, stmt->line);
                // printf("    _lprof_last__ = _lprof_tmp_;\n");
            }
            if (genCoverage || genLineProfile) puts(""); // ************/");
        }

        emit_expr(stmt, level);
        if (!isCtrlExpr(stmt) && stmt->kind != tk_keyword_return)
            puts(";");
        else
            puts("");
        // convert this into a flag which is set in the resolution pass

        // here you see if any vars are to be dropped at this point (because
        // they were last used in this expr. A var can only be dropped in its
        // own scope, not an inner or outer scope, so just scan our own vars.
        // In a sense the stmt->line is a local ID for the statement within the
        // scope.
        ast_scope_t* sco = scope;
        do {
            foreach (ast_var_t*, var, sco->locals)
                if (var->used) {
                    // if (var->last_usage)
                    //     printf("%.*s//-- %s %d %d\n", level, spaces,
                    //     var->name,
                    //         var->last_usage, stmt->line);
                    if (var->last_usage == stmt->line) {
                        printf("%.*sDROP(%s,%s,%s,%s);\n", level, spaces,
                            name_typespec(var->typespec), var->name,
                            collectiontype_e_nativeName(
                                var->typespec->collectionType),
                            StorageClassNames[var->storage]);
                        // TODO^ distinguish between stack, heap, mixed,
                        // refcounted drops
                        var->last_usage = 0; // this means var has been dropped.
                    }
                }
        } while (!sco->isLoop // if loop scope, don't walk up
            && (sco = sco->parent) // walk up to the last loop scope
            && sco->parent && !sco->parent->isLoop);
        // all scopes have the global scope as the final parent.
        // what if some scope tries to drop something from there?

        // TODO:
        // Maybe scope should have a lineno. If a loop scope has the last_usage
        // of a parent var, it cannot drop it inside the loop, but it should be
        // done just after the loop ends. Now it will be dropped anyway at
        // owning scope end which may be suboptimal. If you have the scope line,
        // which is the line of the cond expr of if / while etc., you change the
        // last_usage to that line and it gets dropped just after the scope.

        if (throws_expr(stmt)) printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
    }
    // It's possible some vars were not detected in inner scopes and dropped. So
    // let's drop them here. No need to walk up the parent chain here.
    foreach (ast_var_t*, var, scope->locals)
        if (var->used && var->last_usage)
            printf("%.*sdrop(%s); // anyway\n", level, spaces, var->name);
    // ^ these are all the vars whose last_usage could not be matched. This may
    // be because they are in an inner scope. In this case they should be
    // dropped at the end of the scope. Optimize this later so that if there are
    // multiple subscopes and the last usage is in one of them, the drop happens
    // after that subscope and doesn't wait until the very end.
}

///////////////////////////////////////////////////////////////////////////
static void genJson_type(ast_type_t* type) {
    printf("static void %s_json_(const %s self, int nspc) {\n", type->name,
        type->name);

    printf("    printf(\"{\\n\");\n");
    // printf("    printf(\"\\\"_type_\\\": \\\"%s\\\"\");\n", type->name);
    // if (type->body->locals) printf("    printf(\",\\n\");\n");

    // TODO: move this part into its own func so that subclasses can ask the
    // superclass to add in their fields inline
    foreachn(ast_var_t*, var, vars, type->body->locals) {
        if (!var /*or not var->used*/) continue;
        printf("    printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n",
            var->name);
        const char* valueType = typeName_expr(var->init);
        printf("    %s_json_(self->%s, nspc+4);\n    printf(\"", valueType,
            var->name);
        if (vars->next) printf(",");
        printf("\\n\");\n");
    }
    printf("    printf(\"%%.*s}\", nspc, _spaces_);\n");
    printf("}\nMAKE_json_wrap_(%s)\n//MAKE_json_file(%s)\n", type->name,
        type->name);
    // printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
    //        "%s_json_wrap_(x); }\n\n",
    //     type->name, type->name);
}

///////////////////////////////////////////////////////////////////////////
static void genJsonReader_type(ast_type_t* type) { }

static const char* functionEntryStuff_UNESCAPED
    = "    STACKDEPTH_UP; DO_STACK_CHECK;\n";

// static const char* functionEntryStuff_UNESCAPED__old
//     = "    STACKDEPTH_UP\n"
//       "#ifndef NOSTACKCHECK\n"
//       //   "    // printf(\"%8lu %8lu\\n\",_scUsage_, _scSize_);\n"
//       "    if (_scUsage_ >= _scSize_) {\n"
//       "#ifdef DEBUG\n"
//       "        _scPrintAbove_ = _scDepth_ - _btLimit_;\n"
//       "        printf(\"\\e[31mfatal: stack overflow at call depth %lu.\\n "
//       "in %s\\e[0m\\n\", _scDepth_, sig_);\n"
//       "        printf(\"\\e[90mBacktrace (innermost first):\\n\");\n"
//       "        if (_scDepth_ > 2*_btLimit_)\n"
//       "            printf(\"    limited to %d outer and %d inner "
//       "entries.\\n\", _btLimit_, _btLimit_);\n"
//       "        printf(\"[%lu] \\e[36m%s\\n\", _scDepth_, callsite_);\n"
//       "#else\n"
//       "        printf(\"\\e[31mfatal: stack  overflow at call depth "
//       "%lu.\\e[0m\\n\",_scDepth_);\n"
//       "#endif\n"
//       "        DONE\n    }\n"
//       "#endif\n";
static const char* functionExitStuff_UNESCAPED
    = "    return DEFAULT_VALUE;\n"
      "uncaught: HANDLE_UNCAUGHT;\n"
      "backtrace: SHOW_BACKTRACE_LINE;\n"
      "return_: STACKDEPTH_DOWN;\n"
      "    return DEFAULT_VALUE;";

// static const char* functionExitStuff_UNESCAPED_old
// = "    return DEFAULT_VALUE;\n"
//   "error:\n"
//   "#ifdef DEBUG\n"
//   "    eprintf(\"error: %s\\n\",_err_);\n"
//   "#endif\n"
//   "backtrace:\n"
//   "#ifdef DEBUG\n"
//   "    if (_scDepth_ <= _btLimit_ || _scDepth_ > _scPrintAbove_)\n"
//   "        printf(\"\\e[90m[%lu] \\e[36m%s\\n\", _scDepth_, callsite_);\n"
//   "    else if (_scDepth_ == _scPrintAbove_)\n"
//   "        printf(\"\\e[90m... truncated ...\\e[0m\\n\");\n"
//   "#endif\n"
//   "done:\n"
//   "    STACKDEPTH_DOWN\n"
//   "    return DEFAULT_VALUE;";

///////////////////////////////////////////////////////////////////////////
static void printStackUsageDef_func(size_t stackUsage) {
    printf("#define MYSTACKUSAGE (%lu + 6*sizeof(void*) + "
           "IFDEBUGELSE(sizeof(char*),0))\n",
        stackUsage);
    // printf("#ifdef DEBUG\n"
    //        "#define MYSTACKUSAGE (%lu + 6*sizeof(void*) + sizeof(char*))\n"
    //        "#else\n"
    //        "#define MYSTACKUSAGE (%lu + 6*sizeof(void*))\n"
    //        "#endif\n",
    //     stackUsage, stackUsage);
}

///////////////////////////////////////////////////////////////////////////
static void emit_type(ast_type_t* type, int level) {
    if (!type->body || !type->analysed || type->isDeclare) return;
    // if (! type->body or not type->analysed) return;
    const char* const name = type->name;
    printf("#define FIELDS_%s \\\n", name);
    foreach (ast_var_t*, var, type->body->locals) {
        if (!var /*or not var->used*/) continue;
        // It's not so easy to just skip 'unused' type members.
        // what if I just construct an object and print it?
        // I expect to see the default members. But if they
        // haven't been otherwise accessed, they are left out.
        emit_var(var, level + STEP, false);
        printf("; \\\n");
    }
    printf("\n\nstruct %s {\n", name);

    if (type->super) {
        printf("    FIELDS_");
        ast_typespec_emit(type->super, level, false);
        printf("\n");
    }

    printf("    FIELDS_%s\n};\n\n", name);
    printf("static const char* %s_name_ = \"%s\";\n\n", name, name);
    printf("static %s %s_alloc_() {\n    return _Pool_alloc_(&gPool_, "
           "sizeof(struct %s));\n}\n\n",
        name, name, name);
    printf("static %s %s_init_(%s self) {\n", name, name, name);

    foreach (ast_var_t*, var, type->body->locals) // if (var->used)
        printf("#define %s self->%s\n", var->name, var->name);

    foreach (ast_expr_t*, stmt, type->body->stmts) {
        if (!stmt || stmt->kind != tk_varAssign || !stmt->var->init)
            //            or not stmt->var->used)
            continue;
        printf("%.*s%s = ", level + STEP, spaces, stmt->var->name);
        emit_expr(stmt->var->init, 0);
        puts(";");
        if (throws_expr(stmt->var->init))
            puts("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    foreach (ast_var_t*, var, type->body->locals) // if (var->used)
        printf("#undef %s \n", var->name);

    printf("    return self;\n}\n\n");

    printStackUsageDef_func(48);
    printf("#define DEFAULT_VALUE NULL\n"
           "monostatic %s %s_new_(IFDEBUG(const char* callsite_)) {\n"
           "IFDEBUG(static const char* sig_ = \"%s()\");\n",
        name, name, name);
    puts(functionEntryStuff_UNESCAPED);
    printf("    %s ret = %s_alloc_(); %s_init_(ret);\n"
           "    TRACE_IF_ERROR;\n"
           "    _err_ = NULL; STACKDEPTH_DOWN; return ret;\n",
        name, name, name);
    puts(functionExitStuff_UNESCAPED);
    puts("#undef DEFAULT_VALUE\n#undef MYSTACKUSAGE\n}\n");
    printf("#define %s_print(p) %s_print__(p, STR(p))\n", name, name);
    printf("monostatic void %s_print__(%s self, const char* name) {\n    "
           "printf(\"<%s "
           "'%%s' at %%p size %%luB>\\n\",name, self, sizeof(struct %s));\n}\n",
        name, name, name, name);
    puts("");

    genJson_type(type);
    genJsonReader_type(type);
}

///////////////////////////////////////////////////////////////////////////
static void genh_type(ast_type_t* type, int level) {
    if (!type->body || !type->analysed || type->isDeclare) return;

    const char* const name = type->name;
    printf("typedef struct %s* %s;\nstruct %s;\n", name, name, name);
    printf("static %s %s_alloc_(); \n", name, name);
    printf("static %s %s_init_(%s self);\n", name, name, name);
    printf("%s %s_new_(IFDEBUG(const char* callsite_)); \n", name, name);
    printf("\nDECL_json_wrap_(%s)\n//DECL_json_file(%s)\n", name, name);
    printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
           "%s_json_wrap_(x); }\n\n",
        name, name);
    printf("static void %s_json_(const %s self, int nspc);\n", name, name);
}
///////////////////////////////////////////////////////////////////////////
static void genh_enum(ast_type_t* type, int level) {
    if (!type->body || !type->analysed) return;
    const char* const name = type->name;
    puts("typedef enum {");

    foreach (ast_var_t*, var, type->body->locals)
        printf("    %s_%s,\n", name, var->name);
    printf("} %s;\n", name);
    ast_expr_t* ex1 = type->body->stmts->item;
    const char* datType
        = ex1->kind == tk_opAssign ? typeName_expr(ex1->right) : NULL;
    if (datType)
        printf("monostatic %s %s__data[%d];\n", datType, name,
            list_count(type->body->locals));
    printf("monostatic const char* %s__fullnames[] ={\n", name);
    foreach (ast_var_t*, var, type->body->locals)
        printf("    \"%s.%s\",\n", name, var->name);
    puts("};");
    printf("monostatic const char* %s__names[] ={\n", name);
    foreach (ast_var_t*, var, type->body->locals)
        printf("    \".%s\",\n", var->name);
    puts("};");
    // printf("monostatic const char* %s__names[%d];\n", name,
    //     list_count(type->body->locals));

    printf("monostatic void %s__init() {\n", name);
    // foreach (ast_var_t*, var, type->body->locals) {
    //     printf("    %s__names[%s_%s] =  %s__fullnames[%s_%s] + %zu;\n", name,
    //         name, var->name, name, name, var->name, strlen(name));
    //     // puts("};");
    // }
    foreach (ast_expr_t*, stmt, type->body->stmts) {
        if (!stmt || stmt->kind != tk_opAssign) //|| !stmt->var->init)
            // //     //            or not stmt->var->used)
            continue;
        printf("%.*s%s__data[%s_%s] = ", level + STEP, spaces, name, name,
            stmt->left->string);
        emit_expr(stmt->right, 0);
        puts(";");
        if (throws_expr(stmt->right))
            puts("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    puts("}");

    // printf("static %s %s_alloc_(); \n", name, name);
    // printf("%s %s_new_(IFDEBUG(const char* callsite_)); \n", name, name);
    // printf("\nDECL_json_wrap_(%s)\n//DECL_json_file(%s)\n", name, name);
    // printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
    //        "%s_json_wrap_(x); }\n\n",
    //     name, name);
    // printf("static void %s_json_(const %s self, int nspc);\n", name, name);
}
///////////////////////////////////////////////////////////////////////////
static void emit_func(ast_func_t* func, int level) {
    if (!func->body || !func->analysed || func->isDeclare)
        return; // declares, default ctors

    // actual stack usage is higher due to stack protection, frame bookkeeping
    // ...
    size_t stackUsage = calcSizeUsage_func(func);
    printStackUsageDef_func(stackUsage);

    printf(
        "#define DEFAULT_VALUE %s\n", getDefaultValueForType(func->returnSpec));
    if (!func->isExported) printf("static ");
    if (func->returnSpec) {
        ast_typespec_emit(func->returnSpec, level, false);
    } else {
        printf("void");
    }
    printf(" %s(", func->selector);
    foreachn(ast_var_t*, arg, args, func->args) {
        emit_var(arg, level, true);
        printf(args->next ? ", " : "");
    }

    printf("\n#ifdef DEBUG\n"
           "    %c const char* callsite_ "
           "\n#endif\n",
        ((func->args && func->args->item ? ',' : ' ')));

    // TODO: if (flags.throws) printf("const char** _err_");
    puts(") {");
    printf("    IFDEBUG(static const char* sig_ = \"");
    printf("%s%s(", func->isStmt ? "" : "function ", func->name);

    foreachn(ast_var_t*, arg, args, func->args) {
        format_var(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");
    if (func->returnSpec) {
        printf(" as ");
        ast_typespec_format(func->returnSpec, level);
    }
    puts("\");");

    puts(functionEntryStuff_UNESCAPED);

    emit_scope(func->body, level + STEP);

    puts(functionExitStuff_UNESCAPED);
    puts("}\n#undef DEFAULT_VALUE");
    puts("#undef MYSTACKUSAGE");
}

///////////////////////////////////////////////////////////////////////////
static void genh_func(ast_func_t* func, int level) {
    if (!func->body || !func->analysed || func->isDeclare) return;
    if (!func->isExported) printf("static ");
    if (func->returnSpec) {
        ast_typespec_emit(func->returnSpec, level, false);
    } else {
        printf("void");
    }
    printf(" %s(", func->selector);
    foreachn(ast_var_t*, arg, args, func->args) {
        emit_var(arg, level, true);
        printf(args->next ? ", " : "");
    }
    printf("\n#ifdef DEBUG\n    %c const char* callsite_\n#endif\n",
        ((func->args && func->args->item) ? ',' : ' '));
    puts(");\n");
}

///////////////////////////////////////////////////////////////////////////
static void genh_var(ast_var_t* var, int level) {
    // if (! func->body or not func->analysed) return;
    // if (!func->isExported) printf("static ");
    // if (var->typespec) {
    if (!var->init) return;

    ast_typespec_emit(var->typespec, level, false);
    // }

    printf(" %s = ", var->name);
    emit_expr(var->init, 0);

    puts("");
}

// #define MKEMB(T, ...)                                                          \
//     (T[]) {                                                                    \
//         { __VA_ARGS__ }                                                        \
//     }

// ast_func_t* decld = (ast_func_t[]) { { .name = "Oiunko",
//     .selector = "Oinko_uio_uyt",
//     .line = 21,
//     .args = (PtrList[]) { { .item = (ast_var_t[1]) { { .name = "arg1" } } }
//     }, .isDeclare = 1, .isRecursive = 1 } };

// ast_func_t* declc = MKEMB(ast_func_t, .name = "Oiunko", .selector =
// "Oinko_uio_uyt",
//     .line = 21,
//     .args = MKEMB(PtrList, .item = MKEMB(ast_var_t, .name = "arg1", .line =
//     6)));

////////////////////////////////////////////////////
static void emit_test(ast_test_t* test) // TODO: should tests not return BOOL?
{
    if (!test->body) return;
    printf("\nstatic void test_%s() {\n", test->name);
    emit_scope(test->body, STEP);
    puts("}");
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a subscript (that has been resolved to
/// its corresponding `ast_variable_t`). This function does all of the heavy
/// lifting to decide what the subscript actually does, based on the kind of the
/// subscript expression, number of dimensions and the context.
static void emit_expr_tk_subscriptResolved(ast_expr_t* expr, int level) {
    char* name = expr->var->name;
    ast_expr_t* index = expr->left;
    assert(index);
    // index = index->right;
    switch (index->kind) {
    case tk_number: // indexing with a single number, can be a -ve number
        printf("Array_get_%s(%s, %s)", ast_typespec_cname(expr->var->typespec),
            name, index->string);
        break;

    case tk_string:
    case tk_rawString: // indexing with single string or regex
        printf("Dict_get_CString_%s(%s, %s)",
            ast_typespec_cname(expr->var->typespec), name, index->string);
        break;

    case tk_opComma: // higher dims. validation etc. has been done by this
                     // stage.

        // this is for cases like arr[2, 3, 4].
        printf("Tensor%dD_get_%s(%s, {", expr->var->typespec->dims,
            ast_typespec_cname(expr->var->typespec), name);
        emit_expr(index, 0);
        printf("})");

        // TODO: cases like arr[2:3, 4:5, 1:end]
        // basically the idea is to generate getijk/getIJK/getIJk etc.
        // where a caps means range and lowercase means single number.
        // so arr[2:3, 4:5, 1:end] should generate `getIJK`,
        // arr[2:3, 4, 2:end] should generate `getIjK` and so on.
        // Those are then macros in the "runtime" that have for loops
        // for the ranges and nothing special for the single indices.
        // but they should be put into a tmpvar to avoid repeated eval.

        break;

    case tk_opColon:
        // a single range.
        printf(
            "Array_getSlice_%s(%s, ", name_typespec(expr->var->typespec), name);
        emit_expr(index, 0);
        printf(")");
        break;
        // what about mixed cases, e.g. arr[2:3, 5, 3:end]
        // make this portion a recursive function then, or promote
        // all indexes to ranges first and then let opcomma handle it

    case tk_opEQ:
    case tk_opLE:
    case tk_opGE:
    case tk_opGT:
    case tk_opLT:
    case tk_opNE:
    case tk_keyword_and:
    case tk_keyword_or:
    case tk_keyword_not:
        // indexing by a Boolean expression (filter)
        // by default this implies a copy, but certain funcs e.g. print
        // min max sum count etc. can be done in-place without a copy
        // since they are not mutating the array. That requires either
        // the user to call print(arr, filter = arr < 5) instead of
        // print(arr[arr < 5]), or the compiler to transform the second
        // into the first transparently.
        // Probably the tk_functionCall should check if its argument is
        // a tk_subscript with a Boolean index, and then tip the user
        // to call the optimised function instead (or just generate it).
        // For now, and in the absence of more context, this is a copy.
        // Array_copy_filter is implemented as a C macro for loop, as
        // are most other filtering-enabled functions on arrays.
        // TODO: be careful with the "template" style call here xx()()
        // TODO: actually I think arr[arr < 5] etc. should just be
        // promoted
        //    and then the generation will follow the modified AST.
        //    Don't handle this as a special case at the code generation
        //    stage.
        printf("Array_copy_filter_%s(%s, ", name_typespec(expr->var->typespec),
            name);
        emit_expr(index, 0);
        printf(")");
        break;

    default: unreachable("bad kind: %s", tokenkind_e_names[expr->kind]); break;
    }
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a function call (that has been resolved to
/// its corresponding `ast_func_t`). Type constructors call a C function
/// that has `_new` appended to the type name. This function passes a
/// constructed string as the extra argument `callsite_` that is used to
/// generate accurate backtraces.
static void emit_expr_tk_functionCallResolved(ast_expr_t* expr, int level) {
    char* tmp = expr->func->selector;

    ast_expr_t* arg1 = expr->left;
    const char* tmpc = "";
    if (arg1) {
        if (arg1->kind == tk_opComma) arg1 = arg1->left;
        tmpc = collectiontype_e_nativeName(arg1->collectionType);
    }
    printf("%s%s", tmpc, tmp);
    if (*tmp >= 'A' && *tmp <= 'Z' && !strchr(tmp, '_')) printf("_new_");
    printf("(");

    if (expr->left) emit_expr(expr->left, 0);

    if (!expr->func->isDeclare) {
        printf("\n#ifdef DEBUG\n"
               "      %c \"./\" THISFILE \":%d:%d:\\e[0m ",
            expr->left ? ',' : ' ', expr->line, expr->col);
        format_expr(expr, 0, false, true);
        printf("\"\n"
               "#endif\n        ");
    }
    printf(")");
}

char* strchrnul(char* str, char ch) {
    while (*str && *str != ch) str++;
    return str;
}
static void lineupmultilinestring_expr(ast_expr_t* expr, int indent) {
    return;
    char* pos = expr->string;
    while (*(pos = strpbrk(pos, "\n"))) {
        int del = strspn(pos, " ") - indent;
        if (del <= 0) continue;
    }
}

static void printmultilstr(char* pos) {
    do {
        int p = strcspn(pos, "\n");
        printf("\"%.*s", p, pos);
        pos += p + 1;
        printf(*pos ? "\\n\"" : "\"");
    } while (*pos);
}

static void emit_expr_tk_string(ast_expr_t* expr, int level) {
    lineupmultilinestring_expr(expr, level + STEP);
    if (!expr->vars) {
        printmultilstr(expr->string + 1);
    } else {
        char* pos = expr->string;
        // putc('"', stdout);
        char* last = pos;
        ast_var_t* v;
        PtrList* p = expr->vars;
        ast_expr_t* e = p->item;
        printf("strinterp_h(64, ");
        while (*pos) {
            while (*pos && *pos != '$') pos++;
            *pos++ = 0;
            // int l = pos - last - 1;
            printf("%s", last);
            // printmultilstr(last + 1);
            pos[-1] = '$';
            last = pos;
            while (*pos && isalnum(*pos) || *pos == '.') pos++;
            // l = pos - last;
            // eprintf("%.*s\n", l, last);
            if (e) {
                while (e->kind == tk_period) e = e->right;
                assert(e->kind == tk_identifierResolved);
                assert(e->var);
                printf(
                    "%s", typetype_e_format(e->var->typespec->typeType, false));
                last = pos;
                e = ((p = p->next)) ? p->item : NULL;
            }
        }
        printf("\"");
        foreach (ast_expr_t*, e, expr->vars) {
            printf(", ");
            emit_expr(e, 0);
        }
        printf(")");
    }
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a (literal) numeric expression.
/// Complex numbers follow C99 literal syntax, e.g. 1i generates
/// `_Complex_I * 1`.
static void emit_expr_tk_number(ast_expr_t* expr, int level) {
    size_t ls = CString_length(expr->string);
    if (expr->string[ls - 1] == 'i') {
        printf("_Complex_I*");
        expr->string[ls - 1] = 0;
    }
    printf("%s", expr->string);
}

static void emit_expr_tk_check(ast_expr_t* expr, int level) {
    // TODO: need llhs and lrhs in case all 3 in 3way are exprs
    // e.g. check a+b < c+d < e+f
    ast_expr_t* checkExpr = expr->right; // now use checkExpr below
    ast_expr_t* lhsExpr = checkExpr->left;
    ast_expr_t* rhsExpr = checkExpr->right;
    printf("{\n");
    if (!checkExpr->unary) {
        printf("%.*s%s _lhs = ", level, spaces, typeName_expr(lhsExpr));
        emit_expr(lhsExpr, 0);
        printf(";\n");
    }
    printf("%.*s%s _rhs = ", level, spaces, typeName_expr(rhsExpr));
    emit_expr(rhsExpr, 0);
    printf(";\n");
    printf("%.*sif (!(", level, spaces);
    // ----- use lhs rhs cached values instead of the expression
    emit_expr(checkExpr, 0);
    // how are you doing to deal with x < y < z? Repeat all the logic of
    // emit_expr?
    // ----------------------------------------------------------------
    // if (checkExpr->unary) {
    //     printf("_rhs");
    // } else {
    //     printf("_lhs %s _rhs");
    // }
    // -------------
    printf(")) {\n");
    printf("%.*sprintf(\"\\n\\n\e[31mruntime error:\e[0m check "
           "failed at \e[36m./%%s:%d:%d:\e[0m\\n    %%s\\n\\n\",\n     "
           "       "
           "   THISFILE, \"",
        level + STEP, spaces, expr->line, expr->col + 6);
    format_expr(checkExpr, 0, true, true);
    printf("\");\n");
    printf("#ifdef DEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

    genPrintVars_expr(checkExpr, level + STEP);
    // the `printed` flag on all vars of the expr will be set
    // (genPrintVars uses this to avoid printing the same var
    // twice). This should be unset after every toplevel call to
    // genPrintVars.
    if (!checkExpr->unary) {
        // dont print literals or arrays
        if (lhsExpr->collectionType == cty_none //
            && lhsExpr->kind != tk_string //
            && lhsExpr->kind != tk_number //
            && lhsExpr->kind != tk_rawString //
            && lhsExpr->kind != tk_opLE //
            && lhsExpr->kind != tk_opLT) {

            if (lhsExpr->kind != tk_identifierResolved
                || !lhsExpr->var->visited) {
                printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
                printf("%s", typetype_e_format(lhsExpr->typeType, true));
                printf("%s", "\\n\", \"");
                format_expr(lhsExpr, 0, true, true);
                printf("%s", "\", _lhs);\n");
            }
            // checks can't have tk_varAssign inside them
            // if ()
            //     lhsExpr->var->visited = true;
        }
    }
    if (rhsExpr->collectionType == cty_none //
        && rhsExpr->kind != tk_string //
        && rhsExpr->kind != tk_number //
        && rhsExpr->kind != tk_rawString) {
        if (rhsExpr->kind != tk_identifierResolved || !rhsExpr->var->visited) {
            printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
            printf("%s", typetype_e_format(rhsExpr->typeType, true));
            printf("%s", "\\n\", \"");
            format_expr(rhsExpr, 0, true, true);
            printf("%s", "\", _rhs);\n");
        }
    }

    unmarkVisited_expr(checkExpr);

    printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
    printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
    printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
}

///////////////////////////////////////////////////////////////////////////
/// This should be a standard dispatcher that does nothing except the
/// actual dispatching (via a function pointer table, not a switch).
static void emit_expr(ast_expr_t* expr, int level) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("%.*s", level, spaces);
    switch (expr->kind) {
    case tk_number: emit_expr_tk_number(expr, level); break;

    case tk_keyword_no: printf("no"); break;
    case tk_keyword_yes: printf("yes"); break;
    case tk_keyword_nil: printf("nil"); break;

    case tk_multiDotNumber:
    case tk_identifier: printf("%s", expr->string); break;

    case tk_string: // TODO: parse vars inside, escape stuff, etc.
        emit_expr_tk_string(expr, level);
        // printf(escStrings ? "\\%s\\\"" : "%s\"", expr->string);
        break;

    case tk_identifierResolved: printf("%s", expr->var->name); break;

    case tk_rawString: // 'raw strings' or 'regexes'
        printf("\"%s\"", expr->string + 1);
        break;

    case tk_regexp: // inline C code?
        printf("%s", expr->string + 1);
        break;

    case tk_lineComment: // TODO: skip  comments in generated code
        printf("// %s", expr->string);
        break;

    case tk_functionCall:
        unreachable("unresolved call to '%s'\n", expr->string);
        break;

    case tk_functionCallResolved:
        emit_expr_tk_functionCallResolved(expr, level);
        break;

    case tk_subscript:
        unreachable("unresolved subscript on '%s'\n", expr->string);
        break;

    case tk_subscriptResolved:
        emit_expr_tk_subscriptResolved(expr, level);
        break;

    case tk_opAssign:
    case tk_opPlusEq:
    case tk_opMinusEq:
    case tk_opTimesEq:
    case tk_opSlashEq:
    case tk_opPowerEq:
    case tk_opModEq:

        switch (expr->left->kind) {
        case tk_subscriptResolved:
            switch (expr->left->left->kind) {
            case tk_number:
            case tk_string:
            case tk_rawString:
                // TODO: typename_expr should return Array_Scalar or
                // Tensor2D_Scalar or Dict_String_Scalar etc.
                printf("%s_set(%s, %s,%s, ", typeName_expr(expr->left),
                    expr->left->var->name, expr->left->left->string,
                    tokenkind_e_srepr[expr->kind]);
                emit_expr(expr->right, 0);
                printf(")");
                break;

            case tk_opColon:
                printf("%s_setSlice(%s, ", typeName_expr(expr->left),
                    expr->left->var->name);
                emit_expr(expr->left->left, 0);
                printf(",%s, ", tokenkind_e_srepr[expr->kind]);
                emit_expr(expr->right, 0);
                printf(")");
                break;

            case tk_opEQ:
            case tk_opGE:
            case tk_opNE:
            case tk_opGT:
            case tk_opLE:
            case tk_opLT:
            case tk_keyword_and:
            case tk_keyword_or:
            case tk_keyword_not:
                printf("%s_setFiltered(%s, ", typeName_expr(expr->left),
                    expr->left->var->name);
                emit_expr(expr->left->left, 0);
                printf(",%s, ", tokenkind_e_srepr[expr->kind]);
                emit_expr(expr->right, 0);
                printf(")");
                break;

            case tk_opComma:
                // figure out the type of each element
                // there should be a RangeND just like TensorND and
                // SliceND then you can just pass that to _setSlice
                break;
            case tk_identifierResolved:
                // lookup the var type. note that it need not be Number,
                // string, range etc. it could be an arbitrary object in
                // case you are indexing a Dict with keys of that type.
                break;
            case tk_subscriptResolved:
                // arr[arr2[4]] etc.
                break;
            case tk_functionCallResolved:
                // arr[func(x)]
                break;
            default:
                unreachable("%s\n", tokenkind_e_names[expr->left->kind]);
                assert(0);
            }
            break;
        case tk_identifierResolved:
        case tk_period:
            emit_expr(expr->left, 0);
            printf("%s", tokenkind_e_srepr[expr->kind]);
            emit_expr(expr->right, 0);
            break;
        case tk_identifier:
            unreachable("unresolved var %s", expr->left->string);
            break;
        case tk_argumentLabel:
            // assert(inFuncArgs);
            emit_expr(expr->right, 0);
            // function call arg label, do not generate ->left
            break;
        case tk_string: break;
        default:
            // error: not a valid lvalue
            // TODO: you should at some point e,g, during resolution
            // check for assignments to invalid lvalues and raise an
            // error
            unreachable(
                "found token kind %s\n", tokenkind_e_names[expr->left->kind]);
        }
        // if (! inFuncArgs) {
        //     emit_expr(self->left, 0,
        //     escStrings); printf("%s", tokenkind_e_repr(tk_opAssign,
        //     spacing));
        // }
        // emit_expr(self->right, 0,     escStrings);
        // check various types of lhs  here, eg arr[9:87] = 0,
        // map["uuyt"]="hello" etc.
        break;

    case tk_arrayOpen:
        // TODO: send parent ast_expr_t* as an arg to this function. Then
        // here do various things based on whether parent is a =,
        // funcCall, etc.
        if (!expr->right) {
            printf("Array_init(%s)()", "double");
        } else {
            printf("Array_make(((%s[]) {", "double"); // FIXME
            // TODO: MKARR should be different based on the
            // CollectionType of the var or arg in question, eg stack
            // cArray, heap allocated Array, etc.
            emit_expr(expr->right, 0);
            printf("})");
            printf(", %d)", countCommaList_expr(expr->right));
        }
        break;

    case tk_braceOpen: {
        const char* Ktype = "CString";
        const char* Vtype = "Real64";
        if (!expr->right)
            printf("Dict_init(%s,%s)()", Ktype, Vtype); // FIXME
        else {
            printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
                countCommaList_expr(expr->right), Ktype); // FIXME

            ast_expr_t* p = expr->right;
            while (p && p->kind == tk_opComma) {
                emit_expr(p->left->left, 0);
                printf(", ");
                p = p->right;
            };
            emit_expr(p->left, 0);
            printf("}, (%s[]){", Vtype);
            p = expr->right;
            while (p && p->kind == tk_opComma) {
                emit_expr(p->left->right, 0);
                printf(", ");
                p = p->right;
            };
            emit_expr(p->right, 0);
            printf("})");
        }
    } break;

    case tk_opColon: // convert 3:4:5 to range(...)
                     // must do bounds check first!
        printf(
            "%s(", expr->left->kind != tk_opColon ? "range_to" : "range_to_by");
        if (expr->left->kind == tk_opColon) {
            expr->left->kind = tk_opComma;
            emit_expr(expr->left, 0);
            expr->left->kind = tk_opColon;
        } else
            emit_expr(expr->left, 0);
        printf(", ");
        emit_expr(expr->right, 0);
        printf(")");
        break;

    case tk_varAssign: // basically a tk_opAssign corresponding to a local
                       // var
        // var x as XYZ = abc... -> becomes an ast_var_t and an
        // ast_expr_t (to keep location). Send it to ast_var_t::gen.
        if (expr->var->init != NULL && expr->var->used) {
            printf("%s = ", expr->var->name);
            emit_expr(expr->var->init, 0);
        } else {
            printf("/* %s %s at line %d */", expr->var->name,
                expr->var->used ? "null" : "unused", expr->line);
        }
        break;

    case tk_keyword_else:
        puts("else {");
        if (expr->body) emit_scope(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tk_keyword_elif:
        puts("else if (");
        emit_expr(expr->left, 0);
        puts(") {");
        if (expr->body) emit_scope(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tk_keyword_match: {
        // char* typeName = typeName_expr(expr->left);
        // if (!typeName)
        //     unreachable(
        //         "unresolved type during emit at %d:%d", expr->line,
        //         expr->col);
        // if (expr->left->typeType == ty_object)
        //     typeName = typeName_expr(expr->left);
        printf("{%s __match_cond = ", typeName_expr(expr->left));
        emit_expr(expr->left, 0);
        if (expr->left->typeType > ty_int8
            || (expr->left->typeType == ty_object
                && getObjectType_expr(expr->left)->isEnum))
            puts("; switch (__match_cond) {");
        else
            puts("; { if (0) {}"); // the case will add 'else if's
        // puts(") {");
        if (expr->body) emit_scope(expr->body, level);
        printf("%.*s}}", level, spaces);
        break;
    }

        /*
        This is how you walk a ast_expr_t that is a tk_opComma (left to right):
            process(cond->left);
            while (cond->right->kind == tk_opComma)
                cond = cond->right, process(cond->left);
            process(cond->right);
        */

        // void pro(ast_expr_t * c) { }
        // TODO: generally all comma exprs should be handled like this
        // iteratively. What if you have a large array with lots of items?
        // recursion will blow the stack
    case tk_keyword_case: {
        // TODO: maybe make this a macro
        ast_expr_t* cond = expr->left;
        if (cond->kind == tk_opComma) {
            if (cond->typeType > ty_int8
                || (cond->typeType == ty_object && getEnumType_expr(cond))) {
                printf("case "); // match has handled the cond with a 'switch'
                emit_expr(cond->left, 0);
                printf(": ");
                while (cond->right->kind == tk_opComma) {
                    cond = cond->right;
                    printf("case ");
                    emit_expr(cond->left, 0);
                    printf(": ");
                }
                printf("case ");
                emit_expr(cond->right, 0);
                puts(": {");
            } else if (cond->typeType == ty_string) {
                printf("else if (!strcmp(__match_cond, ");
                emit_expr(cond->left, 0);
                printf(")");
                while (cond->right->kind == tk_opComma) {
                    cond = cond->right;
                    printf(" || !strcmp(__match_cond, ");
                    emit_expr(cond->left, 0);
                    printf(")");
                }
                printf(" || !strcmp(__match_cond, ");
                emit_expr(cond->right, 0);
                puts(")) do {");
            } else {
                printf("else if (__match_cond == ");
                emit_expr(cond->left, 0);
                while (cond->right->kind == tk_opComma) {
                    cond = cond->right;
                    printf(" || __match_cond == (");
                    emit_expr(cond->left, 0);
                }
                emit_expr(cond->right, 0);
                puts(")) do {");
            };

        } else {
            if (cond->typeType > ty_int8
                || (cond->typeType == ty_object && getEnumType_expr(cond))) {
                printf("case "); // match has handled the cond with a 'switch'
                emit_expr(cond, 0);
                puts(": {");
            } else if (cond->typeType == ty_string) {
                printf("else if (!strcmp(__match_cond, ");
                emit_expr(cond, 0);
                puts(")) do {");
            } else {
                printf("else if (__match_cond == (");
                emit_expr(cond, 0);
                puts(")) do {");
            };
        };
        if (expr->body) emit_scope(expr->body, level);
        printf("%.*s}", level, spaces);
        if (cond->typeType > ty_int8
            || (cond->typeType == ty_object && getEnumType_expr(cond)))
            printf(" break");
        else
            printf(" while(0)");
        break;
    }
    case tk_keyword_for:
    case tk_keyword_if:
        //    case tk_keyword_elif:
        //    case tk_keyword_else:
    case tk_keyword_while:
        if (expr->kind == tk_keyword_for)
            printf("FOR(");
        else
            printf("%s (", tokenkind_e_srepr[expr->kind]);
        if (expr->kind == tk_keyword_for) expr->left->kind = tk_opComma;
        if (expr->left) emit_expr(expr->left, 0);
        if (expr->kind == tk_keyword_for) expr->left->kind = tk_opAssign;
        puts(") {");
        if (expr->body) emit_scope(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tk_opPower:
        printf("pow(");
        emit_expr(expr->left, 0);
        printf(",");
        emit_expr(expr->right, 0);
        printf(")");
        break;

    case tk_keyword_return:
        printf("{_err_ = NULL; STACKDEPTH_DOWN; return ");
        if (expr->right) emit_expr(expr->right, 0);
        printf(";}\n");
        break;

    case tk_keyword_check: emit_expr_tk_check(expr, 0); break;

    case tk_period:
        emit_expr(expr->left, 0);
        if (expr->left->typeType == ty_object
            && getObjectType_expr(expr->left)->isEnum)
            printf("_");
        else
            printf("->"); // may be . if right is embedded and not a
                          // reference
        emit_expr(expr->right, 0);
        break;

    case tk_keyword_notin: printf("!"); fallthrough;
    case tk_keyword_in:
        // the RHS should be dims==1 or another kind of collection, you should
        // have checked it in the analysis phase.
        switch (expr->right->kind) {
        case tk_arrayOpen:
            if (expr->right->right->kind == tk_opColon)
                goto inRangeOp; // x in [a:b]
            // now its a literal array. that makes it easy, you can either call
            // isin() or the macro ISIN() if you have relatively few items in
            // the array.

            {
                int c = countCommaList_expr(expr->right->right);
                // if (c <= 64) {
                // TODO: ISIN/isin must be specialized for types other than
                // int. in particular strings cannot be used yet
                printf("%s(%d, ", c <= 64 ? "ISIN" : "isin", c);
                emit_expr(expr->left, 0);
                printf(", ");
                emit_expr(expr->right->right, 0);
                printf(")");
                // }
            }

            break;
        case tk_subscript:
            // maybe slice or something
            break;
        case tk_opColon: // x in a:b
        inRangeOp:
            break;
        default:
            unreachable("inside in operator: rhs is %s",
                tokenkind_e_repr[expr->right->kind]);
            // for anything else, figure it out.
        }
        break;

    case tk_opEQ:
    case tk_opNE:
    case tk_opGE:
    case tk_opLE:
    case tk_opGT:
    case tk_opLT:
        if ((expr->kind == tk_opLE || expr->kind == tk_opLT)
            && (expr->left->kind == tk_opLE | expr->left->kind == tk_opLT)) {
            printf("%s_cmp3way_%s_%s(", typeName_expr(expr->left->right),
                tokenkind_e_ascrepr(expr->kind, false),
                tokenkind_e_ascrepr(expr->left->kind, false));
            emit_expr(expr->left->left, 0);
            printf(", ");
            emit_expr(expr->left->right, 0);
            printf(", ");
            emit_expr(expr->right, 0);
            printf(")");
            break;
        } else if (expr->right->typeType == ty_string) {
            printf("CString_cmp(%s, ", tokenkind_e_srepr[expr->kind]);
            emit_expr(expr->left, 0);
            printf(", ");
            emit_expr(expr->right, 0);
            printf(")");
            break;
        }
        fallthrough;
    default:
        if (!expr->prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr->left && expr->left->prec && expr->left->prec < expr->prec;
        bool rightBr = expr->right && expr->right->prec
            && expr->right->kind != tk_keyword_return
            && expr->right->prec < expr->prec;
        // found in 'or return'

        char lpo = '(';
        char lpc = ')';
        if (leftBr) putc(lpo, stdout);
        if (expr->left) emit_expr(expr->left, 0);
        if (leftBr) putc(lpc, stdout);

        if (expr->kind == tk_arrayOpen)
            putc('{', stdout);
        else
            printf("%s", tokenkind_e_srepr[expr->kind]);

        char rpo = '(';
        char rpc = ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right) emit_expr(expr->right, 0);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tk_arrayOpen) putc('}', stdout);
    }
}

// WARNING: DO NOT USE THESE STRINGS WITH PRINTF(...) USE PUTS(...).
// static const char* coverageFunc[] = { //
//     "static void coverage_report() { /* unused */ }",
//     "static void coverage_report() {\n"
//     "    int count=0,l=NUMLINES;\n"
//     "    while(--l>0) count+=!!_cov_[l];\n"
//     "    printf(\"coverage: %d/%d lines = %.2f%%\\n\","
//     "        count, NUMLINES, count*100.0/NUMLINES);\n"
//     "}"
// };
// WARNING: DO NOT USE THESE STRINGS WITH PRINTF(...) USE PUTS(...).
// static const char* lineProfileFunc[] = {
//     //
//     "static void lineprofile_report() { /* unused */ }\n"
//     "static void lineprofile_begin() { /* unused */ }\n",
//     "static void lineprofile_report() {\n"
//     // "    printf(\"profiler: %llu cycles\\n\","
//     // "        _lprof_[NUMLINES-1]-_lprof_[0]);\n"
//     "    FILE* fd = fopen(\".\" THISFILE \"r\", \"w\");\n"
//     // "    for (int i=1; i<NUMLINES; i++)\n"
//     // "        if (0== _lprof_[i]) _lprof_[i] = _lprof_[i-1];\n"
//     // "    for (int i=NUMLINES-1; i>0; i--) _lprof_[i] -=
//     _lprof_[i-1];\n" "    ticks sum=0; for (int i=0; i<NUMLINES; i++)
//     sum += _lprof_[i];\n" "    for (int i=0; i<NUMLINES; i++) {\n" "
//     double pct = _lprof_[i] * 100.0 / sum;\n" "        if (pct>1.0)"
//     "            fprintf(fd,\" %8.1f%% |\\n\", pct);\n"
//     "        else if (pct == 0.0)"
//     "            fprintf(fd,\"           |\\n\");\n"
//     "        else"
//     "            fprintf(fd,\"         ~ |\\n\");\n"
//     "    }\n"
//     "    fclose(fd);\n"
//     "    system(\"past_e -d ' ' .\" THISFILE \"r \" THISFILE \" > \" "
//     "THISFILE "
//     "\"r\" );"
//     "}\n"
//     "static void lineprofile_begin() {_lprof_last__=getticks();}\n"
// };
///////////////////////////////////////////////////////////////////////////
// TODO: why do you need to pass level here?

void genTypeInfoDecls_type(ast_type_t* type);
void genTypeInfoDefs_type(ast_type_t* type);
void genNameAccessors_type(ast_type_t* type);
static void emit_module(ast_module_t* module) {
    // puts("");
    foreach (ast_import_t*, import, module->imports)
        emit_import(import, 0);

    puts("");

    foreach (ast_var_t*, var, module->scope->locals)
        if (var->used) genh_var(var, 0);

    foreach (ast_type_t*, type, module->enums) {
        if (type->body && type->analysed) {
            genh_enum(type, 0);
            // genTypeInfoDecls_type(type);
        }
    }

    foreach (ast_type_t*, type, module->types) {
        if (type->body && type->analysed) {
            genh_type(type, 0);
            genTypeInfoDecls_type(type);
        }
    }
    foreach (ast_func_t*, func, module->funcs) {
        if (func->body && func->analysed) { genh_func(func, 0); }
    }

    foreach (ast_type_t*, type, module->types) {
        if (type->body && type->analysed) {
            // foreach (ast_expr_t*, expr, type->body->stmts)
            //     prepareInterp_expr(expr, type->body);
            // ^ MOVE THIS INTO emit_type
            emit_type(type, 0);
            genTypeInfoDefs_type(type);
            genNameAccessors_type(type);
        }
    }
    foreach (ast_func_t*, func, module->funcs) {
        if (func->body && func->analysed) {
            // foreach (ast_expr_t*, expr, func->body->stmts) {
            //     prepareInterp_expr(parser,expr, func->body);
            // }
            // ^ MOVE THIS INTO emit_func
            emit_func(func, 0);
        }
    }
    foreach (ast_import_t*, import, module->imports)
        undefc_import(import);

    // puts(coverageFunc[genCoverage]);
    // puts(lineProfileFunc[genLineProfile]);
}

static void genTests_module(ast_module_t* module) {
    emit_module(module);
    foreach (ast_test_t*, test, module->tests)
        emit_test(test);
    // generate a func that main will call
    printf("\nvoid tests_run_%s() {\n", module->name);
    foreach (ast_test_t*, test, module->tests)
        printf("    test_%s();\n", test->name);
    puts("}");
}

// Generates a couple of functions that allow setting an integral member
// of a type at runtime by name, or getting a pointer to a member by
// name.
void genNameAccessors_type(ast_type_t* type) {
    // TODO: instead of a linear search over all members this should
    // generate a switch for checking using a prefix tree -> see
    // genrec.c
    if (!type->analysed || type->isDeclare) return;
    // genMemberRecognizer_type( type, "Int64 value",  )

    printf("static void* %s__memberNamed(%s self, const char* name) {\n",
        type->name, type->name);

    // TODO: skip bitfield members in this loop or it wont compile
    foreach (ast_var_t*, var, type->body->locals) /*if (var->used) */ //
        printf("    if (CString_equals(name, \"%s\")) return "
               "&(self->%s);\n",
            var->name, var->name);
    printf("    return NULL;\n}\n");

    // this func sets bools or ints that may be part of bitfields
    printf("static void %s__setMemberNamed(%s self, const char* name, "
           "Int64 "
           "value) {\n",
        type->name, type->name);
    foreach (ast_var_t*, var, type->body->locals) // if (var->used) //
        if (var->typespec->typeType >= ty_bool
            && var->typespec->typeType <= ty_real64)
            printf("    if (CString_equals(name, \"%s\"))  {self->%s = "
                   "*(%s*) "
                   "&value;return;}\n",
                var->name, var->name, ast_typespec_cname(var->typespec));
    printf("}\n");
}

// Generates some per-type functions that write out meta info of the
// type to be used for reflection, serialization, etc.
void genTypeInfoDecls_type(ast_type_t* type) {
    if (!type->analysed || type->isDeclare) return;

    printf("static const char* const %s__memberNames[] = {\n    ", type->name);
    if (type->body) //
        foreachn(ast_var_t*, var, varn, type->body->locals) {
            if (!var || !var->used) continue;
            printf("\"%s\", ", var->name);
            // emit_var(var, level + STEP, false);
        }
    printf("};\n");
}

void genTypeInfoDefs_type(ast_type_t* type) {
    // printf("static const char* const %s__memberNames[] = {\n",
    // type->name); foreachn(ast_var_t*, var, varn, type->body->locals)
    // {
    //     if (! var) continue;
    //     printf("\"%s\",\n", var->name);
    //     // emit_var(var, level + STEP, false);
    //     printf("}; \\\n");
    // }
}
