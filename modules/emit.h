#define genLineNumbers 0
#define genCoverage 1
#define genLineProfile 1

///////////////////////////////////////////////////////////////////////////
static void ASTImport_emit(ASTImport* import, int level) {
    CString_tr_ip(import->importFile, '.', '_', 0);
    printf("\n#include \"%s.h\"\n", import->importFile);
    if (import->hasAlias)
        printf("#define %s %s\n", import->importFile + import->aliasOffset,
            import->importFile);
    CString_tr_ip(import->importFile, '_', '.', 0);
}

///////////////////////////////////////////////////////////////////////////
static void ASTImport_undefc(ASTImport* import) {
    if (import->hasAlias)
        printf("#undef %s\n", import->importFile + import->aliasOffset);
}

///////////////////////////////////////////////////////////////////////////
static void ASTTypeSpec_emit(ASTTypeSpec* typeSpec, int level, bool isconst) {
    if (isconst) printf("const ");
    // TODO: actually this depends on the collectionType. In general
    // Array is the default, but in other cases it may be SArray, Array64,
    // whatever
    if (typeSpec->dims) {
        if (typeSpec->dims > 1)
            // TODO: this should be TensorND, without type params?
            // well actually there isn't a TensorND, since its not always
            // double thats in a tensor but can be Complex, Range,
            // Reciprocal, Rational, whatever
            // -- sure, but double (and float) should be enough since
            // the other types are rarely needed in a tensor form
            printf("SArray%dD(", typeSpec->dims);
        else
            printf("SArray(");
    }

    switch (typeSpec->typeType) {
    case TYObject:
        // objects are always T* const, if meant to be r/o they are
        // const T* const. Later we may have a byval flag to embed structs
        // or pass around by value.
        // leaving it as is for now
        printf("%s", typeSpec->type->name);
        break;
    case TYUnresolved:
        unreachable("unresolved: '%s' at %d:%d", typeSpec->name, typeSpec->line,
            typeSpec->col);
        printf("%s", *typeSpec->name ? typeSpec->name : "Error_Type");
        break;
    default:
        printf("%s", TypeType_name(typeSpec->typeType));
        break;
    }

    //     if (isconst ) printf(" const"); // only if a ptr type
    if (typeSpec->dims /*or typeSpec->typeType == TYObject*/) printf("%s", ")");
    //        if (status == TSDimensionedNumber) {
    //            genc(units, level);
    //        }
}

static void ASTExpr_emit(ASTExpr* expr, int level);

///////////////////////////////////////////////////////////////////////////
static void ASTVar_emit(ASTVar* var, int level, bool isconst) {
    // for C the variables go at the top of the block, without init
    printf("%.*s", level, spaces);
    if (var->typeSpec) ASTTypeSpec_emit(var->typeSpec, level + STEP, isconst);
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
static void ASTExpr_unmarkVisited(ASTExpr* expr) {
    switch (expr->kind) {
    case tkIdentifierResolved:
    case tkVarAssign:
        expr->var->visited = false;
        break;
    case tkFunctionCallResolved:
    case tkFunctionCall: // shouldnt happen
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_while:
        ASTExpr_unmarkVisited(expr->left);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary) ASTExpr_unmarkVisited(expr->left);
            ASTExpr_unmarkVisited(expr->right);
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
static void ASTExpr_genPrintVars(ASTExpr* expr, int level) {
    assert(expr);
    // what about func args?
    switch (expr->kind) {
    case tkIdentifierResolved:
    case tkVarAssign:
        if (expr->var->visited) break;
        printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level, spaces,
            expr->var->name, TypeType_format(expr->typeType, true),
            expr->var->name);
        expr->var->visited = true;
        break;

    case tkPeriod:
        //        {
        //            ASTExpr* e = expr->right;
        //            while (e->kind==tkPeriod) e=e->right;
        ////            if (e->var->visited) break;
        //            printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level,
        //            spaces,
        //                   expr->var->name, TypeType_format(e->typeType,
        //                   true), expr->var->name);
        //        }
        break;

    case tkFunctionCallResolved:
    case tkFunctionCall: // shouldnt happen
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_else:
    case tkKeyword_for:
    case tkKeyword_while:
        ASTExpr_genPrintVars(expr->left, level);
        break;

    default:
        if (expr->prec) {
            if (!expr->unary) ASTExpr_genPrintVars(expr->left, level);
            ASTExpr_genPrintVars(expr->right, level);
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// Promotion scan & promotion happens AFTER resolving functions!
static ASTExpr* ASTExpr_findPromotionCandidate(ASTExpr* expr) {
    assert(expr);
    ASTExpr* ret;

    // what about func args?
    switch (expr->kind) {
    case tkFunctionCallResolved:
        // promote innermost first, so check args
        if (expr->left && (ret = ASTExpr_findPromotionCandidate(expr->left)))
            return ret;
        else if (mustPromote(expr->func->selector))
            return expr;
        break;

    case tkSubscriptResolved:
        // TODO: here see if the subscript itself needs to be promoted up
        return ASTExpr_findPromotionCandidate(expr->left);

    case tkSubscript:
        return ASTExpr_findPromotionCandidate(expr->left);

    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_while:
        if (expr->left) return ASTExpr_findPromotionCandidate(expr->left);
        // body will be handled by parent scope

    case tkVarAssign:
        if ((ret = ASTExpr_findPromotionCandidate(expr->var->init))) return ret;
        break;

    case tkFunctionCall: // unresolved
        // assert(0);
        unreachable("unresolved call %s\n", expr->string);
        if ((ret = ASTExpr_findPromotionCandidate(expr->left))) return ret;
        break;

    default:
        if (expr->prec) {
            if (expr->right
                && (ret = ASTExpr_findPromotionCandidate(expr->right)))
                return ret;
            if (!expr->unary)
                if ((ret = ASTExpr_findPromotionCandidate(expr->left)))
                    return ret;
        }
    }
    return NULL;
}

static char* newTmpVarName(int num, char c) {
    char buf[8];
    int l = snprintf(buf, 8, "_%c%d", c, num);
    return pstrndup(buf, l);
}

///////////////////////////////////////////////////////////////////////////
static bool isCtrlExpr(ASTExpr* expr) {
    return expr->kind == tkKeyword_if //
        || expr->kind == tkKeyword_for //
        || expr->kind == tkKeyword_while //
        || expr->kind == tkKeyword_else;
}

static bool isLiteralExpr(ASTExpr* expr) { return false; }
static bool isComparatorExpr(ASTExpr* expr) { return false; }

///////////////////////////////////////////////////////////////////////////
static void ASTScope_lowerElementalOps(ASTScope* scope) {
    foreach (ASTExpr*, stmt, scope->stmts) {

        if (isCtrlExpr(stmt) && stmt->body)
            ASTScope_lowerElementalOps(stmt->body);

        if (!stmt->elemental) continue;

        // wrap it in an empty block (or use if true)
        ASTExpr* ifblk = NEW(ASTExpr);
        ifblk->kind = tkKeyword_if;
        ifblk->left = NEW(ASTExpr);
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
        // a->start+i #define vec_1 *vec_p1 // these could be ASTVars with
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
static void ASTScope_promoteCandidates(ASTScope* scope) {
    int tmpCount = 0;
    ASTExpr* pc = NULL;
    List(ASTExpr)* prev = NULL;
    foreachn(ASTExpr*, stmt, stmts, scope->stmts) {
        // TODO:
        // if (! stmt->promote) {prev=stmts;continue;}

        if (isCtrlExpr(stmt) && stmt->body)
            ASTScope_promoteCandidates(stmt->body);

    startloop:

        if (!(pc = ASTExpr_findPromotionCandidate(stmt))) { // most likely
            prev = stmts;
            continue;
        }
        if (pc == stmt) {
            // possible, less likely: stmt already at toplevel.
            // TODO: in this case, you still have to add the extra arg.
            prev = stmts;
            continue;
        }

        ASTExpr* pcClone = NEW(ASTExpr);
        *pcClone = *pc;

        // 1. add a temp var to the scope
        ASTVar* tmpvar = NEW(ASTVar);
        tmpvar->name = newTmpVarName(++tmpCount, 'p');
        tmpvar->typeSpec = NEW(ASTTypeSpec);
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
            ASTExpr* com = NEW(ASTExpr);
            // TODO: really should have an astexpr ctor
            com->prec = TokenKind_getPrecedence(tkOpComma);
            com->kind = tkOpComma;
            com->left = pcClone->left;
            com->right = pc;
            pcClone->left = com;
        } else {
            ASTExpr* argn = pcClone->left;
            while (argn->kind == tkOpComma && argn->right->kind == tkOpComma)
                argn = argn->right;
            ASTExpr* com = NEW(ASTExpr);
            // TODO: really should have an astexpr ctor
            com->prec = TokenKind_getPrecedence(tkOpComma);
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
        } // List(ASTExpr)* insertionPos = prev ? prev->next : self->stmts;
          //  insertionPos
        //  = insertionPos;
        goto startloop; // it will continue there if no more promotions are
                        // needed

        prev = stmts;
    }
}

///////////////////////////////////////////////////////////////////////////
static void ASTScope_emit(ASTScope* scope, int level) {
    foreach (ASTVar*, local, scope->locals)
        if (local->used) {
            ASTVar_emit(local, level, false);
            puts(";");
        } // these will be declared at top and defined within the expr list
    foreach (ASTExpr*, stmt, scope->stmts) {
        if (stmt->kind == tkLineComment) continue;

        if (genLineNumbers) printf("#line %d\n", stmt->line);
        // if (genCoverage) printf("    _cov_[%d]++;\n", stmt->line - 1);
        // if (genLineProfile) {
        //     printf("    _lprof_tmp_ = getticks();\n");
        //     printf("    _lprof_[%d] += (_lprof_tmp_-_lprof_last_)/100;\n",
        //         stmt->line - 1);
        //     printf("    _lprof_last_ = _lprof_tmp_;\n");
        // }

        // You need to know if the ASTExpr_emit will in fact generate something.
        // This is true in general unless it is an unused var init.
        if (stmt->kind != tkVarAssign || stmt->var->used) {
            if (genCoverage || genLineProfile) //
                printf("    /************/ ");
            if (genCoverage) printf("JET_COVERAGE_UP(%d); ", stmt->line);
            if (genLineProfile) {
                // printf("    _lprof_tmp_ = getticks();\n");
                printf("JET_PROFILE_LINE(%d);", stmt->line);
                // printf("    _lprof_last_ = _lprof_tmp_;\n");
            }
            if (genCoverage || genLineProfile) puts(" /************/");
        }

        ASTExpr_emit(stmt, level);
        if (!isCtrlExpr(stmt) && stmt->kind != tkKeyword_return)
            puts(";");
        else
            puts("");
        // convert this into a flag which is set in the resolution pass
        if (ASTExpr_throws(stmt))
            printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
    }
}

///////////////////////////////////////////////////////////////////////////
static void ASTType_genJson(ASTType* type) {
    printf("static void %s_json_(const %s self, int nspc) {\n", type->name,
        type->name);

    printf("    printf(\"{\\n\");\n");
    // printf("    printf(\"\\\"_type_\\\": \\\"%s\\\"\");\n", type->name);
    // if (type->body->locals) printf("    printf(\",\\n\");\n");

    // TODO: move this part into its own func so that subclasses can ask the
    // superclass to add in their fields inline
    foreachn(ASTVar*, var, vars, type->body->locals) {
        if (!var /*or not var->used*/) continue;
        printf("    printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n",
            var->name);
        const char* valueType = ASTExpr_typeName(var->init);
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
static void ASTType_genJsonReader(ASTType* type) { }

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
static void ASTFunc_printStackUsageDef(size_t stackUsage) {
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
static void ASTType_emit(ASTType* type, int level) {
    // if (! type->body or not type->analysed) return;
    const char* const name = type->name;
    printf("#define FIELDS_%s \\\n", name);
    foreach (ASTVar*, var, type->body->locals) {
        if (!var /*or not var->used*/) continue;
        // It's not so easy to just skip 'unused' type members.
        // what if I just construct an object and print it?
        // I expect to see the default members. But if they
        // haven't been otherwise accessed, they are left out.
        ASTVar_emit(var, level + STEP, false);
        printf("; \\\n");
    }
    printf("\n\nstruct %s {\n", name);

    if (type->super) {
        printf("    FIELDS_");
        ASTTypeSpec_emit(type->super, level, false);
        printf("\n");
    }

    printf("    FIELDS_%s\n};\n\n", name);
    printf("static const char* %s_name_ = \"%s\";\n\n", name, name);
    printf("static %s %s_alloc_() {\n    return _Pool_alloc_(&gPool_, "
           "sizeof(struct %s));\n}\n\n",
        name, name, name);
    printf("static %s %s_init_(%s self) {\n", name, name, name);

    foreach (ASTVar*, var, type->body->locals) // if (var->used)
        printf("#define %s self->%s\n", var->name, var->name);

    foreach (ASTExpr*, stmt, type->body->stmts) {
        if (!stmt || stmt->kind != tkVarAssign || !stmt->var->init)
            //            or not stmt->var->used)
            continue;
        printf("%.*s%s = ", level + STEP, spaces, stmt->var->name);
        ASTExpr_emit(stmt->var->init, 0);
        puts(";");
        if (ASTExpr_throws(stmt->var->init))
            puts("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    foreach (ASTVar*, var, type->body->locals) // if (var->used)
        printf("#undef %s \n", var->name);

    printf("    return self;\n}\n\n");
    printf("%s %s_new_(IFDEBUG(const char* callsite_)) {\n", name, name);

    printf("#define DEFAULT_VALUE NULL\n    "
           "IFDEBUG(static const char* sig_ = \"%s()\");\n",
        name);
    ASTFunc_printStackUsageDef(48);
    puts(functionEntryStuff_UNESCAPED);
    printf("    %s ret = %s_alloc_(); %s_init_(ret);\n"
           "    TRACE_IF_ERROR;\n"
           "    _err_ = NULL; STACKDEPTH_DOWN; return ret;\n",
        name, name, name);
    puts(functionExitStuff_UNESCAPED);
    puts("#undef DEFAULT_VALUE\n#undef MYSTACKUSAGE\n}\n");
    printf("#define %s_print(p) %s_print__(p, STR(p))\n", name, name);
    printf("void %s_print__(%s self, const char* name) {\n    printf(\"<%s "
           "'%%s' at %%p size %%luB>\\n\",name, self, sizeof(struct %s));\n}\n",
        name, name, name, name);
    puts("");

    ASTType_genJson(type);
    ASTType_genJsonReader(type);
}

///////////////////////////////////////////////////////////////////////////
static void ASTType_genh(ASTType* type, int level) {
    if (!type->body || !type->analysed) return;
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
static void ASTFunc_emit(ASTFunc* func, int level) {
    if (!func->body || !func->analysed) return; // declares, default ctors

    // actual stack usage is higher due to stack protection, frame bookkeeping
    // ...
    size_t stackUsage = ASTFunc_calcSizeUsage(func);
    ASTFunc_printStackUsageDef(stackUsage);

    printf(
        "#define DEFAULT_VALUE %s\n", getDefaultValueForType(func->returnSpec));
    if (!func->isExported) printf("static ");
    if (func->returnSpec) {
        ASTTypeSpec_emit(func->returnSpec, level, false);
    } else {
        printf("void");
    }
    printf(" %s(", func->selector);
    foreachn(ASTVar*, arg, args, func->args) {
        ASTVar_emit(arg, level, true);
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

    foreachn(ASTVar*, arg, args, func->args) {
        ASTVar_lint(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");
    if (func->returnSpec) {
        printf(" as ");
        ASTTypeSpec_lint(func->returnSpec, level);
    }
    puts("\");");

    puts(functionEntryStuff_UNESCAPED);

    ASTScope_emit(func->body, level + STEP);

    puts(functionExitStuff_UNESCAPED);
    puts("}\n#undef DEFAULT_VALUE");
    puts("#undef MYSTACKUSAGE");
}

///////////////////////////////////////////////////////////////////////////
static void ASTFunc_genh(ASTFunc* func, int level) {
    // if (! func->body or not func->analysed) return;
    if (!func->isExported) printf("static ");
    if (func->returnSpec) {
        ASTTypeSpec_emit(func->returnSpec, level, false);
    } else {
        printf("void");
    }
    printf(" %s(", func->selector);
    foreachn(ASTVar*, arg, args, func->args) {
        ASTVar_emit(arg, level, true);
        printf(args->next ? ", " : "");
    }
    printf("\n#ifdef DEBUG\n    %c const char* callsite_\n#endif\n",
        ((func->args && func->args->item) ? ',' : ' '));
    puts(");\n");
}

////////////////////////////////////////////////////
static void ASTTest_emit(ASTTest* test) // TODO: should tests not return BOOL?
{
    if (!test->body) return;
    printf("\nstatic void test_%s() {\n", test->name);
    ASTScope_emit(test->body, STEP);
    puts("}");
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a subscript (that has been resolved to
/// its corresponding `ASTVariable`). This function does all of the heavy
/// lifting to decide what the subscript actually does, based on the kind of the
/// subscript expression, number of dimensions and the context.
static void ASTExpr_emit_tkSubscriptResolved(ASTExpr* expr, int level) {
    char* name = expr->var->name;
    ASTExpr* index = expr->left;
    assert(index);
    // index = index->right;
    switch (index->kind) {
    case tkNumber: // indexing with a single number, can be a -ve number
        printf("Array_get_%s(%s, %s)", ASTTypeSpec_cname(expr->var->typeSpec),
            name, index->string);
        break;

    case tkString:
    case tkRawString: // indexing with single string or regex
        printf("Dict_get_CString_%s(%s, %s)",
            ASTTypeSpec_cname(expr->var->typeSpec), name, index->string);
        break;

    case tkOpComma: // higher dims. validation etc. has been done by this stage.

        // this is for cases like arr[2, 3, 4].
        printf("Tensor%dD_get_%s(%s, {", expr->var->typeSpec->dims,
            ASTTypeSpec_cname(expr->var->typeSpec), name);
        ASTExpr_emit(index, 0);
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

    case tkOpColon:
        // a single range.
        printf("Array_getSlice_%s(%s, ", ASTTypeSpec_name(expr->var->typeSpec),
            name);
        ASTExpr_emit(index, 0);
        printf(")");
        break;
        // what about mixed cases, e.g. arr[2:3, 5, 3:end]
        // make this portion a recursive function then, or promote
        // all indexes to ranges first and then let opcomma handle it

    case tkOpEQ:
    case tkOpLE:
    case tkOpGE:
    case tkOpGT:
    case tkOpLT:
    case tkOpNE:
    case tkKeyword_and:
    case tkKeyword_or:
    case tkKeyword_not:
        // indexing by a Boolean expression (filter)
        // by default this implies a copy, but certain funcs e.g. print
        // min max sum count etc. can be done in-place without a copy
        // since they are not mutating the array. That requires either
        // the user to call print(arr, filter = arr < 5) instead of
        // print(arr[arr < 5]), or the compiler to transform the second
        // into the first transparently.
        // Probably the tkFunctionCall should check if its argument is
        // a tkSubscript with a Boolean index, and then tip the user
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
        printf("Array_copy_filter_%s(%s, ",
            ASTTypeSpec_name(expr->var->typeSpec), name);
        ASTExpr_emit(index, 0);
        printf(")");
        break;

    default:
        unreachable("bad kind: %s", TokenKind_str[expr->kind]);
        break;
    }
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a function call (that has been resolved to
/// its corresponding `ASTFunc`). Type constructors call a C function
/// that has `_new` appended to the type name. This function passes a
/// constructed string as the extra argument `callsite_` that is used to
/// generate accurate backtraces.
static void ASTExpr_emit_tkFunctionCallResolved(ASTExpr* expr, int level) {
    char* tmp = expr->func->selector;

    ASTExpr* arg1 = expr->left;
    const char* tmpc = "";
    if (arg1) {
        if (arg1->kind == tkOpComma) arg1 = arg1->left;
        tmpc = CollectionType_nativeName(arg1->collectionType);
    }
    printf("%s%s", tmpc, tmp);
    if (*tmp >= 'A' && *tmp <= 'Z' && !strchr(tmp, '_')) printf("_new_");
    printf("(");

    if (expr->left) ASTExpr_emit(expr->left, 0);

    if (!expr->func->isDeclare) {
        printf("\n#ifdef DEBUG\n"
               "      %c \"./\" THISFILE \":%d:%d:\\e[0m ",
            expr->left ? ',' : ' ', expr->line, expr->col);
        ASTExpr_lint(expr, 0, false, true);
        printf("\"\n"
               "#endif\n        ");
    }
    printf(")");
}

char* strchrnul(char* str, char ch) {
    while (*str && *str != ch) str++;
    return str;
}

// This is really taking up a whole pass. It should be moved to the analysis
// phase! ASTExpr should hold some extra data for string interpolation.
static void ASTExpr_prepareInterp(
    ASTExpr* expr, ASTScope* scope) { // TODO: you should have a flag that tells
                                      // you whether there is any string
    // interpolation at all, and if yes what the expected size might be, and if
    // it can be put on the stack

    // actually the string may have embedded %s, so you need to process it
    // in any case, unless you plan on doing puts.
    // there are 3 things to do.
    // 1. compute the format string
    // 2. compute a guess for the size
    // 3. keep a stack of vars that are in the string in the right order
    static Array(Ptr) vars;

    //     char* pos = expr->string - 1,*last = expr->string;
    //     while (*++pos) {
    // while (*pos!='$') pos++;
    switch (expr->kind) {
    case tkString:
        //      }
        {
            char* pos = expr->string + 1; // starts with '"'
            while (*pos) {
                // in this loop you print the text preceding a $, then text
                // suceeding a $, then loop. Here's the text before the $
                char* dollar = strchrnul(pos, '$');
                // not using $..., using {...}
                long lentxt = dollar - pos;
                // printf(",\"%.*s\"", lentxt, pos);
                if (!*dollar) break;
                // after the $ is a variable, so look it up etc.
                char* varname = dollar + 1;
                char* varend = strchrnul(varname, ' ');
                *varend = 0;
                ASTVar* var = ASTScope_getVar(scope, varname);
                // ^ TODO: this should be getQualVar which looks up a.b.c and
                // returns c
                if (!var) {
                    // you have a real problem. You should have checked for all
                    // var refs to be valid in the analysis phase!
                    unreachable("undefined var found: %s", varname);
                }
                const char* fmtstr;
                if (var->typeSpec->typeType == TYObject) {
                    fmtstr = "%s";
                    // you'll have to call Type_str(...) for it. Or actually why
                    // not just do Type_print?
                } else {
                    fmtstr = TypeType_format(var->typeSpec->typeType, false);
                }
                // printf(",%s", var->name);
                // ^ this should be qualified name or the cgen version
                // of accessing the actual member for a.b.c
                pos = varend + 1; // you might go past the end!!!
            }
        }
        break;
    case tkRawString:
    case tkNumber:
    case tkIdentifierResolved:
    case tkIdentifier:
    case tkArgumentLabel:
        break;
    case tkFunctionCallResolved:
    case tkFunctionCall:
    case tkSubscript:
    case tkObjectInit:
    case tkObjectInitResolved:
    case tkSubscriptResolved:
        if (expr->left) ASTExpr_prepareInterp(expr->left, scope);
        break;
    case tkVarAssign:
        ASTExpr_prepareInterp(expr->var->init, scope);
        break;
    case tkKeyword_for:
    case tkKeyword_if:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_while:
        ASTExpr_prepareInterp(expr->left, scope);
        foreach (ASTExpr*, subexpr, expr->body->stmts)
            ASTExpr_prepareInterp(subexpr, expr->body);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary) ASTExpr_prepareInterp(expr->left, scope);
            if (expr->right) ASTExpr_prepareInterp(expr->right, scope);
        } else {
            unreachable("unknown token kind %s", TokenKind_str[expr->kind]);
        }
    }

    // #this string
    //     "The quick brown fox $jumps over $the lazy dog.";
    // #becomes
    //     `"%s%s%s%s%s", "The quick brown fox ", jumps, " over ", the, "lazy
    //     dog.",
    //         ""`;
}

static void ASTExpr_emit_tkString(ASTExpr* expr, int level) {

    printf("%s\"", expr->string);
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a (literal) numeric expression. Complex
/// numbers follow C99 literal syntax, e.g. 1i generates `_Complex_I * 1`.
static void ASTExpr_emit_tkNumber(ASTExpr* expr, int level) {
    size_t ls = CString_length(expr->string);
    if (expr->string[ls - 1] == 'i') {
        printf("_Complex_I*");
        expr->string[ls - 1] = 0;
    }
    printf("%s", expr->string);
}

static void ASTExpr_emit_tkCheck(ASTExpr* expr, int level) {
    // TODO: need llhs and lrhs in case all 3 in 3way are exprs
    // e.g. check a+b < c+d < e+f
    ASTExpr* checkExpr = expr->right; // now use checkExpr below
    ASTExpr* lhsExpr = checkExpr->left;
    ASTExpr* rhsExpr = checkExpr->right;
    printf("{\n");
    if (!checkExpr->unary) {
        printf("%.*s%s _lhs = ", level, spaces, ASTExpr_typeName(lhsExpr));
        ASTExpr_emit(lhsExpr, 0);
        printf(";\n");
    }
    printf("%.*s%s _rhs = ", level, spaces, ASTExpr_typeName(rhsExpr));
    ASTExpr_emit(rhsExpr, 0);
    printf(";\n");
    printf("%.*sif (!(", level, spaces);
    // ----- use lhs rhs cached values instead of the expression
    ASTExpr_emit(checkExpr, 0);
    // how are you doing to deal with x < y < z? Repeat all the logic of
    // ASTExpr_emit?
    // ----------------------------------------------------------------
    // if (checkExpr->unary) {
    //     printf("_rhs");
    // } else {
    //     printf("_lhs %s _rhs");
    // }
    // -------------
    printf(")) {\n");
    printf("%.*sprintf(\"\\n\\n\e[31mruntime error:\e[0m check "
           "failed at \e[36m./%%s:%d:%d:\e[0m\\n    %%s\\n\\n\",\n            "
           "   THISFILE, \"",
        level + STEP, spaces, expr->line, expr->col + 6);
    ASTExpr_lint(checkExpr, 0, true, true);
    printf("\");\n");
    printf("#ifdef DEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

    ASTExpr_genPrintVars(checkExpr, level + STEP);
    // the `printed` flag on all vars of the expr will be set
    // (genPrintVars uses this to avoid printing the same var
    // twice). This should be unset after every toplevel call to
    // genPrintVars.
    if (!checkExpr->unary) {
        // dont print literals or arrays
        if (lhsExpr->collectionType == CTYNone //
            && lhsExpr->kind != tkString //
            && lhsExpr->kind != tkNumber //
            && lhsExpr->kind != tkRawString //
            && lhsExpr->kind != tkOpLE //
            && lhsExpr->kind != tkOpLT) {

            if (lhsExpr->kind != tkIdentifierResolved
                || !lhsExpr->var->visited) {
                printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
                printf("%s", TypeType_format(lhsExpr->typeType, true));
                printf("%s", "\\n\", \"");
                ASTExpr_lint(lhsExpr, 0, true, true);
                printf("%s", "\", _lhs);\n");
            }
            // checks can't have tkVarAssign inside them
            // if ()
            //     lhsExpr->var->visited = true;
        }
    }
    if (rhsExpr->collectionType == CTYNone //
        && rhsExpr->kind != tkString //
        && rhsExpr->kind != tkNumber //
        && rhsExpr->kind != tkRawString) {
        if (rhsExpr->kind != tkIdentifierResolved || !rhsExpr->var->visited) {
            printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
            printf("%s", TypeType_format(rhsExpr->typeType, true));
            printf("%s", "\\n\", \"");
            ASTExpr_lint(rhsExpr, 0, true, true);
            printf("%s", "\", _rhs);\n");
        }
    }

    ASTExpr_unmarkVisited(checkExpr);

    printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
    printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
    printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
}

///////////////////////////////////////////////////////////////////////////
/// This should be a standard dispatcher that does nothing except the actual
/// dispatching (via a function pointer table, not a switch).
static void ASTExpr_emit(ASTExpr* expr, int level) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("%.*s", level, spaces);
    switch (expr->kind) {
    case tkNumber:
        ASTExpr_emit_tkNumber(expr, level);
        break;

    case tkMultiDotNumber:
    case tkIdentifier:
        printf("%s", expr->string);
        break;

    case tkString: // TODO: parse vars inside, escape stuff, etc.
        ASTExpr_emit_tkString(expr, level);
        // printf(escStrings ? "\\%s\\\"" : "%s\"", expr->string);
        break;

    case tkIdentifierResolved:
        printf("%s", expr->var->name);
        break;

    case tkRawString: // 'raw strings' or 'regexes'
        printf("\"%s\"", expr->string + 1);
        break;

    case tkRegexp: // inline C code?
        printf("%s", expr->string + 1);
        break;

    case tkLineComment: // TODO: skip  comments in generated code
        printf("// %s", expr->string);
        break;

    case tkFunctionCall:
        unreachable("unresolved call to '%s'\n", expr->string);
        break;

    case tkFunctionCallResolved:
        ASTExpr_emit_tkFunctionCallResolved(expr, level);
        break;

    case tkSubscript:
        unreachable("unresolved subscript on '%s'\n", expr->string);
        break;

    case tkSubscriptResolved:
        ASTExpr_emit_tkSubscriptResolved(expr, level);
        break;

    case tkOpAssign:
    case tkPlusEq:
    case tkMinusEq:
    case tkTimesEq:
    case tkSlashEq:
    case tkPowerEq:
    case tkOpModEq:

        switch (expr->left->kind) {
        case tkSubscriptResolved:
            switch (expr->left->left->kind) {
            case tkNumber:
            case tkString:
            case tkRawString:
                // TODO: astexpr_typename should return Array_Scalar or
                // Tensor2D_Scalar or Dict_String_Scalar etc.
                printf("%s_set(%s, %s,%s, ", ASTExpr_typeName(expr->left),
                    expr->left->var->name, expr->left->left->string,
                    TokenKind_repr(expr->kind, true));
                ASTExpr_emit(expr->right, 0);
                printf(")");
                break;

            case tkOpColon:
                printf("%s_setSlice(%s, ", ASTExpr_typeName(expr->left),
                    expr->left->var->name);
                ASTExpr_emit(expr->left->left, 0);
                printf(",%s, ", TokenKind_repr(expr->kind, true));
                ASTExpr_emit(expr->right, 0);
                printf(")");
                break;

            case tkOpEQ:
            case tkOpGE:
            case tkOpNE:
            case tkOpGT:
            case tkOpLE:
            case tkOpLT:
            case tkKeyword_and:
            case tkKeyword_or:
            case tkKeyword_not:
                printf("%s_setFiltered(%s, ", ASTExpr_typeName(expr->left),
                    expr->left->var->name);
                ASTExpr_emit(expr->left->left, 0);
                printf(",%s, ", TokenKind_repr(expr->kind, true));
                ASTExpr_emit(expr->right, 0);
                printf(")");
                break;

            case tkOpComma:
                // figure out the type of each element
                // there should be a RangeND just like TensorND and SliceND
                // then you can just pass that to _setSlice
                break;
            case tkIdentifierResolved:
                // lookup the var type. note that it need not be Number,
                // string, range etc. it could be an arbitrary object in
                // case you are indexing a Dict with keys of that type.
                break;
            case tkSubscriptResolved:
                // arr[arr2[4]] etc.
                break;
            case tkFunctionCallResolved:
                // arr[func(x)]
                break;
            default:
                unreachable("%s\n", TokenKind_str[expr->left->kind]);
                assert(0);
            }
            break;
        case tkIdentifierResolved:
        case tkPeriod:
            ASTExpr_emit(expr->left, 0);
            printf("%s", TokenKind_repr(expr->kind, true));
            ASTExpr_emit(expr->right, 0);
            break;
        case tkIdentifier:
            unreachable("unresolved var %s", expr->left->string);
            break;
        case tkArgumentLabel:
            // assert(inFuncArgs);
            ASTExpr_emit(expr->right, 0);
            // function call arg label, do not generate ->left
            break;
        case tkString:
            break;
        default:
            // error: not a valid lvalue
            // TODO: you should at some point e,g, during resolution check
            // for assignments to invalid lvalues and raise an error
            unreachable(
                "found token kind %s\n", TokenKind_str[expr->left->kind]);
        }
        // if (! inFuncArgs) {
        //     ASTExpr_emit(self->left, 0,
        //     escStrings); printf("%s", TokenKind_repr(tkOpAssign,
        //     spacing));
        // }
        // ASTExpr_emit(self->right, 0,     escStrings);
        // check various types of lhs  here, eg arr[9:87] = 0,
        // map["uuyt"]="hello" etc.
        break;

    case tkArrayOpen:
        // TODO: send parent ASTExpr* as an arg to this function. Then
        // here do various things based on whether parent is a =,
        // funcCall, etc.
        if (!expr->right) {
            printf("Array_init(%s)()", "double");
        } else {
            printf("Array_make(((%s[]) {", "double"); // FIXME
            // TODO: MKARR should be different based on the CollectionType
            // of the var or arg in question, eg stack cArray, heap
            // allocated Array, etc.
            ASTExpr_emit(expr->right, 0);
            printf("})");
            printf(", %d)", ASTExpr_countCommaList(expr->right));
        }
        break;

    case tkBraceOpen: {
        const char* Ktype = "CString";
        const char* Vtype = "Real64";
        if (!expr->right)
            printf("Dict_init(%s,%s)()", Ktype, Vtype); // FIXME
        else {
            printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
                ASTExpr_countCommaList(expr->right), Ktype); // FIXME

            ASTExpr* p = expr->right;
            while (p && p->kind == tkOpComma) {
                ASTExpr_emit(p->left->left, 0);
                printf(", ");
                p = p->right;
            };
            ASTExpr_emit(p->left, 0);
            printf("}, (%s[]){", Vtype);
            p = expr->right;
            while (p && p->kind == tkOpComma) {
                ASTExpr_emit(p->left->right, 0);
                printf(", ");
                p = p->right;
            };
            ASTExpr_emit(p->right, 0);
            printf("})");
        }
    } break;

    case tkOpColon: // convert 3:4:5 to range(...)
                    // must do bounds check first!
        printf(
            "%s(", expr->left->kind != tkOpColon ? "range_to" : "range_to_by");
        if (expr->left->kind == tkOpColon) {
            expr->left->kind = tkOpComma;
            ASTExpr_emit(expr->left, 0);
            expr->left->kind = tkOpColon;
        } else
            ASTExpr_emit(expr->left, 0);
        printf(", ");
        ASTExpr_emit(expr->right, 0);
        printf(")");
        break;

    case tkVarAssign: // basically a tkOpAssign corresponding to a local
                      // var
        // var x as XYZ = abc... -> becomes an ASTVar and an
        // ASTExpr (to keep location). Send it to ASTVar::gen.
        if (expr->var->init != NULL && expr->var->used) {
            printf("%s = ", expr->var->name);
            ASTExpr_emit(expr->var->init, 0);
        } else {
            printf("/* %s %s at line %d */", expr->var->name,
                expr->var->used ? "null" : "unused", expr->line);
        }
        break;

    case tkKeyword_else:
        puts("else {");
        if (expr->body) ASTScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_elif:
        puts("else if (");
        ASTExpr_emit(expr->left, 0);
        puts(") {");
        if (expr->body) ASTScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_for:
    case tkKeyword_if:
        //    case tkKeyword_elif:
        //    case tkKeyword_else:
    case tkKeyword_while:
        if (expr->kind == tkKeyword_for)
            printf("FOR(");
        else
            printf("%s (", TokenKind_repr(expr->kind, true));
        if (expr->kind == tkKeyword_for) expr->left->kind = tkOpComma;
        if (expr->left) ASTExpr_emit(expr->left, 0);
        if (expr->kind == tkKeyword_for) expr->left->kind = tkOpAssign;
        puts(") {");
        if (expr->body) ASTScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkPower:
        printf("pow(");
        ASTExpr_emit(expr->left, 0);
        printf(",");
        ASTExpr_emit(expr->right, 0);
        printf(")");
        break;

    case tkKeyword_return:
        printf("{_err_ = NULL; STACKDEPTH_DOWN; return ");
        if (expr->right) ASTExpr_emit(expr->right, 0);
        printf(";}\n");
        break;

    case tkKeyword_check:
        ASTExpr_emit_tkCheck(expr, 0);
        break;

    case tkPeriod:
        ASTExpr_emit(expr->left, 0);
        printf("->"); // may be . if right is embedded and not a reference
        ASTExpr_emit(expr->right, 0);
        break;

    case tkOpEQ:
    case tkOpNE:
    case tkOpGE:
    case tkOpLE:
    case tkOpGT:
    case tkOpLT:
        if ((expr->kind == tkOpLE || expr->kind == tkOpLT)
            && (expr->left->kind == tkOpLE | expr->left->kind == tkOpLT)) {
            printf("%s_cmp3way_%s_%s(", ASTExpr_typeName(expr->left->right),
                TokenKind_ascrepr(expr->kind, false),
                TokenKind_ascrepr(expr->left->kind, false));
            ASTExpr_emit(expr->left->left, 0);
            printf(", ");
            ASTExpr_emit(expr->left->right, 0);
            printf(", ");
            ASTExpr_emit(expr->right, 0);
            printf(")");
            break;
        } else if (expr->right->typeType == TYString) {
            printf("CString_cmp(%s, ", tksrepr[expr->kind]);
            ASTExpr_emit(expr->left, 0);
            printf(", ");
            ASTExpr_emit(expr->right, 0);
            printf(")");
            break;
        }
        fallthrough default : if (!expr->prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr->left && expr->left->prec && expr->left->prec < expr->prec;
        bool rightBr = expr->right && expr->right->prec
            && expr->right->kind != tkKeyword_return
            && expr->right->prec < expr->prec;
        // found in 'or return'

        char lpo = '(';
        char lpc = ')';
        if (leftBr) putc(lpo, stdout);
        if (expr->left) ASTExpr_emit(expr->left, 0);
        if (leftBr) putc(lpc, stdout);

        if (expr->kind == tkArrayOpen)
            putc('{', stdout);
        else
            printf("%s", TokenKind_repr(expr->kind, true));

        char rpo = '(';
        char rpc = ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right) ASTExpr_emit(expr->right, 0);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tkArrayOpen) putc('}', stdout);
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
//     // "    for (int i=NUMLINES-1; i>0; i--) _lprof_[i] -= _lprof_[i-1];\n"
//     "    ticks sum=0; for (int i=0; i<NUMLINES; i++) sum += _lprof_[i];\n"
//     "    for (int i=0; i<NUMLINES; i++) {\n"
//     "        double pct = _lprof_[i] * 100.0 / sum;\n"
//     "        if (pct>1.0)"
//     "            fprintf(fd,\" %8.1f%% |\\n\", pct);\n"
//     "        else if (pct == 0.0)"
//     "            fprintf(fd,\"           |\\n\");\n"
//     "        else"
//     "            fprintf(fd,\"         ~ |\\n\");\n"
//     "    }\n"
//     "    fclose(fd);\n"
//     "    system(\"paste -d ' ' .\" THISFILE \"r \" THISFILE \" > \" "
//     "THISFILE "
//     "\"r\" );"
//     "}\n"
//     "static void lineprofile_begin() {_lprof_last_=getticks();}\n"
// };
///////////////////////////////////////////////////////////////////////////
// TODO: why do you need to pass level here?

void ASTType_genTypeInfoDecls(ASTType* type);
void ASTType_genTypeInfoDefs(ASTType* type);
void ASTType_genNameAccessors(ASTType* type);
static void ASTModule_emit(ASTModule* module) {
    // puts("");
    foreach (ASTImport*, import, module->imports)
        ASTImport_emit(import, 0);

    puts("");

    foreach (ASTType*, type, module->types) {
        if (type->body && type->analysed) {
            ASTType_genh(type, 0);
            ASTType_genTypeInfoDecls(type);
        }
    }
    foreach (ASTFunc*, func, module->funcs) {
        if (func->body && func->analysed) { ASTFunc_genh(func, 0); }
    }

    foreach (ASTType*, type, module->types) {
        if (type->body && type->analysed) {
            foreach (ASTExpr*, expr, type->body->stmts)
                ASTExpr_prepareInterp(expr, type->body);
            // ^ MOVE THIS INTO ASTType_emit
            ASTType_emit(type, 0);
            ASTType_genTypeInfoDefs(type);
            ASTType_genNameAccessors(type);
        }
    }
    foreach (ASTFunc*, func, module->funcs) {
        if (func->body && func->analysed) {
            foreach (ASTExpr*, expr, func->body->stmts) {
                ASTExpr_prepareInterp(expr, func->body);
            }
            // ^ MOVE THIS INTO ASTFunc_emit
            ASTFunc_emit(func, 0);
        }
    }
    foreach (ASTImport*, import, module->imports)
        ASTImport_undefc(import);

    // puts(coverageFunc[genCoverage]);
    // puts(lineProfileFunc[genLineProfile]);
}

static void ASTModule_genTests(ASTModule* module) {
    ASTModule_emit(module);
    foreach (ASTTest*, test, module->tests)
        ASTTest_emit(test);
    // generate a func that main will call
    printf("\nvoid tests_run_%s() {\n", module->name);
    foreach (ASTTest*, test, module->tests)
        printf("    test_%s();\n", test->name);
    puts("}");
}

// Generates a couple of functions that allow setting an integral member of a
// type at runtime by name, or getting a pointer to a member by name.
void ASTType_genNameAccessors(ASTType* type) {
    // TODO: instead of a linear search over all members this should generate
    // a switch for checking using a prefix tree -> see genrec.c

    // ASTType_genMemberRecognizer( type, "Int64 value",  )

    printf("static void* %s__memberNamed(%s self, const char* name) {\n",
        type->name, type->name);
    // TODO: skip bitfield members in this loop or it wont compile
    foreach (ASTVar*, var, type->body->locals) /*if (var->used) */ //
        printf("    if (CString_equals(name, \"%s\")) return &(self->%s);\n",
            var->name, var->name);
    printf("    return NULL;\n}\n");

    // this func sets bools or ints that may be part of bitfields
    printf("static void %s__setMemberNamed(%s self, const char* name, Int64 "
           "value) {\n",
        type->name, type->name);
    foreach (ASTVar*, var, type->body->locals) // if (var->used) //
        if (var->typeSpec->typeType >= TYBool
            && var->typeSpec->typeType <= TYReal64)
            printf("    if (CString_equals(name, \"%s\"))  {self->%s = *(%s*) "
                   "&value;return;}\n",
                var->name, var->name, ASTTypeSpec_cname(var->typeSpec));
    printf("}\n");
}

// Generates some per-type functions that write out meta info of the type to be
// used for reflection, serialization, etc.
void ASTType_genTypeInfoDecls(ASTType* type) {
    printf("static const char* const %s__memberNames[] = {\n    ", type->name);
    if (type->body) //
        foreachn(ASTVar*, var, varn, type->body->locals) {
            if (!var || !var->used) continue;
            printf("\"%s\", ", var->name);
            // ASTVar_emit(var, level + STEP, false);
        }
    printf("};\n");
}

void ASTType_genTypeInfoDefs(ASTType* type) {
    // printf("static const char* const %s__memberNames[] = {\n", type->name);
    // foreachn(ASTVar*, var, varn, type->body->locals)
    // {
    //     if (! var) continue;
    //     printf("\"%s\",\n", var->name);
    //     // ASTVar_emit(var, level + STEP, false);
    //     printf("}; \\\n");
    // }
}
