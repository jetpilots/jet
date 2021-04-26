#define genLineNumbers 0
#define genCoverage 1
#define genLineProfile 1

#define outln(s) fwrite(s "\n", sizeof(s), 1, stdout)
#define outl(s) fwrite(s "", sizeof(s) - 1, 1, stdout)
#define iprintf(nspc, fmt, ...)                                                \
    printf("%.*s", nspc, spaces), printf(fmt, __VA_ARGS__)

static void JetImport_emit(JetImport* import, int level) {
    char* alias = import->aliasOffset + import->name;
    CString_tr_ip_len(import->name, '.', '_', 0);
    printf("\n#include \"%s.h\"\n", import->name);
    if (alias) printf("#define %s %s\n", alias, import->name);
    // TODO: remove #defines! There could be a field of any
    // struct with the same name that will get clobbered
    CString_tr_ip_len(import->name, '_', '.', 0);
}

static void JetImport_undefc(JetImport* import) {
    // if (import->alias) printf("#undef %s\n", import->alias);
}

static void JetTypeSpec_emit(JetTypeSpec* spec, int level, bool isconst) {
    if (isconst) outl("const ");
    // TODO: actually this depends on the collectionType. In general
    // Array is the default, but in other cases it may be SArray, Array64,
    // whatever
    if (spec->dims) {
        if (spec->dims > 1)
            // TODO: this should be TensorND, without type params?
            // well actually there isn't a TensorND, since its not always
            // double thats in a tensor but can be Complex, Range,
            // Reciprocal, Rational, whatever
            // -- sure, but double (and float) should be enough since
            // the other types are rarely needed in a tensor form
            printf("SArray%dD(", spec->dims);
        else
            outl("SArray(");
    }

    switch (spec->typeType) {
    case TYObject:
        // objects are always T* const, if meant to be r/o they are
        // const T* const. Later we may have a byval flag to embed structs
        // or pass around by value.
        // leaving it as is for now
        printf("%s", spec->type->name);
        break;
    case TYUnresolved:
        unreachable(
            "unresolved: '%s' at %d:%d", spec->name, spec->line, spec->col);
        printf("%s", *spec->name ? spec->name : "Error_Type");
        break;
    default: printf("%s", TypeType_name(spec->typeType)); break;
    }

    //     if (isconst ) outl(" const"); // only if a ptr type
    if (spec->dims /*or spec->typeType == TYObject*/) printf("%s", ")");
    //        if (status == TSDimensionedNumber) {
    //            genc(units, level);
    //        }
}

static void JetExpr_emit(JetExpr* expr, int level);

static void JetVar_emit(JetVar* var, int level, bool isconst) {
    // for C the variables go at the top of the block, without init
    printf("%.*s", level, spaces);
    if (var->spec) JetTypeSpec_emit(var->spec, level + STEP, isconst);
    printf(" %s", var->name);
}

// Functions like Array_any_filter, Array_count_filter etc.
// are macros and don't return a value but may set one. For these
// and other such funcs, the call must be moved to before the
// containing statement, and in place of the original call you
// should place a temporary holding the value that would have been
// "returned".
// static bool mustPromote(const char* name) {
//     // TODO: at some point these should go into a dict or trie or MPH
//     // whatever
//     if (!strcmp(name, "Array_any_filter")) return true;
//     if (!strcmp(name, "Array_all_filter")) return true;
//     if (!strcmp(name, "Array_count_filter")) return true;
//     if (!strcmp(name, "Array_write_filter")) return true;
//     if (!strcmp(name, "Strs_print_filter")) return true;
//     return false;
// }

static void JetExpr_unmarkVisited(JetExpr* expr) {
    switch (expr->kind) {
    case tkIdentifierResolved:
    case tkVarAssign: expr->var->visited = false; break;
    case tkFunctionCallResolved:
    case tkFunctionCall: // shouldnt happen
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_while: JetExpr_unmarkVisited(expr->left); break;
    default:
        if (expr->prec) {
            if (!expr->unary) JetExpr_unmarkVisited(expr->left);
            JetExpr_unmarkVisited(expr->right);
        }
    }
}

// given an expr, generate code to print all the resolved vars in it (only
// scalars). for example in f(x + 4) + m + y[5:6], the following should be
// generated
// printf("x = %?\n", x);
// printf("m = %?\n", m);
// checks will print the vars involved in the check expr, if the check
// fails. This routine will be used there.
static void JetExpr_genPrintVars(JetExpr* expr, int level) {
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
        //            JetExpr* e = expr->right;
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
    case tkKeyword_while: JetExpr_genPrintVars(expr->left, level); break;

    default:
        if (expr->prec) {
            if (!expr->unary) JetExpr_genPrintVars(expr->left, level);
            JetExpr_genPrintVars(expr->right, level);
        }
    }
}

// Extraction scan & Extraction happens AFTER resolving functions!
static JetExpr* JetExpr_findExtractionCandidate(JetExpr* expr) {
    assert(expr);
    JetExpr* ret;

    // what about func args?
    switch (expr->kind) {
    case tkFunctionCallResolved:
        // promote innermost first, so check args
        if (expr->left && (ret = JetExpr_findExtractionCandidate(expr->left)))
            return ret;
        else if (expr->extract)
            return expr;
        break;

    case tkSubscriptResolved:
        // TODO: here see if the subscript itself needs to be promoted up
        return JetExpr_findExtractionCandidate(expr->left);

    case tkSubscript: return JetExpr_findExtractionCandidate(expr->left);

    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_while:
        if (expr->left) return JetExpr_findExtractionCandidate(expr->left);
        // body will be handled by parent scope

    case tkVarAssign:
        if ((ret = JetExpr_findExtractionCandidate(expr->var->init)))
            return ret;
        break;

    case tkFunctionCall: // unresolved
        // assert(0);
        unreachable("unresolved call %s\n", expr->string);
        if ((ret = JetExpr_findExtractionCandidate(expr->left))) return ret;
        break;

    default:
        if (expr->prec) {
            if (expr->right
                && (ret = JetExpr_findExtractionCandidate(expr->right)))
                return ret;
            if (!expr->unary)
                if ((ret = JetExpr_findExtractionCandidate(expr->left)))
                    return ret;
        }
    }
    return NULL;
}

static char* newTmpVarName(int num, char c) {
    char buf[8];
    int l = snprintf(buf, 8, "_%c%d", c, num);
    return CString_pndup(buf, l);
}

static bool isCtrlExpr(JetExpr* expr) {
    return expr->kind == tkKeyword_if //
        || expr->kind == tkKeyword_for //
        || expr->kind == tkKeyword_while //
        || expr->kind == tkKeyword_else;
}

static bool isLiteralExpr(JetExpr* expr) { return false; }
static bool isComparatorExpr(JetExpr* expr) { return false; }

static void JetScope_lowerElementalOps(JetScope* scope) {
    foreach (JetExpr*, stmt, scope->stmts) {

        if (isCtrlExpr(stmt) && stmt->body)
            JetScope_lowerElementalOps(stmt->body);

        if (!stmt->elemental) continue;

        // wrap it in an empty block (or use if true)
        JetExpr* ifblk = NEW(JetExpr);
        ifblk->kind = tkKeyword_if;
        ifblk->left = NEW(JetExpr);
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
        // a->start+i #define vec_1 *vec_p1 // these could be JetVars with
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

static void JetScope_promoteCandidates(JetScope* scope) {
    int tmpCount = 0;
    JetExpr* pc = NULL;
    List(JetExpr)* prev = NULL;
    foreachn(JetExpr*, stmt, stmts, scope->stmts) {
        // TODO:
        // if (! stmt->promote) {prev=stmts;continue;}

        if (isCtrlExpr(stmt) && stmt->body)
            JetScope_promoteCandidates(stmt->body);

    startloop:

        if (!(pc = JetExpr_findExtractionCandidate(stmt))) { // most likely
            prev = stmts;
            continue;
        }
        if (pc == stmt) {
            // possible, less likely: stmt already at toplevel.
            // TODO: in this case, you still have to add the extra arg.
            prev = stmts;
            continue;
        }

        JetExpr* pcClone = NEW(JetExpr);
        *pcClone = *pc;

        // 1. add a temp var to the scope
        JetVar* tmpvar = NEW(JetVar);
        tmpvar->name = newTmpVarName(++tmpCount, 'p');
        tmpvar->spec = NEW(JetTypeSpec);
        //        tmpvar->spec->typeType = TYReal64; // FIXME
        // TODO: setup tmpvar->spec
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
            JetExpr* com = NEW(JetExpr);
            // TODO: really should have an astexpr ctor
            com->prec = TokenKind_getPrecedence(tkOpComma);
            com->kind = tkOpComma;
            com->left = pcClone->left;
            com->right = pc;
            pcClone->left = com;
        } else {
            JetExpr* argn = pcClone->left;
            while (argn->kind == tkOpComma && argn->right->kind == tkOpComma)
                argn = argn->right;
            JetExpr* com = NEW(JetExpr);
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
        } // List(JetExpr)* insertionPos = prev ? prev->next : self->stmts;
          //  insertionPos
        //  = insertionPos;
        goto startloop; // it will continue there if no more Extractions are
                        // needed

        prev = stmts;
    }
}

void JetVar_insertDrop(JetVar* var, int level) {
    iprintf(level, "DROP(%s,%s,%s,%s);\n", JetTypeSpec_name(var->spec),
        var->name, CollectionType_nativeName(var->spec->collectionType),
        StorageClassNames[var->storage]);
}

static void JetScope_emit(JetScope* scope, int level) {

    foreach (JetExpr*, stmt, scope->stmts) {
        if (stmt->kind == tkLineComment) continue;

        if (genLineNumbers) printf("#line %d\n", stmt->line);

        // You need to know if the JetExpr_emit will in fact generate something.
        // This is true in general unless it is an unused var init.
        if (stmt->kind != tkVarAssign || stmt->var->used) {

            if (genCoverage)
                iprintf(level, "JET_COVERAGE_UP(%d); \n", stmt->line);
            if (genLineProfile) {
                iprintf(level, "JET_PROFILE_LINE(%d);\n", stmt->line);
            }
            if (genCoverage || genLineProfile) outln("");
        }

        JetExpr_emit(stmt, level);
        if (!isCtrlExpr(stmt) && stmt->kind != tkKeyword_return)
            outln(";");
        else
            outln("");
        // convert this into a flag which is set in the resolution pass

        // here you see if any vars are to be dropped at this point (because
        // they were last used in this expr. A var can only be dropped in its
        // own scope, not an inner or outer scope, so just scan our own vars.
        // In a sense the stmt->line is a local ID for the statement within the
        // scope.
        JetScope* sco = scope;
        do {
            foreach (JetVar*, var, sco->locals)
                if (var->used && var->lastUsage == stmt->line) {
                    JetVar_insertDrop(var, level);
                    var->lastUsage = 0; // this means var has been dropped.
                }
        } while (!sco->isLoop // if loop scope, don't walk up
            && (sco = sco->parent) // walk up to the last loop scope
            && sco->parent && !sco->parent->isLoop);
        // all scopes have the global scope as the final parent.
        // what if some scope tries to drop something from there?

        // TODO:
        // Maybe scope should have a lineno. If a loop scope has the lastUsage
        // of a parent var, it cannot drop it inside the loop, but it should be
        // done just after the loop ends. Now it will be dropped anyway at
        // owning scope end which may be suboptimal. If you have the scope line,
        // which is the line of the cond expr of if / while etc., you change the
        // lastUsage to that line and it gets dropped just after the scope.

        if (JetExpr_throws(stmt))
            printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
    }
    // It's possible some vars were not detected in inner scopes and dropped. So
    // let's drop them here. No need to walk up the parent chain here.
    foreach (JetVar*, var, scope->locals)
        if (var->used && var->lastUsage) JetVar_insertDrop(var, level);
    // ^ these are all the vars whose lastUsage could not be
    // matched. This may be because they are in an inner scope. In
    // this case they should be dropped at the end of the scope.
    // Optimize this later so that if there are multiple subscopes
    // and the last usage is in one of them, the drop happens
    // after that subscope and doesn't wait until the very end.
}

static void JetType_genJson(JetType* type) {
    printf("static void %s_json_(const %s self, int nspc) {\n", type->name,
        type->name);

    printf("    printf(\"{\\n\");\n");

    // TODO: move this part into its own func so that subclasses can ask the
    // superclass to add in their fields inline
    foreachn(JetVar*, var, vars, type->body->locals) {
        if (!var) continue;
        printf("    printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n",
            var->name);
        const char* valueType = JetExpr_typeName(var->init);
        printf("    %s_json_(self->%s, nspc+4);\n    printf(\"", valueType,
            var->name);
        if (vars->next) outl(",");
        outln("\\n\");");
    }
    printf("    printf(\"%%.*s}\", nspc, _spaces_);\n");
    printf("}\nMAKE_json_wrap_(%s)\n//MAKE_json_file(%s)\n", type->name,
        type->name);
}

static void JetType_genJsonReader(JetType* type) { }

static const char functionEntryStuff_UNESCAPED[]
    = "    STACKDEPTH_UP; DO_STACK_CHECK;\n";

static const char functionExitStuff_UNESCAPED[]
    = "\n"
      "    return result;\n"
      "uncaught: HANDLE_UNCAUGHT;\n"
      "backtrace: SHOW_BACKTRACE_LINE;\n"
      "return_: STACKDEPTH_DOWN;\n"
      "    return DEFAULT_VALUE;";

static void JetFunc_printStackUsageDef(size_t stackUsage) {
    printf("#define MYSTACKUSAGE (%lu + 6*sizeof(void*) + "
           "IFDEBUGELSE(sizeof(char*),0))\n",
        stackUsage);
}

static void JetType_emit(JetType* type, int level) {
    if (!type->body || !type->analysed || type->isDeclare) return;
    // if (! type->body or not type->analysed) return;
    const char* const name = type->name;
    printf("#define FIELDS_%s \\\n", name);
    foreach (JetVar*, var, type->body->locals) {
        if (!var /*or not var->used*/) continue;
        // It's not so easy to just skip 'unused' type members.
        // what if I just construct an object and print it?
        // I expect to see the default members. But if they
        // haven't been otherwise accessed, they are left out.
        JetVar_emit(var, level + STEP, false);
        outln("; \\");
    }
    printf("\n\nstruct %s {\n", name);

    if (type->super) {
        outl("    FIELDS_");
        JetTypeSpec_emit(type->super, level, false);
        outln("");
    }

    printf("    FIELDS_%s\n};\n\n", name);
    printf("static const char* %s_name_ = \"%s\";\n\n", name, name);
    printf("static %s %s_alloc_() {\n    return _Pool_alloc_(&gPool_, "
           "sizeof(struct %s));\n}\n\n",
        name, name, name);
    printf("static %s %s_init_(%s self) {\n", name, name, name);

    foreach (JetVar*, var, type->body->locals) // if (var->used)
        printf("#define %s self->%s\n", var->name, var->name);

    foreach (JetExpr*, stmt, type->body->stmts) {
        if (!stmt //
            || stmt->kind != tkVarAssign //
            || !stmt->var->init)
            continue;
        printf("%.*s%s = ", level + STEP, spaces, stmt->var->name);
        JetExpr_emit(stmt->var->init, 0);
        outln(";");
        if (JetExpr_throws(stmt->var->init))
            outln("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    foreach (JetVar*, var, type->body->locals)
        printf("#undef %s \n", var->name);

    outln("    return self;\n}\n");

    JetFunc_printStackUsageDef(48);
    printf("#define DEFAULT_VALUE NULL\n"
           "JET_STATIC %s %s_new_(IFDEBUG(const char* callsite_)) {\n"
           "IFDEBUG(static const char* sig_ = \"%s()\");\n",
        name, name, name);
    puts(functionEntryStuff_UNESCAPED);
    printf("    %s ret = %s_alloc_(); %s_init_(ret);\n"
           "    TRACE_IF_ERROR;\n"
           "    _err_ = NULL; STACKDEPTH_DOWN; return ret;\n",
        name, name, name);
    puts(functionExitStuff_UNESCAPED);
    outln("#undef DEFAULT_VALUE\n#undef MYSTACKUSAGE\n}\n");
    printf("#define %s_print(p) %s_print__(p, STR(p))\n", name, name);
    printf("JET_STATIC void %s_print__(%s self, const char* name) {\n    "
           "printf(\"<%s "
           "'%%s' at %%p size %%luB>\\n\",name, self, sizeof(struct %s));\n}\n",
        name, name, name, name);
    outln("");

    JetType_genJson(type);
    JetType_genJsonReader(type);
}

static void JetType_genh(JetType* type, int level) {
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

static void JetEnum_genh(JetType* type, int level) {
    if (!type->body || !type->analysed) return;
    const char* const name = type->name;
    outln("typedef enum {");

    foreach (JetVar*, var, type->body->locals)
        printf("    %s_%s,\n", name, var->name);
    printf("} %s;\n", name);
    JetExpr* ex1 = type->body->stmts->item;
    const char* datType
        = ex1->kind == tkOpAssign ? JetExpr_typeName(ex1->right) : NULL;
    if (datType)
        printf("JET_STATIC %s %s__data[%d];\n", datType, name,
            PtrList_count(type->body->locals));
    printf("JET_STATIC const char* %s__fullnames[] ={\n", name);
    foreach (JetVar*, var, type->body->locals)
        printf("    \"%s.%s\",\n", name, var->name);
    outln("};");
    printf("JET_STATIC const char* %s__names[] ={\n", name);
    foreach (JetVar*, var, type->body->locals)
        printf("    \".%s\",\n", var->name);
    outln("};");

    printf("JET_STATIC void %s__init() {\n", name);

    foreach (JetExpr*, stmt, type->body->stmts) {
        if (!stmt || stmt->kind != tkOpAssign) continue;
        printf("%.*s%s__data[%s_%s] = ", level + STEP, spaces, name, name,
            stmt->left->string);
        JetExpr_emit(stmt->right, 0);
        outln(";");
        if (stmt->right->throws) outln("    TRACE_IF_ERROR;");
    }
    outln("}");
}

static void JetFunc_emit(JetFunc* func, int level) {
    if (!func->body || !func->analysed || func->isDeclare)
        return; // declares, default ctors

    // actual stack usage is higher due to stack protection, frame bookkeeping
    // ...
    size_t stackUsage = JetFunc_calcSizeUsage(func);
    JetFunc_printStackUsageDef(stackUsage);

    printf("#define DEFAULT_VALUE %s\n", getDefaultValueForType(func->spec));
    if (!func->isExported) outl("static ");
    if (func->spec) {
        JetTypeSpec_emit(func->spec, level, false);
    } else {
        outl("void");
    }
    printf(" %s(", func->selector);
    foreachn(JetVar*, arg, args, func->args) {
        JetVar_emit(arg, level, true);
        printf(args->next ? ", " : "");
    }

    printf("\n#ifdef DEBUG\n"
           "    %c const char* callsite_ "
           "\n#endif\n",
        ((func->args && func->args->item ? ',' : ' ')));

    // TODO: if (flags.throws) outl("const char** _err_");
    outln(") {");
    printf("    IFDEBUG(static const char* sig_ = \"");
    printf("%s%s(", func->isStmt ? "" : "function ", func->name);

    foreachn(JetVar*, arg, args, func->args) {
        JetVar_write(arg, level);
        printf(args->next ? ", " : "");
    }
    outl(")");
    if (func->spec) {
        outl(" as ");
        JetTypeSpec_write(func->spec, level);
    }
    outln("\");");

    puts(functionEntryStuff_UNESCAPED);

    JetScope_emit(func->body, level + STEP);

    puts(functionExitStuff_UNESCAPED);
    outln("}\n#undef DEFAULT_VALUE");
    outln("#undef MYSTACKUSAGE");
}

static void JetFunc_genh(JetFunc* func, int level) {
    if (!func->body || !func->analysed || func->isDeclare) return;
    if (!func->isExported) outl("static ");
    if (func->spec) {
        JetTypeSpec_emit(func->spec, level, false);
    } else {
        outl("void");
    }
    printf(" %s(", func->selector);
    foreachn(JetVar*, arg, args, func->args) {
        JetVar_emit(arg, level, true);
        printf(args->next ? ", " : "");
    }
    printf("\n#ifdef DEBUG\n    %c const char* callsite_\n#endif\n",
        ((func->args && func->args->item) ? ',' : ' '));
    outln(");\n");
}

static void JetVar_genh(JetVar* var, int level) {
    // if (! func->body or not func->analysed) return;
    // if (!func->isExported) outl("static ");
    // if (var->spec) {
    if (!var->init) return;

    JetTypeSpec_emit(var->spec, level, false);

    printf(" %s = ", var->name);
    JetExpr_emit(var->init, 0);

    outln("");
}

// #define MKEMB(T, ...)                                                          \
//     (T[]) {                                                                    \
//         { __VA_ARGS__ }                                                        \
//     }

// JetFunc* decld = (JetFunc[]) { { .name = "Oiunko",
//     .selector = "Oinko_uio_uyt",
//     .line = 21,
//     .args = (PtrList[]) { { .item = (JetVar[1]) { { .name = "arg1" } } } },
//     .isDeclare = 1,
//     .isRecursive = 1 } };

// JetFunc* declc = MKEMB(JetFunc, .name = "Oiunko", .selector =
// "Oinko_uio_uyt",
//     .line = 21,
//     .args = MKEMB(PtrList, .item = MKEMB(JetVar, .name = "arg1", .line =
//     6)));

static void JetTest_emit(JetTest* test) // TODO: should tests not return BOOL?
{
    if (!test->body) return;
    printf("\nstatic void test_%s() {\n", test->name);
    JetScope_emit(test->body, STEP);
    outln("}");
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a subscript (that has been resolved to
/// its corresponding `JetVariable`). This function does all of the heavy
/// lifting to decide what the subscript actually does, based on the kind of the
/// subscript expression, number of dimensions and the context.
static void JetExpr_emit_tkSubscriptResolved(JetExpr* expr, int level) {
    char* name = expr->var->name;
    JetExpr* index = expr->left;
    assert(index);
    // index = index->right;
    switch (index->kind) {
    case tkNumber: // indexing with a single number, can be a -ve number
        printf("Array_get_%s(%s, %s)", JetTypeSpec_cname(expr->var->spec), name,
            index->string);
        break;

    case tkString:
    case tkRawString: // indexing with single string or regex
        printf("Dict_get_CString_%s(%s, %s)",
            JetTypeSpec_cname(expr->var->spec), name, index->string);
        break;

    case tkOpComma: // higher dims. validation etc. has been done by this stage.

        // this is for cases like arr[2, 3, 4].
        printf("Tensor%dD_get_%s(%s, {", expr->var->spec->dims,
            JetTypeSpec_cname(expr->var->spec), name);
        JetExpr_emit(index, 0);
        outl("})");

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
        printf(
            "Array_getSlice_%s(%s, ", JetTypeSpec_name(expr->var->spec), name);
        JetExpr_emit(index, 0);
        outl(")");
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
        //    and then the generation will follow the modified Jet.
        //    Don't handle this as a special case at the code generation
        //    stage.
        printf("Array_copy_filter_%s(%s, ", JetTypeSpec_name(expr->var->spec),
            name);
        JetExpr_emit(index, 0);
        outl(")");
        break;

    default: unreachable("bad kind: %s", TokenKind_names[expr->kind]); break;
    }
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a function call (that has been resolved to
/// its corresponding `JetFunc`). Type constructors call a C function
/// that has `_new` appended to the type name. This function passes a
/// constructed string as the extra argument `callsite_` that is used to
/// generate accurate backtraces.
static void JetExpr_emit_tkFunctionCallResolved(JetExpr* expr, int level) {
    char* tmp = expr->func->selector;

    JetExpr* arg1 = expr->left;
    const char* tmpc = "";
    if (arg1) {
        if (arg1->kind == tkOpComma) arg1 = arg1->left;
        tmpc = CollectionType_nativeName(arg1->collectionType);
    }
    printf("%s%s", tmpc, tmp);
    if (*tmp >= 'A' && *tmp <= 'Z' && !strchr(tmp, '_')) outl("_new_");
    outl("(");

    if (expr->left) JetExpr_emit(expr->left, 0);

    if (!expr->func->isDeclare) {
        printf("\n#ifdef DEBUG\n"
               "      %c \"./\" THISFILE \":%d:%d:\\e[0m ",
            expr->left ? ',' : ' ', expr->line, expr->col);
        JetExpr_write(expr, 0, false, true);
        printf("\"\n"
               "#endif\n        ");
    }
    outl(")");
}

char* strchrnul(char* str, char ch) {
    while (*str && *str != ch) str++;
    return str;
}
static void astexpr_lineupmultilinestring(JetExpr* expr, int indent) {
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

static void JetExpr_emit_tkString(JetExpr* expr, int level) {
    astexpr_lineupmultilinestring(expr, level + STEP);
    if (!expr->vars) {
        printmultilstr(expr->string + 1);
    } else {
        char* pos = expr->string;
        // putc('"', stdout);
        char* last = pos;
        JetVar* v;
        PtrList* p = expr->vars;
        JetExpr* e = p->item;
        outl("strinterp_h(64, ");
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
                while (e->kind == tkPeriod) e = e->right;
                assert(e->kind == tkIdentifierResolved);
                assert(e->var);
                printf("%s", TypeType_format(e->var->spec->typeType, false));
                last = pos;
                e = ((p = p->next)) ? p->item : NULL;
            }
        }
        printf("\"");
        foreach (JetExpr*, e, expr->vars) {
            outl(", ");
            JetExpr_emit(e, 0);
        }
        outl(")");
    }
}

//_____________________________________________________________________________
/// Emits the equivalent C code for a (literal) numeric expression.
/// Complex numbers follow C99 literal syntax, e.g. 1i generates
/// `_Complex_I * 1`.
static void JetExpr_emit_tkNumber(JetExpr* expr, int level) {
    size_t ls = CString_length(expr->string);
    if (expr->string[ls - 1] == 'i') {
        outl("_Complex_I*");
        expr->string[ls - 1] = 0;
    }
    printf("%s", expr->string);
}

static void JetExpr_emit_tkCheck(JetExpr* expr, int level) {
    // TODO: need llhs and lrhs in case all 3 in 3way are exprs
    // e.g. check a+b < c+d < e+f
    JetExpr* checkExpr = expr->right; // now use checkExpr below
    JetExpr* lhsExpr = checkExpr->left;
    JetExpr* rhsExpr = checkExpr->right;
    outln("{");
    if (!checkExpr->unary) {
        printf("%.*s%s _lhs = ", level, spaces, JetExpr_typeName(lhsExpr));
        JetExpr_emit(lhsExpr, 0);
        outln(";");
    }
    printf("%.*s%s _rhs = ", level, spaces, JetExpr_typeName(rhsExpr));
    JetExpr_emit(rhsExpr, 0);
    outln(";");
    printf("%.*sif (!(", level, spaces);
    // ----- use lhs rhs cached values instead of the expression
    JetExpr_emit(checkExpr, 0);
    // how are you doing to deal with x < y < z? Repeat all the logic of
    // JetExpr_emit?
    // ----------------------------------------------------------------
    // if (checkExpr->unary) {
    //     outl("_rhs");
    // } else {
    //     outl("_lhs %s _rhs");
    // }
    // -------------
    outln(")) {");
    printf("%.*sprintf(\"\\n\\n\e[31mruntime error:\e[0m check "
           "failed at \e[36m./%%s:%d:%d:\e[0m\\n    %%s\\n\\n\",\n     "
           "       "
           "   THISFILE, \"",
        level + STEP, spaces, expr->line, expr->col + 6);
    JetExpr_write(checkExpr, 0, true, true);
    outln("\");");
    printf("#ifdef DEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

    JetExpr_genPrintVars(checkExpr, level + STEP);
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
                JetExpr_write(lhsExpr, 0, true, true);
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
            JetExpr_write(rhsExpr, 0, true, true);
            printf("%s", "\", _rhs);\n");
        }
    }

    JetExpr_unmarkVisited(checkExpr);

    printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
    printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
    printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
}

/// This should be a standard dispatcher that does nothing except the
/// actual dispatching (via a function pointer table, not a switch).
static void JetExpr_emit(JetExpr* expr, int level) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("%.*s", level, spaces);
    switch (expr->kind) {
    case tkNumber: JetExpr_emit_tkNumber(expr, level); break;

    case tkKeyword_no: outl("no"); break;
    case tkKeyword_yes: outl("yes"); break;
    case tkKeyword_nil: outl("nil"); break;

    case tkMultiDotNumber:
    case tkIdentifier: printf("%s", expr->string); break;

    case tkString: // TODO: parse vars inside, escape stuff, etc.
        JetExpr_emit_tkString(expr, level);
        // printf(escStrings ? "\\%s\\\"" : "%s\"", expr->string);
        break;

    case tkIdentifierResolved: printf("%s", expr->var->name); break;

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
        JetExpr_emit_tkFunctionCallResolved(expr, level);
        break;

    case tkSubscript:
        unreachable("unresolved subscript on '%s'\n", expr->string);
        break;

    case tkSubscriptResolved:
        JetExpr_emit_tkSubscriptResolved(expr, level);
        break;

    case tkOpAssign:
    case tkOpPlusEq:
    case tkOpMinusEq:
    case tkOpTimesEq:
    case tkOpSlashEq:
    case tkOpPowerEq:
    case tkOpModEq:

        switch (expr->left->kind) {
        case tkSubscriptResolved:
            switch (expr->left->left->kind) {
            case tkNumber:
            case tkString:
            case tkRawString:
                // TODO: astexpr_typename should return Array_Scalar or
                // Tensor2D_Scalar or Dict_String_Scalar etc.
                printf("%s_set(%s, %s,%s, ", JetExpr_typeName(expr->left),
                    expr->left->var->name, expr->left->left->string,
                    TokenKind_srepr[expr->kind]);
                JetExpr_emit(expr->right, 0);
                outl(")");
                break;

            case tkOpColon:
                printf("%s_setSlice(%s, ", JetExpr_typeName(expr->left),
                    expr->left->var->name);
                JetExpr_emit(expr->left->left, 0);
                printf(",%s, ", TokenKind_srepr[expr->kind]);
                JetExpr_emit(expr->right, 0);
                outl(")");
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
                printf("%s_setFiltered(%s, ", JetExpr_typeName(expr->left),
                    expr->left->var->name);
                JetExpr_emit(expr->left->left, 0);
                printf(",%s, ", TokenKind_srepr[expr->kind]);
                JetExpr_emit(expr->right, 0);
                outl(")");
                break;

            case tkOpComma:
                // figure out the type of each element
                // there should be a RangeND just like TensorND and
                // SliceND then you can just pass that to _setSlice
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
                unreachable("%s\n", TokenKind_names[expr->left->kind]);
                assert(0);
            }
            break;
        case tkIdentifierResolved:
        case tkPeriod:
            JetExpr_emit(expr->left, 0);
            printf("%s", TokenKind_srepr[expr->kind]);
            JetExpr_emit(expr->right, 0);
            break;
        case tkIdentifier:
            unreachable("unresolved var %s", expr->left->string);
            break;
        case tkArgumentLabel:
            // assert(inFuncArgs);
            JetExpr_emit(expr->right, 0);
            // function call arg label, do not generate ->left
            break;
        case tkString: break;
        default:
            // error: not a valid lvalue
            // TODO: you should at some point e,g, during resolution
            // check for assignments to invalid lvalues and raise an
            // error
            unreachable(
                "found token kind %s\n", TokenKind_names[expr->left->kind]);
        }
        // if (! inFuncArgs) {
        //     JetExpr_emit(self->left, 0,
        //     escStrings); printf("%s", TokenKind_repr(tkOpAssign,
        //     spacing));
        // }
        // JetExpr_emit(self->right, 0,     escStrings);
        // check various types of lhs  here, eg arr[9:87] = 0,
        // map["uuyt"]="hello" etc.
        break;

    case tkArrayOpen:
        // TODO: send parent JetExpr* as an arg to this function. Then
        // here do various things based on whether parent is a =,
        // funcCall, etc.
        if (!expr->right) {
            printf("Array_init(%s)()", "double");
        } else {
            printf("Array_make(((%s[]) {", "double"); // FIXME
            // TODO: MKARR should be different based on the
            // CollectionType of the var or arg in question, eg stack
            // cArray, heap allocated Array, etc.
            JetExpr_emit(expr->right, 0);
            outl("})");
            printf(", %d)", JetExpr_countCommaList(expr->right));
        }
        break;

    case tkBraceOpen: {
        const char* Ktype = "CString";
        const char* Vtype = "Real64";
        if (!expr->right)
            printf("Dict_init(%s,%s)()", Ktype, Vtype); // FIXME
        else {
            printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
                JetExpr_countCommaList(expr->right), Ktype); // FIXME

            JetExpr* p = expr->right;
            while (p && p->kind == tkOpComma) {
                JetExpr_emit(p->left->left, 0);
                outl(", ");
                p = p->right;
            };
            JetExpr_emit(p->left, 0);
            printf("}, (%s[]){", Vtype);
            p = expr->right;
            while (p && p->kind == tkOpComma) {
                JetExpr_emit(p->left->right, 0);
                outl(", ");
                p = p->right;
            };
            JetExpr_emit(p->right, 0);
            outl("})");
        }
    } break;

    case tkOpColon: // convert 3:4:5 to range(...)
                    // must do bounds check first!
        printf(
            "%s(", expr->left->kind != tkOpColon ? "range_to" : "range_to_by");
        if (expr->left->kind == tkOpColon) {
            expr->left->kind = tkOpComma;
            JetExpr_emit(expr->left, 0);
            expr->left->kind = tkOpColon;
        } else
            JetExpr_emit(expr->left, 0);
        outl(", ");
        JetExpr_emit(expr->right, 0);
        outl(")");
        break;

    case tkVarAssign: // basically a tkOpAssign corresponding to a local
                      // var
        // var x as XYZ = abc... -> becomes an JetVar and an
        // JetExpr (to keep location). Send it to JetVar::gen.
        if (expr->var->init != NULL && expr->var->used) {
            JetVar_emit(expr->var, 0, !expr->var->isVar);
            outl(" = "); //, expr->var->name);
            JetExpr_emit(expr->var->init, 0);
        } else {
            printf("/* %s %s at line %d */", expr->var->name,
                expr->var->used ? "null" : "unused", expr->line);
        }
        break;

    case tkKeyword_else:
        outln("else {");
        if (expr->body) JetScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_elif:
        outln("else if (");
        JetExpr_emit(expr->left, 0);
        outln(") {");
        if (expr->body) JetScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_match: {
        // char* typeName = JetExpr_typeName(expr->left);
        // if (!typeName)
        //     unreachable(
        //         "unresolved type during emit at %d:%d", expr->line,
        //         expr->col);
        // if (expr->left->typeType == TYObject)
        //     typeName = JetExpr_typeName(expr->left);
        printf("{%s __match_cond = ", JetExpr_typeName(expr->left));
        JetExpr_emit(expr->left, 0);
        if (expr->left->typeType > TYInt8
            || (expr->left->typeType == TYObject
                && JetExpr_getObjectType(expr->left)->isEnum))
            outln("; switch (__match_cond) {");
        else
            outln("; { if (0) {}"); // the case will add 'else if's
        // outln(") {");
        if (expr->body) JetScope_emit(expr->body, level);
        printf("%.*s}}", level, spaces);
        break;
    }

        /*
        This is how you walk a JetExpr that is a tkOpComma (left to right):
            process(cond->left);
            while (cond->right->kind == tkOpComma)
                cond = cond->right, process(cond->left);
            process(cond->right);
        */

        // void pro(JetExpr * c) { }
        // TODO: generally all comma exprs should be handled like this
        // iteratively. What if you have a large array with lots of items?
        // recursion will blow the stack
    case tkKeyword_case: {
        // TODO: maybe make this a macro
        JetExpr* cond = expr->left;
        if (cond->kind == tkOpComma) {
            if (cond->typeType > TYInt8
                || (cond->typeType == TYObject && JetExpr_getEnumType(cond))) {
                // match has handled the cond with a 'switch'
                outl("case "), JetExpr_emit(cond->left, 0), outl(": ");
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    outl("case "), JetExpr_emit(cond->left, 0), outl(": ");
                }
                outl("case "), JetExpr_emit(cond->right, 0), outln(": {");
            } else if (cond->typeType == TYString) {
                outl("else if (!strcmp(__match_cond, ");
                JetExpr_emit(cond->left, 0);
                outl(")");
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    outl(" || !strcmp(__match_cond, ");
                    JetExpr_emit(cond->left, 0), outl(")");
                }
                outl(" || !strcmp(__match_cond, ");
                JetExpr_emit(cond->right, 0), outln(")) do {");
            } else {
                outl("else if (__match_cond == ");
                JetExpr_emit(cond->left, 0);
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    outl(" || __match_cond == ("), JetExpr_emit(cond->left, 0);
                }
                JetExpr_emit(cond->right, 0);
                outln(")) do {");
            };

        } else {
            if (cond->typeType > TYInt8
                || (cond->typeType == TYObject && JetExpr_getEnumType(cond))) {
                outl("case "); // match has handled the cond with a 'switch'
                JetExpr_emit(cond, 0);
                outln(": {");
            } else if (cond->typeType == TYString) {
                outl("else if (!strcmp(__match_cond, ");
                JetExpr_emit(cond, 0);
                outln(")) do {");
            } else {
                outl("else if (__match_cond == (");
                JetExpr_emit(cond, 0);
                outln(")) do {");
            };
        };
        if (expr->body) JetScope_emit(expr->body, level);
        printf("%.*s}", level, spaces);
        if (cond->typeType > TYInt8
            || (cond->typeType == TYObject && JetExpr_getEnumType(cond)))
            outl(" break");
        else
            outl(" while(0)");
        break;
    }
    case tkKeyword_for:
    case tkKeyword_if:
        //    case tkKeyword_elif:
        //    case tkKeyword_else:
    case tkKeyword_while:
        if (expr->kind == tkKeyword_for)
            outl("FOR(");
        else
            printf("%s (", TokenKind_repr[expr->kind]);
        if (expr->kind == tkKeyword_for) expr->left->kind = tkOpComma;
        if (expr->left) JetExpr_emit(expr->left, 0);
        if (expr->kind == tkKeyword_for) expr->left->kind = tkOpAssign;
        outln(") {");
        if (expr->body) JetScope_emit(expr->body, level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkOpPower:
        outl("pow(");
        JetExpr_emit(expr->left, 0);
        outl(",");
        JetExpr_emit(expr->right, 0);
        outl(")");
        break;

    case tkKeyword_return:
        outl("{_err_ = NULL; STACKDEPTH_DOWN; return ");
        if (expr->right) JetExpr_emit(expr->right, 0);
        outln(";}");
        break;

    case tkKeyword_check: JetExpr_emit_tkCheck(expr, 0); break;

    case tkPeriod:
        JetExpr_emit(expr->left, 0);
        if (expr->left->typeType == TYObject
            && JetExpr_getObjectType(expr->left)->isEnum)
            outl("_");
        else
            outl("->"); // may be . if right is embedded and not a
                        // reference
        JetExpr_emit(expr->right, 0);
        break;

    case tkKeyword_notin: outl("!"); fallthrough;
    case tkKeyword_in:
        // the RHS should be dims==1 or another kind of collection, you should
        // have checked it in the analysis phase.
        switch (expr->right->kind) {
        case tkArrayOpen:
            if (expr->right->right->kind == tkOpColon)
                goto inRangeOp; // x in [a:b]
            // now its a literal array. that makes it easy, you can either call
            // isin() or the macro ISIN() if you have relatively few items in
            // the array.

            {
                int c = JetExpr_countCommaList(expr->right->right);
                // if (c <= 64) {
                // TODO: ISIN/isin must be specialized for types other than
                // int. in particular strings cannot be used yet
                printf("%s(%d, ", c <= 64 ? "ISIN" : "isin", c);
                JetExpr_emit(expr->left, 0);
                outl(", ");
                JetExpr_emit(expr->right->right, 0);
                outl(")");
                // }
            }

            break;
        case tkSubscript:
            // maybe slice or something
            break;
        case tkOpColon: // x in a:b
        inRangeOp:
            break;
        default:
            unreachable("inside in operator: rhs is %s",
                TokenKind_repr[expr->right->kind]);
            // for anything else, figure it out.
        }
        break;

    case tkOpEQ:
    case tkOpNE:
    case tkOpGE:
    case tkOpLE:
    case tkOpGT:
    case tkOpLT:
        if ((expr->kind == tkOpLE || expr->kind == tkOpLT)
            && (expr->left->kind == tkOpLE | expr->left->kind == tkOpLT)) {
            printf("%s_cmp3way_%s_%s(", JetExpr_typeName(expr->left->right),
                TokenKind_ascrepr(expr->kind, false),
                TokenKind_ascrepr(expr->left->kind, false));
            JetExpr_emit(expr->left->left, 0);
            outl(", ");
            JetExpr_emit(expr->left->right, 0);
            outl(", ");
            JetExpr_emit(expr->right, 0);
            outl(")");
            break;
        } else if (expr->right->typeType == TYString) {
            printf("CString_cmp(%s, ", TokenKind_srepr[expr->kind]);
            JetExpr_emit(expr->left, 0);
            outl(", ");
            JetExpr_emit(expr->right, 0);
            outl(")");
            break;
        }
        fallthrough;
    default:
        if (!expr->prec) break;
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
        if (expr->left) JetExpr_emit(expr->left, 0);
        if (leftBr) putc(lpc, stdout);

        if (expr->kind == tkArrayOpen)
            putc('{', stdout);
        else
            printf("%s", TokenKind_srepr[expr->kind]);

        char rpo = '(';
        char rpc = ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right) JetExpr_emit(expr->right, 0);
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
//     "    system(\"paste -d ' ' .\" THISFILE \"r \" THISFILE \" > \" "
//     "THISFILE "
//     "\"r\" );"
//     "}\n"
//     "static void lineprofile_begin() {_lprof_last_=getticks();}\n"
// };
// TODO: why do you need to pass level here?

void JetType_genTypeInfoDecls(JetType* type);
void JetType_genTypeInfoDefs(JetType* type);
void JetType_genNameAccessors(JetType* type);
static void JetModule_emit(JetModule* module) {
    // outln("");
    printf("#ifndef HAVE_%s\n#define HAVE_%s\n\n", module->name, module->name);
    printf("#define THISMODULE %s\n", module->name);

    foreach (JetImport*, import, module->imports)
        JetImport_emit(import, 0);

    outln("");

    foreach (JetVar*, var, module->scope->locals)
        if (var->used) JetVar_genh(var, 0);

    foreach (JetType*, type, module->enums) {
        if (type->body && type->analysed) {
            JetEnum_genh(type, 0);
            // JetType_genTypeInfoDecls(type);
        }
    }

    foreach (JetType*, type, module->types) {
        if (type->body && type->analysed) {
            JetType_genh(type, 0);
            JetType_genTypeInfoDecls(type);
        }
    }
    foreach (JetFunc*, func, module->funcs) {
        if (func->body && func->analysed) { JetFunc_genh(func, 0); }
    }

    foreach (JetType*, type, module->types) {
        if (type->body && type->analysed) {
            // foreach (JetExpr*, expr, type->body->stmts)
            //     JetExpr_prepareInterp(expr, type->body);
            // ^ MOVE THIS INTO JetType_emit
            JetType_emit(type, 0);
            JetType_genTypeInfoDefs(type);
            JetType_genNameAccessors(type);
        }
    }
    foreach (JetFunc*, func, module->funcs) {
        if (func->body && func->analysed) {
            // foreach (JetExpr*, expr, func->body->stmts) {
            //     JetExpr_prepareInterp(parser,expr, func->body);
            // }
            // ^ MOVE THIS INTO JetFunc_emit
            JetFunc_emit(func, 0);
        }
    }
    foreach (JetImport*, import, module->imports)
        JetImport_undefc(import);

    // puts(coverageFunc[genCoverage]);
    // puts(lineProfileFunc[genLineProfile]);

    printf("#undef THISMODULE\n");
    printf("#endif // HAVE_%s\n", module->name);
}

static void JetModule_genTests(JetModule* module) {
    JetModule_emit(module);
    foreach (JetTest*, test, module->tests)
        JetTest_emit(test);
    // generate a func that main will call
    printf("\nvoid tests_run_%s() {\n", module->name);
    foreach (JetTest*, test, module->tests)
        printf("    test_%s();\n", test->name);
    outln("}");
}

// Generates a couple of functions that allow setting an integral member
// of a type at runtime by name, or getting a pointer to a member by
// name.
void JetType_genNameAccessors(JetType* type) {
    // TODO: instead of a linear search over all members this should
    // generate a switch for checking using a prefix tree -> see
    // genrec.c
    if (!type->analysed || type->isDeclare) return;
    // JetType_genMemberRecognizer( type, "Int64 value",  )

    printf("static void* %s__memberNamed(%s self, const char* name) {\n",
        type->name, type->name);

    // TODO: skip bitfield members in this loop or it wont compile
    foreach (JetVar*, var, type->body->locals) /*if (var->used) */ //
        printf("    if (CString_equals(name, \"%s\")) return "
               "&(self->%s);\n",
            var->name, var->name);
    outln("    return NULL;\n}");

    // this func sets bools or ints that may be part of bitfields
    printf("static void %s__setMemberNamed(%s self, const char* name, "
           "Int64 "
           "value) {\n",
        type->name, type->name);
    foreach (JetVar*, var, type->body->locals) // if (var->used) //
        if (var->spec->typeType >= TYBool && var->spec->typeType <= TYReal64)
            printf("    if (CString_equals(name, \"%s\"))  {self->%s = "
                   "*(%s*) "
                   "&value;return;}\n",
                var->name, var->name, JetTypeSpec_cname(var->spec));
    outln("}");
}

// Generates some per-type functions that write out meta info of the
// type to be used for reflection, serialization, etc.
void JetType_genTypeInfoDecls(JetType* type) {
    if (!type->analysed || type->isDeclare) return;

    printf("static const char* const %s__memberNames[] = {\n    ", type->name);
    if (type->body) //
        foreachn(JetVar*, var, varn, type->body->locals) {
            if (!var || !var->used) continue;
            printf("\"%s\", ", var->name);
            // JetVar_emit(var, level + STEP, false);
        }
    outln("};");
}

void JetType_genTypeInfoDefs(JetType* type) {
    // printf("static const char* const %s__memberNames[] = {\n",
    // type->name); foreachn(JetVar*, var, varn, type->body->locals)
    // {
    //     if (! var) continue;
    //     printf("\"%s\",\n", var->name);
    //     // JetVar_emit(var, level + STEP, false);
    //     outln("}; \\");
    // }
}
