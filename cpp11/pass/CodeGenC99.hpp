const char* functionExitStuff_UNESCAPED
  = "    return DEFAULT_VALUE;\n"
    "uncaught: HANDLE_UNCAUGHT;\n"
    "backtrace: SHOW_BACKTRACE_LINE;\n"
    "return_: STACKDEPTH_DOWN;\n"
    "    return DEFAULT_VALUE;";
const char* functionEntryStuff_UNESCAPED
  = "    STACKDEPTH_UP; DO_STACK_CHECK;\n";

#define genLineNumbers 0
#define genCoverage 1
#define genLineProfile 1

#include <cstdio>
#include <cstring>

struct CodeGenC99 {

  ///////////////////////////////////////////////////////////////////////////
  void emit(Import& import, int level) {
    char* alias = import.aliasOffset + import.name;
    CString_tr_ip(import.name, '.', '_', 0);
    printf("\n#include \"%s.h\"\n", import.name);
    if (alias) printf("#define %s %s\n", alias, import.name);
    // TODO: remove #defines! There could be a field of any
    // struct with the same name that will get clobbered
    CString_tr_ip(import.name, '_', '.', 0);
  }

  ///////////////////////////////////////////////////////////////////////////
  void undefc(Import& import) {
    // if (import.alias) printf("#undef %s\n", import.alias);
  }

  ///////////////////////////////////////////////////////////////////////////
  void emit(TypeInfo typeInfo, int level, bool isconst) {
    if (isconst) printf("const ");
    // TODO: actually this depends on the collectionType. In general
    // Array is the default, but in other cases it may be SArray, Array64,
    // whatever
    if (typeInfo.dims) {
      if (typeInfo.dims > 1)
        // TODO: this should be TensorND, without type params?
        // well actually there isn't a TensorND, since its not always
        // double thats in a tensor but can be Complex, Range,
        // Reciprocal, Rational, whatever
        // -- sure, but double (and float) should be enough since
        // the other types are rarely needed in a tensor form
        printf("SArray%dD(", typeInfo.dims);
      else
        printf("SArray(");
    }

    switch (typeInfo.typeType) {
    case TYObject:
      // objects are always T* const, if meant to be r/o they are
      // const T* const. Later we may have a byval flag to embed structs
      // or pass around by value.
      // leaving it as is for now
      printf("%s", typeInfo.type.name);
      break;
    case TYUnresolved:
      unreachable("unresolved: '%s' at %d:%d", typeInfo.name, typeInfo.line,
        typeInfo.col);
      printf("%s", *typeInfo.name ? typeInfo.name : "Error_Type");
      break;
    default: printf("%s", TypeType_name(typeInfo.typeType)); break;
    }

    //     if (isconst ) printf(" const"); // only if a ptr type
    if (typeInfo.dims /*or typeInfo.typeType == TYObject*/)
      printf("%s", ")");
    //        if (status == TSDimensionedNumber) {
    //            genc(units, level);
    //        }
  }

  void emit(Expr& expr, int level);

  ///////////////////////////////////////////////////////////////////////////
  void emit(Var& var, int level, bool isconst) {
    // for C the variables go at the top of the block, without init
    printf("%.*s", level, spaces);
    if (var.typeInfo) emit(var.typeInfo, level + STEP, isconst);
    printf(" %s", var.name);
  }

  ///////////////////////////////////////////////////////////////////////////
  // Functions like Array_any_filter, Array_count_filter etc.
  // are macros and don't return a value but may set one. For these
  // and other such funcs, the call must be moved to before the
  // containing statement, and in place of the original call you
  // should place a temporary holding the value that would have been
  // "returned".
  bool mustPromote(const char* name) {
    // TODO: at some point these should go into a dict or trie or MPH
    // whatever
    if (not strcmp(name, "Array_any_filter")) return true;
    if (not strcmp(name, "Array_all_filter")) return true;
    if (not strcmp(name, "Array_count_filter")) return true;
    if (not strcmp(name, "Array_write_filter")) return true;
    if (not strcmp(name, "Strs_print_filter")) return true;
    return false;
  }

  ///////////////////////////////////////////////////////////////////////////
  void unmarkVisited(Expr& expr) {
    switch (expr.kind) {
    case tkIdentifierResolved:
    case tkVarAssign: expr.var->visited = false; break;
    case tkFunctionCallResolved:
    case tkFunctionCall: // shouldnt happen
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_while: unmarkVisited(expr.left); break;
    default:
      if (expr.prec) {
        if (not expr.unary) unmarkVisited(expr.left);
        unmarkVisited(expr.right);
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
  void genPrintVars(Expr& expr, int level) {
    // assert(expr);
    // what about func args?
    switch (expr.kind) {
    case tkIdentifierResolved:
    case tkVarAssign:
      if (expr.var->visited) break;
      printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level, spaces,
        expr.var->name, TypeType_format(expr.typeType, true),
        expr.var->name);
      expr.var->visited = true;
      break;

    case tkPeriod:
      //        {
      //            Expr& e = expr.right;
      //            while (e->kind==tkPeriod) e=e->right;
      ////            if (e->var.visited) break;
      //            printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level,
      //            spaces,
      //                   expr.var->name, TypeType_format(e->typeType,
      //                   true), expr.var->name);
      //        }
      break;

    case tkFunctionCallResolved:
    case tkFunctionCall: // shouldnt happen
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_else:
    case tkKeyword_for:
    case tkKeyword_while: genPrintVars(expr.left, level); break;

    default:
      if (expr.prec) {
        if (not expr.unary) genPrintVars(expr.left, level);
        genPrintVars(expr.right, level);
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Promotion scan & promotion happens AFTER resolving functions!
  Expr& findPromotionCandidate(Expr& expr) {
    assert(expr);
    Expr& ret;

    // what about func args?
    switch (expr.kind) {
    case tkFunctionCallResolved:
      // promote innermost first, so check args
      if (expr.left and (ret = findPromotionCandidate(expr.left)))
        return ret;
      else if (mustPromote(expr.func.selector))
        return expr;
      break;

    case tkSubscriptResolved:
      // TODO: here see if the subscript itself needs to be promoted up
      return findPromotionCandidate(expr.left);

    case tkSubscript: return findPromotionCandidate(expr.left);

    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_while:
      if (expr.left) return findPromotionCandidate(*expr.left);
      // body will be handled by parent scope

    case tkVarAssign:
      if ((ret = findPromotionCandidate(*expr.var->init))) return ret;
      break;

    case tkFunctionCall: // unresolved
      // assert(0);
      unreachable("unresolved call %s\n", expr.string);
      if ((ret = findPromotionCandidate(*expr.left))) return ret;
      break;

    default:
      if (expr.prec) {
        if (expr.right and (ret = findPromotionCandidate(*expr.right)))
          return ret;
        if (not expr.unary)
          if ((ret = findPromotionCandidate(*expr.left))) return ret;
      }
    }
    return NULL;
  }

  char* newTmpVarName(int num, char c) {
    char buf[8];
    int l = snprintf(buf, 8, "_%c%d", c, num);
    return CString_pndup(buf, l);
  }

  ///////////////////////////////////////////////////////////////////////////
  bool isCtrlExpr(Expr& expr) {
    return expr.kind == tkKeyword_if  //
      or expr.kind == tkKeyword_for   //
      or expr.kind == tkKeyword_while //
      or expr.kind == tkKeyword_else;
  }

  bool isLiteralExpr(Expr& expr) { return false; }
  bool isComparatorExpr(Expr& expr) { return false; }

  ///////////////////////////////////////////////////////////////////////////
  void lowerElementalOps(Scope& scope) {
    for (Expr &stmt, scope->stmts) {

      if (isCtrlExpr(stmt) and stmt.body) lowerElementalOps(stmt.body);

      if (not stmt.elemental) continue;

      // wrap it in an empty block (or use if true)
      Expr& ifblk = NEW(ASTExpr);
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
  void promoteCandidates(Scope& scope) {
    int tmpCount = 0;
    Expr& pc = NULL;
    List(ASTExpr)* prev = NULL;
    for (Expr &stmt, stmts, scope->stmts) {
      // TODO:
      // if (not  stmt.promote) {prev=stmts;continue;}

      if (isCtrlExpr(stmt) and stmt.body) promoteCandidates(stmt.body);

    startloop:

      if (not(pc = findPromotionCandidate(stmt))) { // most likely
        prev = stmts;
        continue;
      }
      if (pc == stmt) {
        // possible, less likely: stmt already at toplevel.
        // TODO: in this case, you still have to add the extra arg.
        prev = stmts;
        continue;
      }

      Expr& pcClone = NEW(ASTExpr);
      *pcClone = *pc;

      // 1. add a temp var to the scope
      Var& tmpvar = NEW(ASTVar);
      tmpvar.name = newTmpVarName(++tmpCount, 'p');
      tmpvar.typeInfo = NEW(ASTTypeSpec);
      //        tmpvar.typeInfo.typeType = TYReal64; // FIXME
      // TODO: setup tmpvar.typeInfo
      PtrList_append(&scope->vars, tmpvar);

      // 2. change the original to an ident
      pc->kind = tkIdentifierResolved;
      pc->prec = 0;
      pc->var = tmpvar;

      // 3. insert the tmp var as an additional argument into the call

      if (not pcClone->left)
        pcClone->left = pc;
      else if (pcClone->left->kind != tkOpComma) {
        // single arg
        Expr& com = NEW(ASTExpr);
        // TODO: really should have an astexpr ctor
        com->prec = TokenKind_getPrecedence(tkOpComma);
        com->kind = tkOpComma;
        com->left = pcClone->left;
        com->right = pc;
        pcClone->left = com;
      } else {
        Expr& argn = pcClone->left;
        while (argn->kind == tkOpComma and argn->right->kind == tkOpComma)
          argn = argn->right;
        Expr& com = NEW(ASTExpr);
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
      if (not prev) {
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
  void emit(Scope& scope, int level) {
    for (Var &local, scope->vars)
      if (local->used) {
        emit(local, level, false);
        puts(";");
      } // these will be declared at top and defined within the expr list
    for (Expr &stmt, scope->stmts) {
      if (stmt.kind == tkLineComment) continue;

      if (genLineNumbers) printf("#line %d\n", stmt.line);
      // if (genCoverage) printf("    _cov_[%d]++;\n", stmt.line - 1);
      // if (genLineProfile) {
      //     printf("    _lprof_tmp_ = getticks();\n");
      //     printf("    _lprof_[%d] +=
      //     (_lprof_tmp_-_lprof_last_)/100;\n",
      //         stmt.line - 1);
      //     printf("    _lprof_last_ = _lprof_tmp_;\n");
      // }

      // You need to know if the emit will in fact generate
      // something. This is true in general unless it is an unused var
      // init.
      if (stmt.kind != tkVarAssign or stmt.var.used) {

        // if (genCoverage or genLineProfile) //
        //     printf("    /************/ ");
        if (genCoverage)
          printf("%.*sJET_COVERAGE_UP(%d); \n", level, spaces, stmt.line);
        if (genLineProfile) {
          // printf("    _lprof_tmp_ = getticks();\n");
          printf("%.*sJET_PROFILE_LINE(%d);\n", level, spaces, stmt.line);
          // printf("    _lprof_last_ = _lprof_tmp_;\n");
        }
        if (genCoverage or genLineProfile) puts(""); // ************/");
      }

      emit(stmt, level);
      if (not isCtrlExpr(stmt) and stmt.kind != tkKeyword_return)
        puts(";");
      else
        puts("");
      // convert this into a flag which is set in the resolution pass

      // here you see if any vars are to be dropped at this point (because
      // they were last used in this expr. A var can only be dropped in
      // its own scope, not an inner or outer scope, so just scan our own
      // vars. In a sense the stmt.line is a local ID for the statement
      // within the scope.
      Scope& sco = scope;
      do {
        for (Var &var, sco->vars)
          if (var.used) {
            // if (var.lastUsage)
            //     printf("%.*s//-- %s %d %d\n", level, spaces,
            //     var.name,
            //         var.lastUsage, stmt.line);
            if (var.lastUsage == stmt.line) {
              printf("%.*sDROP(%s,%s,%s,%s);\n", level, spaces,
                name(var.typeInfo), var.name,
                CollectionType_nativeName(var.typeInfo.collectionType),
                StorageClassNames[var.storage]);
              // TODO^ distinguish between stack, heap, mixed,
              // refcounted drops
              var.lastUsage = 0; // this means var has been dropped.
            }
          }
      } while (not sco->isLoop  // if loop scope, don't walk up
        and (sco = sco->parent) // walk up to the last loop scope
        and sco->parent and !sco->parent->isLoop);
      // all scopes have the global scope as the final parent.
      // what if some scope tries to drop something from there?

      // TODO:
      // Maybe scope should have a lineno. If a loop scope has the
      // lastUsage of a parent var, it cannot drop it inside the loop, but
      // it should be done just after the loop ends. Now it will be
      // dropped anyway at owning scope end which may be suboptimal. If
      // you have the scope line, which is the line of the cond expr of if
      // / while etc., you change the lastUsage to that line and it gets
      // dropped just after the scope.

      if (throws(stmt)) printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
    }
    // It's possible some vars were not detected in inner scopes and
    // dropped. So let's drop them here. No need to walk up the parent chain
    // here.
    for (Var &var, scope->vars)
      if (var.used and var.lastUsage)
        printf("%.*sdrop(%s); // anyway\n", level, spaces, var.name);
    // ^ these are all the vars whose lastUsage could not be matched. This
    // may be because they are in an inner scope. In this case they should
    // be dropped at the end of the scope. Optimize this later so that if
    // there are multiple subscopes and the last usage is in one of them,
    // the drop happens after that subscope and doesn't wait until the very
    // end.
  }

  ///////////////////////////////////////////////////////////////////////////
  void genJson(Type& type) {
    printf(
      "  void %s_json_(const %s self, int nspc) {\n", type.name, type.name);

    printf("    printf(\"{\\n\");\n");
    // printf("    printf(\"\\\"_type_\\\": \\\"%s\\\"\");\n", type.name);
    // if (type.body->vars) printf("    printf(\",\\n\");\n");

    // TODO: move this part into its own func so that subclasses can ask the
    // superclass to add in their fields inline
    for (Var& var : type.body->vars) {
      if (not var /*or not var.used*/) continue;
      printf(
        "    printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n", var.name);
      const char* valueType = typeName(var.init);
      printf("    %s_json_(self->%s, nspc+4);\n    printf(\"", valueType,
        var.name);
      if (vars->next) printf(",");
      printf("\\n\");\n");
    }
    printf("    printf(\"%%.*s}\", nspc, _spaces_);\n");
    printf("}\nMAKE_json_wrap_(%s)\n//MAKE_json_file(%s)\n", type.name,
      type.name);
  }

  ///////////////////////////////////////////////////////////////////////////
  void genJsonReader(Type& type) { }

  const char* functionEntryStuff_UNESCAPED
    = "    STACKDEPTH_UP; DO_STACK_CHECK;\n";

  const char* functionExitStuff_UNESCAPED
    = "    return DEFAULT_VALUE;\n"
      "uncaught: HANDLE_UNCAUGHT;\n"
      "backtrace: SHOW_BACKTRACE_LINE;\n"
      "return_: STACKDEPTH_DOWN;\n"
      "    return DEFAULT_VALUE;";

  ///////////////////////////////////////////////////////////////////////////
  void printStackUsageDef(size_t stackUsage) {
    printf("#define MYSTACKUSAGE (%lu + 6*sizeof(void*) + "
           "IFDEBUGELSE(sizeof(char*),0))\n",
      stackUsage);
  }

  ///////////////////////////////////////////////////////////////////////////
  void emit(Type& type, int level) {
    if (not type.body or not type.analysed or type.isDeclare) return;
    // if (not  type.body or not type.analysed) return;
    const char* const name = type.name;
    printf("#define FIELDS_%s \\\n", name);
    for (Var &var, type.body->vars) {
      // if (not var /*or not var.used*/) continue;
      // It's not so easy to just skip 'unused' type members.
      // what if I just construct an object and print it?
      // I expect to see the default members. But if they
      // haven't been otherwise accessed, they are left out.
      emit(var, level + STEP, false);
      printf("; \\\n");
    }
    printf("\n\nstruct %s {\n", name);

    if (type.super) {
      printf("    FIELDS_");
      emit(type.super, level, false);
      printf("\n");
    }

    printf("    FIELDS_%s\n};\n\n", name);
    printf("  const char* %s_name_ = \"%s\";\n\n", name, name);
    printf("  %s %s_alloc_() {\n    return _Pool_alloc_(&gPool_, "
           "sizeof(struct %s));\n}\n\n",
      name, name, name);
    printf("  %s %s_init_(%s self) {\n", name, name, name);

    for (Var& var : type.body->vars) // if (var.used)
      printf("#define %s self->%s\n", var.name, var.name);

    for (Expr& stmt : type.body->stmts) {
      if (stmt.kind != tkVarAssign or not stmt.var->init) continue;
      // or not stmt.var.used)
      printf("%.*s%s = ", level + STEP, spaces, stmt.var.name);
      emit(stmt.var->init, 0);
      puts(";");
      if (throws(stmt.var->init))
        puts("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    for (Var& var : type.body->vars) // if (var.used)
      printf("#undef %s \n", var.name);

    printf("    return self;\n}\n\n");

    printStackUsageDef(48);
    printf("#define DEFAULT_VALUE NULL\n"
           "monostatic %s %s_new_(IFDEBUG(const char* callsite_)) {\n"
           "IFDEBUG(  const char* sig_ = \"%s()\");\n",
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
           "'%%s' at %%p size %%luB>\\n\",name, self, sizeof(struct "
           "%s));\n}\n",
      name, name, name, name);
    puts("");

    genJson(type);
    genJsonReader(type);
  }

  ///////////////////////////////////////////////////////////////////////////
  void genh(Type& type, int level) {
    if (not type.body or not type.analysed or type.external) return;

    const char* const name = type.name;
    printf("typedef struct %s* %s;\nstruct %s;\n", name, name, name);
    printf("  %s %s_alloc_(); \n", name, name);
    printf("  %s %s_init_(%s self);\n", name, name, name);
    printf("%s %s_new_(IFDEBUG(const char* callsite_)); \n", name, name);
    printf("\nDECL_json_wrap_(%s)\n//DECL_json_file(%s)\n", name, name);
    printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
           "%s_json_wrap_(x); }\n\n",
      name, name);
    printf("  void %s_json_(const %s self, int nspc);\n", name, name);
  }
  ///////////////////////////////////////////////////////////////////////////
  void genh(Type& type, int level) {
    if (not type.body or not type.analysed) return;
    const char* const name = type.name;
    puts("typedef enum {");

    for (Var& var : type.body->vars) printf("    %s_%s,\n", name, var.name);
    printf("} %s;\n", name);
    Expr& ex1 = *(type.body->stmts); //->item;
    const char* datType
      = ex1->kind == tkOpAssign ? typeName(ex1->right) : NULL;
    if (datType)
      printf("monostatic %s %s__data[%d];\n", datType, name,
        PtrList_count(type.body->vars));
    printf("monostatic const char* %s__fullnames[] ={\n", name);
    for (Var &var, type.body->vars)
      printf("    \"%s.%s\",\n", name, var.name);
    puts("};");
    printf("monostatic const char* %s__names[] ={\n", name);
    for (Var &var, type.body->vars) printf("    \".%s\",\n", var.name);
    puts("};");
    // printf("monostatic const char* %s__names[%d];\n", name,
    //     PtrList_count(type.body->vars));

    printf("monostatic void %s__init() {\n", name);
    // for (Var& var, type.body->vars) {
    //     printf("    %s__names[%s_%s] =  %s__fullnames[%s_%s] + %zu;\n",
    //     name,
    //         name, var.name, name, name, var.name, cstr_len(name));
    //     // puts("};");
    // }
    for (Expr& stmt : type.body->stmts) {
      if (stmt.kind != tkOpAssign) continue;
      // or not stmt.var->init)
      // //     //            or not stmt.var.used)
      printf("%.*s%s__data[%s_%s] = ", level + STEP, spaces, name, name,
        stmt.left->string);
      emit(stmt.right, 0);
      puts(";");
      if (throws(stmt.right))
        puts("    if (_err_ == ERROR_TRACE) return NULL;");
    }
    puts("}");

    // printf("  %s %s_alloc_(); \n", name, name);
    // printf("%s %s_new_(IFDEBUG(const char* callsite_)); \n", name, name);
    // printf("\nDECL_json_wrap_(%s)\n//DECL_json_file(%s)\n", name, name);
    // printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
    //        "%s_json_wrap_(x); }\n\n",
    //     name, name);
    // printf("  void %s_json_(const %s self, int nspc);\n", name,
    // name);
  }
  ///////////////////////////////////////////////////////////////////////////
  void emit(Func& func, int level) {
    if (not func.body or not func.analysed or func.isDeclare)
      return; // declares, default ctors

    // actual stack usage is higher due to stack protection, frame
    // bookkeeping
    // ...
    size_t stackUsage = calcSizeUsage(func);
    printStackUsageDef(stackUsage);

    printf("#define DEFAULT_VALUE %s\n",
      getDefaultValueForType(func.returnSpec));
    if (not func.isExported) printf("  ");
    if (func.returnSpec) {
      emit(func.returnSpec, level, false);
    } else {
      printf("void");
    }
    printf(" %s(", func.selector);
    for (Var& arg : func.args) {
      emit(arg, level, true);
      printf(args->next ? ", " : "");
    }

    printf("\n#ifdef DEBUG\n"
           "    %c const char* callsite_ "
           "\n#endif\n",
      ((func.args and func.args->item ? ',' : ' ')));

    // TODO: if (flags.throws) printf("const char** _err_");
    puts(") {");
    printf("    IFDEBUG(  const char* sig_ = \"");
    printf("%s%s(", func.isStmt ? "" : "function ", func.name);

    for (Var &arg, args, func.args) {
      lint(arg, level);
      printf(args->next ? ", " : "");
    }
    printf(")");
    if (func.returnSpec) {
      printf(" as ");
      lint(func.returnSpec, level);
    }
    puts("\");");

    puts(functionEntryStuff_UNESCAPED);

    emit(func.body, level + STEP);

    puts(functionExitStuff_UNESCAPED);
    puts("}\n#undef DEFAULT_VALUE");
    puts("#undef MYSTACKUSAGE");
  }

  ///////////////////////////////////////////////////////////////////////////
  void genh(Func& func, int level) {
    if (not func.body or not func.analysed or func.external) return;
    if (not func.exported) printf("  ");
    if (func.returnSpec) {
      emit(func.returnSpec, level, false);
    } else {
      printf("void");
    }
    printf(" %s(", func.selector);
    for (Var& arg : func.args) {
      emit(arg, level, true);
      printf(args->next ? ", " : "");
    }
    printf("\n#ifdef DEBUG\n    %c const char* callsite_\n#endif\n",
      ((func.args and func.args->item) ? ',' : ' '));
    puts(");\n");
  }

  ///////////////////////////////////////////////////////////////////////////
  void genh(Var& var, int level) {
    if (not var.init) return;

    emit(var.typeInfo, level, false);

    printf(" %s = ", var.name);
    emit(var.init, 0);

    puts("");
  }

  ////////////////////////////////////////////////////
  void emit(Test& test) // TODO: should tests not return BOOL?
  {
    if (not test->body) return;
    printf("\nstatic void test_%s() {\n", test->name);
    emit(test->body, STEP);
    puts("}");
  }

  //_____________________________________________________________________________
  /// Emits the equivalent C code for a subscript (that has been resolved to
  /// its corresponding `ASTVariable`). This function does all of the heavy
  /// lifting to decide what the subscript actually does, based on the kind
  /// of the subscript expression, number of dimensions and the context.
  void emit_subscriptResolved(Expr& expr, int level) {
    const char* name = expr.var->name;
    Expr& index = *expr.left;
    // assert(index);
    // index = index->right;
    switch (index.kind) {
    case tkNumber: // indexing with a single number, can be a -ve number
      printf("Array_get_%s(%s, %s)", cname(expr.var->typeInfo), name,
        index.string);
      break;

    case tkString:
    case tkRawString: // indexing with single string or regex
      printf("Dict_get_CString_%s(%s, %s)", cname(expr.var->typeInfo), name,
        index.string);
      break;

    case tkOpComma: // higher dims. validation etc. has been done by this
                    // stage.

      // this is for cases like arr[2, 3, 4].
      printf("Tensor%dD_get_%s(%s, {", expr.var->typeInfo.dims,
        cname(expr.var->typeInfo), name);
      emit(index, 0);
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
      printf("Array_getSlice_%s(%s, ", name(expr.var->typeInfo), name);
      emit(index, 0);
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
      printf("Array_copy_filter_%s(%s, ", name(expr.var->typeInfo), name);
      emit(index, 0);
      printf(")");
      break;

    default:
      unreachable("bad kind: %s", TokenKinds_names[expr.kind]);
      break;
    }
  }

  //_____________________________________________________________________________
  /// Emits the equivalent C code for a function call (that has been
  /// resolved to its corresponding `ASTFunc`). Type constructors call a C
  /// function that has `_new` appended to the type name. This function
  /// passes a constructed string as the extra argument `callsite_` that is
  /// used to generate accurate backtraces.
  void emit_functionCallResolved(Expr& expr, int level) {
    char* tmp = expr.func.selector;

    Expr* arg1 = expr.left;
    const char* tmpc = "";
    if (arg1) {
      if (arg1->kind == tkOpComma) arg1 = arg1->left;
      tmpc = CollectionType_nativeName(arg1->collectionType);
    }
    printf("%s%s", tmpc, tmp);
    if (*tmp >= 'A' and *tmp <= 'Z' and !strchr(tmp, '_')) printf("_new_");
    printf("(");

    if (expr.left) emit(*expr.left, 0);

    if (not expr.func->external) {
      printf("\n#ifdef DEBUG\n"
             "      %c \"./\" THISFILE \":%d:%d:\\e[0m ",
        expr.left ? ',' : ' ', expr.loc.line, expr.loc.col);
      lint(expr, 0, false, true);
      printf("\"\n"
             "#endif\n        ");
    }
    printf(")");
  }

  char* strchrnul(char* str, char ch) {
    while (*str and *str != ch) str++;
    return str;
  }
  void lineupmultilinestring(Expr& expr, int indent) {
    return;
    char* pos = expr.string;
    while (*(pos = strpbrk(pos, "\n"))) {
      int del = strspn(pos, " ") - indent;
      if (del <= 0) continue;
    }
  }

  void printmultilstr(char* pos) {
    do {
      int p = strcspn(pos, "\n");
      printf("\"%.*s", p, pos);
      pos += p + 1;
      printf(*pos ? "\\n\"" : "\"");
    } while (*pos);
  }

  void emit_string(Expr& expr, int level) {
    lineupmultilinestring(expr, level + STEP);
    if (not expr.vars) {
      printmultilstr(expr.string + 1);
    } else {
      char* pos = expr.string;
      // putc('"', stdout);
      char* last = pos;
      Var& v;
      PtrList* p = expr.vars;
      Expr& e = p->item;
      printf("strinterp_h(64, ");
      while (*pos) {
        while (*pos and *pos != '$') pos++;
        *pos++ = 0;
        // int l = pos - last - 1;
        printf("%s", last);
        // printmultilstr(last + 1);
        pos[-1] = '$';
        last = pos;
        while (*pos and isalnum(*pos) or *pos == '.') pos++;
        // l = pos - last;
        // eprintf("%.*s\n", l, last);
        if (e) {
          while (e->kind == tkPeriod) e = e->right;
          assert(e->kind == tkIdentifierResolved);
          assert(e->var);
          printf("%s", TypeType_format(e->var.typeInfo.typeType, false));
          last = pos;
          e = ((p = p->next)) ? p->item : NULL;
        }
      }
      printf("\"");
      for (Var& e : expr.vars) { printf(", "), emit(e, 0); }
      printf(")");
    }
  }

  //_____________________________________________________________________________
  /// Emits the equivalent C code for a (literal) numeric expression.
  /// Complex numbers follow C99 literal syntax, e.g. 1i generates
  /// `_Complex_I * 1`.
  void emit_number(Expr& expr, int level) {
    size_t ls = CString_length(expr.string);
    if (expr.string[ls - 1] == 'i') {
      printf("_Complex_I*");
      expr.string[ls - 1] = 0;
    }
    printf("%s", expr.string);
  }

  void emit_check(Expr& expr, int level) {
    // TODO: need llhs and lrhs in case all 3 in 3way are exprs
    // e.g. check a+b < c+d < e+f
    Expr* checkExpr = expr.right; // now use checkExpr below
    Expr* lhsExpr = checkExpr->left;
    Expr* rhsExpr = checkExpr->right;
    printf("{\n");
    if (not checkExpr.unary) {
      printf("%.*s%s _lhs = ", level, spaces, typeName(lhsExpr));
      emit(lhsExpr, 0);
      printf(";\n");
    }
    printf("%.*s%s _rhs = ", level, spaces, typeName(rhsExpr));
    emit(rhsExpr, 0);
    printf(";\n");
    printf("%.*sif (not (", level, spaces);
    // ----- use lhs rhs cached values instead of the expression
    emit(checkExpr, 0);
    // how are you doing to deal with x < y < z? Repeat all the logic of
    // emit?
    // ----------------------------------------------------------------
    // if (checkExpr.unary) {
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
      level + STEP, spaces, expr.line, expr.col + 6);
    lint(checkExpr, 0, true, true);
    printf("\");\n");
    printf("#ifdef DEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

    genPrintVars(checkExpr, level + STEP);
    // the `printed` flag on all vars of the expr will be set
    // (genPrintVars uses this to avoid printing the same var
    // twice). This should be unset after every toplevel call to
    // genPrintVars.
    if (not checkExpr.unary) {
      // dont print literals or arrays
            if (lhsExpr.collectionType == CTYNone //
                and not lhsExpr.is(tkString, tkNumber, tkRawString, tkOpLE, tkOpLT) {
        if (lhsExpr.kind != tkIdentifierResolved
          or not lhsExpr.var->visited) {
          printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
          printf("%s", TypeType_format(lhsExpr.typeType, true));
          printf("%s", "\\n\", \"");
          lint(lhsExpr, 0, true, true);
          printf("%s", "\", _lhs);\n");
        }
        // checks can't have tkVarAssign inside them
        // if ()
        //     lhsExpr.var->visited = true;
            }
    }
    if (rhsExpr.collectionType == CTYNone //
      and not rhsExpr.is(tkString, tkNumber, tkRawString)) {
      if (rhsExpr.kind != tkIdentifierResolved
        or not rhsExpr.var->visited) {
        printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
        printf("%s", TypeType_format(rhsExpr.typeType, true));
        printf("%s", "\\n\", \"");
        lint(rhsExpr, 0, true, true);
        printf("%s", "\", _rhs);\n");
      }
    }

    unmarkVisited(checkExpr);

    printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
    printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
    printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// This should be a standard dispatcher that does nothing except the
  /// actual dispatching (via a function pointer table, not a switch).
  void emit(Expr& expr, int level) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("%.*s", level, spaces);
    switch (expr.kind) {
    case tkNumber: tkNumber(expr, level); break;

    case tkKeyword_no: printf("no"); break;
    case tkKeyword_yes: printf("yes"); break;
    case tkKeyword_nil: printf("nil"); break;

    case tkMultiDotNumber:
    case tkIdentifier: printf("%s", expr.string); break;

    case tkString: emit_string(expr, level); break;

    case tkIdentifierResolved: printf("%s", expr.var->name); break;

    case tkRawString: // 'raw strings' or 'regexes'
      printf("\"%s\"", expr.string + 1);
      break;

    case tkRegexp: // inline C code?
      printf("%s", expr.string + 1);
      break;

    case tkLineComment: // TODO: skip  comments in generated code
      printf("// %s", expr.string);
      break;

    case tkFunctionCall:
      unreachable("unresolved call to '%s'\n", expr.string);
      break;

    case tkFunctionCallResolved:
      emit_functionCallResolved(expr, level);
      break;

    case tkSubscript:
      unreachable("unresolved subscript on '%s'\n", expr.string);
      break;

    case tkSubscriptResolved: tkSubscriptResolved(expr, level); break;

    case tkOpAssign:
    case tkOpPlusEq:
    case tkOpMinusEq:
    case tkOpTimesEq:
    case tkOpSlashEq:
    case tkOpPowerEq:
    case tkOpModEq:

      switch (expr.left->kind) {
      case tkSubscriptResolved:
        switch (expr.left->left->kind) {
        case tkNumber:
        case tkString:
        case tkRawString:
          // TODO: typename should return Array_Scalar or
          // Tensor2D_Scalar or Dict_String_Scalar etc.
          printf("%s_set(%s, %s,%s, ", typeName(expr.left),
            expr.left->var.name, expr.left->left->string,
            TokenKind_repr(expr.kind, true));
          emit(expr.right, 0);
          printf(")");
          break;

        case tkOpColon:
          printf(
            "%s_setSlice(%s, ", typeName(expr.left), expr.left->var.name);
          emit(expr.left->left, 0);
          printf(",%s, ", TokenKind_repr(expr.kind, true));
          emit(expr.right, 0);
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
          printf("%s_setFiltered(%s, ", typeName(expr.left),
            expr.left->var.name);
          emit(expr.left->left, 0);
          printf(",%s, ", TokenKind_repr(expr.kind, true));
          emit(expr.right, 0);
          printf(")");
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
          unreachable("%s\n", TokenKinds_names[expr.left->kind]);
          assert(0);
        }
        break;
      case tkIdentifierResolved:
      case tkPeriod:
        emit(expr.left, 0);
        printf("%s", TokenKind_repr(expr.kind, true));
        emit(expr.right, 0);
        break;
      case tkIdentifier:
        unreachable("unresolved var %s", expr.left->string);
        break;
      case tkArgumentLabel:
        // assert(inFuncArgs);
        emit(expr.right, 0);
        // function call arg label, do not generate ->left
        break;
      case tkString: break;
      default:
        // error: not a valid lvalue
        // TODO: you should at some point e,g, during resolution
        // check for assignments to invalid lvalues and raise an
        // error
        unreachable(
          "found token kind %s\n", TokenKinds_names[expr.left->kind]);
      }
      // if (not  inFuncArgs) {
      //     emit(self->left, 0,
      //     escStrings); printf("%s", TokenKind_repr(tkOpAssign,
      //     spacing));
      // }
      // emit(self->right, 0,     escStrings);
      // check various types of lhs  here, eg arr[9:87] = 0,
      // map["uuyt"]="hello" etc.
      break;

    case tkArrayOpen:
      // TODO: send parent Expr& as an arg to this function. Then
      // here do various things based on whether parent is a =,
      // funcCall, etc.
      if (not expr.right) {
        printf("Array_init(%s)()", "double");
      } else {
        printf("Array_make(((%s[]) {", "double"); // FIXME
        // TODO: MKARR should be different based on the
        // CollectionType of the var or arg in question, eg stack
        // cArray, heap allocated Array, etc.
        emit(expr.right, 0);
        printf("})");
        printf(", %d)", countCommaList(expr.right));
      }
      break;

    case tkBraceOpen: {
      const char* Ktype = "CString";
      const char* Vtype = "Real64";
      if (not expr.right)
        printf("Dict_init(%s,%s)()", Ktype, Vtype); // FIXME
      else {
        printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
          countCommaList(expr.right), Ktype); // FIXME

        Expr& p = expr.right;
        while (p and p->kind == tkOpComma) {
          emit(p->left->left, 0);
          printf(", ");
          p = p->right;
        };
        emit(p->left, 0);
        printf("}, (%s[]){", Vtype);
        p = expr.right;
        while (p and p->kind == tkOpComma) {
          emit(p->left->right, 0);
          printf(", ");
          p = p->right;
        };
        emit(p->right, 0);
        printf("})");
      }
    } break;

    case tkOpColon: // convert 3:4:5 to range(...)
                    // must do bounds check first!
      printf(
        "%s(", expr.left->kind != tkOpColon ? "range_to" : "range_to_by");
      if (expr.left->kind == tkOpColon) {
        expr.left->kind = tkOpComma;
        emit(expr.left, 0);
        expr.left->kind = tkOpColon;
      } else
        emit(expr.left, 0);
      printf(", ");
      emit(expr.right, 0);
      printf(")");
      break;

    case tkVarAssign: // basically a tkOpAssign corresponding to a local
                      // var
      // var x as XYZ = abc... -> becomes an ASTVar and an
      // ASTExpr (to keep location). Send it to ASTVar::gen.
      if (expr.var->init != NULL and expr.var->used) {
        printf("%s = ", expr.var->name);
        emit(expr.var->init, 0);
      } else {
        printf("/* %s %s at line %d */", expr.var->name,
          expr.var->used ? "null" : "unused", expr.line);
      }
      break;

    case tkKeyword_else:
      puts("else {");
      if (expr.body) emit(expr.body, level + STEP);
      printf("%.*s}", level, spaces);
      break;

    case tkKeyword_elif:
      puts("else if (");
      emit(expr.left, 0);
      puts(") {");
      if (expr.body) emit(expr.body, level + STEP);
      printf("%.*s}", level, spaces);
      break;

    case tkKeyword_match: {
      // char* typeName = typeName(expr.left);
      // if (not typeName)
      //     unreachable(
      //         "unresolved type during emit at %d:%d", expr.line,
      //         expr.col);
      // if (expr.left->typeType == TYObject)
      //     typeName = typeName(expr.left);
      printf("{%s __match_cond = ", typeName(expr.left));
      emit(expr.left, 0);
      if (expr.left->typeType > TYInt8
        or (expr.left->typeType == TYObject
          and getObjectType(expr.left)->isEnum))
        puts("; switch (__match_cond) {");
      else
        puts("; { if (0) {}"); // the case will add 'else if's
      // puts(") {");
      if (expr.body) emit(expr.body, level);
      printf("%.*s}}", level, spaces);
      break;
    }

      /*
      This is how you walk a ASTExpr that is a tkOpComma (left to right):
          process(cond->left);
          while (cond->right->kind == tkOpComma)
              cond = cond->right, process(cond->left);
          process(cond->right);
      */

      // void pro(ASTExpr * c) { }
      // TODO: generally all comma exprs should be handled like this
      // iteratively. What if you have a large array with lots of items?
      // recursion will blow the stack
    case tkKeyword_case: {
      // TODO: maybe make this a macro
      Expr& cond = expr.left;
      if (cond->kind == tkOpComma) {
        if (cond->typeType > TYInt8
          or (cond->typeType == TYObject and getEnumType(cond))) {
          printf("case "); // match has handled the cond with a 'switch'
          emit(cond->left, 0);
          printf(": ");
          while (cond->right->kind == tkOpComma) {
            cond = cond->right;
            printf("case ");
            emit(cond->left, 0);
            printf(": ");
          }
          printf("case ");
          emit(cond->right, 0);
          puts(": {");
        } else if (cond->typeType == TYString) {
          printf("else if (not strcmp(__match_cond, ");
          emit(cond->left, 0);
          printf(")");
          while (cond->right->kind == tkOpComma) {
            cond = cond->right;
            printf(" or not strcmp(__match_cond, ");
            emit(cond->left, 0);
            printf(")");
          }
          printf(" or not strcmp(__match_cond, ");
          emit(cond->right, 0);
          puts(")) do {");
        } else {
          printf("else if (__match_cond == ");
          emit(cond->left, 0);
          while (cond->right->kind == tkOpComma) {
            cond = cond->right;
            printf(" or __match_cond == (");
            emit(cond->left, 0);
          }
          emit(cond->right, 0);
          puts(")) do {");
        };

      } else {
        if (cond->typeType > TYInt8
          or (cond->typeType == TYObject and getEnumType(cond))) {
          printf("case "); // match has handled the cond with a 'switch'
          emit(cond, 0);
          puts(": {");
        } else if (cond->typeType == TYString) {
          printf("else if (not strcmp(__match_cond, ");
          emit(cond, 0);
          puts(")) do {");
        } else {
          printf("else if (__match_cond == (");
          emit(cond, 0);
          puts(")) do {");
        };
      };
      if (expr.body) emit(expr.body, level);
      printf("%.*s}", level, spaces);
      if (cond->typeType > TYInt8
        or (cond->typeType == TYObject and getEnumType(cond)))
        printf(" break");
      else
        printf(" while(0)");
      break;
    }
    case tkKeyword_for:
    case tkKeyword_if:
      //    case tkKeyword_elif:
      //    case tkKeyword_else:
    case tkKeyword_while:
      if (expr.kind == tkKeyword_for)
        printf("FOR(");
      else
        printf("%s (", TokenKind_repr(expr.kind, true));
      if (expr.kind == tkKeyword_for) expr.left->kind = tkOpComma;
      if (expr.left) emit(expr.left, 0);
      if (expr.kind == tkKeyword_for) expr.left->kind = tkOpAssign;
      puts(") {");
      if (expr.body) emit(expr.body, level + STEP);
      printf("%.*s}", level, spaces);
      break;

    case tkOpPower:
      printf("pow(");
      emit(expr.left, 0);
      printf(",");
      emit(expr.right, 0);
      printf(")");
      break;

    case tkKeyword_return:
      printf("{_err_ = NULL; STACKDEPTH_DOWN; return ");
      if (expr.right) emit(expr.right, 0);
      printf(";}\n");
      break;

    case tkKeyword_check: tkCheck(expr, 0); break;

    case tkPeriod:
      emit(expr.left, 0);
      if (expr.left->typeType == TYObject
        and getObjectType(expr.left)->isEnum)
        printf("_");
      else
        printf("->"); // may be . if right is embedded and not a
                      // reference
      emit(expr.right, 0);
      break;

    case tkKeyword_notin: printf("!"); fallthrough;
    case tkKeyword_in:
      // the RHS should be dims==1 or another kind of collection, you
      // should have checked it in the analysis phase.
      switch (expr.right->kind) {
      case tkArrayOpen:
        if (expr.right->right->kind == tkOpColon)
          goto inRangeOp; // x in [a:b]
        // now its a literal array. that makes it easy, you can either
        // call isin() or the macro ISIN() if you have relatively few
        // items in the array.

        {
          int c = countCommaList(expr.right->right);
          // if (c <= 64) {
          // TODO: ISIN/isin must be specialized for types other than
          // int. in particular strings cannot be used yet
          printf("%s(%d, ", c <= 64 ? "ISIN" : "isin", c);
          emit(expr.left, 0);
          printf(", ");
          emit(expr.right->right, 0);
          printf(")");
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
          TokenKind_repr(expr.right->kind, false));
        // for anything else, figure it out.
      }
      break;

    case tkOpEQ:
    case tkOpNE:
    case tkOpGE:
    case tkOpLE:
    case tkOpGT:
    case tkOpLT:
            if ((expr.in( tkOpLE, tkOpLT)
                and (expr.left->in( tkOpLE , tkOpLT)) {
        printf("%s_cmp3way_%s_%s(", typeName(expr.left->right),
          TokenKind_ascrepr(expr.kind, false),
          TokenKind_ascrepr(expr.left->kind, false));
        emit(expr.left->left, 0), printf(", ");
        emit(expr.left->right, 0), printf(", ");
        emit(expr.right, 0), printf(")");
        break;
            } else if (expr.right->typeType == TYString) {
        printf("CString_cmp(%s, ", TokenKinds_srepr[expr.kind]);
        emit(expr.left, 0), printf(", ");
        emit(expr.right, 0), printf(")");
        break;
            }
            fallthrough;
        default:
            if (not expr.prec) break;
            // not an operator, but this should be error if you reach here
            bool leftBr
                = expr.left and expr.left->prec and expr.left->prec < expr.prec;
            bool rightBr = expr.right and expr.right->prec
                and expr.right->kind != tkKeyword_return
                and expr.right->prec < expr.prec;
            // found in 'or return'

            char lpo = '(';
            char lpc = ')';
            if (leftBr) putc(lpo, stdout);
            if (expr.left) emit(expr.left, 0);
            if (leftBr) putc(lpc, stdout);

            if (expr.kind == tkArrayOpen)
                putc('{', stdout);
            else
                printf("%s", TokenKind_repr(expr.kind, true));

            char rpo = '(';
            char rpc = ')';
            if (rightBr) putc(rpo, stdout);
            if (expr.right) emit(expr.right, 0);
            if (rightBr) putc(rpc, stdout);

            if (expr.kind == tkArrayOpen) putc('}', stdout);
    }
  }

  // WARNING: DO NOT USE THESE STRINGS WITH PRINTF(...) USE PUTS(...).
  //   const char* coverageFunc[] = { //
  //     "  void coverage_report() { /* unused */ }",
  //     "  void coverage_report() {\n"
  //     "    int count=0,l=NUMLINES;\n"
  //     "    while(--l>0) count+=!!_cov_[l];\n"
  //     "    printf(\"coverage: %d/%d lines = %.2f%%\\n\","
  //     "        count, NUMLINES, count*100.0/NUMLINES);\n"
  //     "}"
  // };
  // WARNING: DO NOT USE THESE STRINGS WITH PRINTF(...) USE PUTS(...).
  //   const char* lineProfileFunc[] = {
  //     //
  //     "  void lineprofile_report() { /* unused */ }\n"
  //     "  void lineprofile_begin() { /* unused */ }\n",
  //     "  void lineprofile_report() {\n"
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
  //     "  void lineprofile_begin() {_lprof_last_=getticks();}\n"
  // };
  ///////////////////////////////////////////////////////////////////////////
  // TODO: why do you need to pass level here?

  void genTypeInfoDecls(Type& type);
  void genTypeInfoDefs(Type& type);
  void genNameAccessors(Type& type);
  void emit(Module& module) {
    // puts("");
    for (Import& import : module.imports) emit(import, 0);

    puts("");

    for (Var& var : module.scope->vars)
      if (var.used) genh(var, 0);

    // for (Type& type : module.enums)
    //     if (type.body and type.analysed) genh(type, 0);

    for (Type& type : module.types)
      if (type.body and type.analysed)
        genh(type, 0), genTypeInfoDecls(type);

    for (Func& func : module.funcs)
      if (func.body and func.analysed) { genh(func, 0); }

    for (Type& type : module.types) {
      if (type.body and type.analysed) {
        emit(type, 0);
        genTypeInfoDefs(type);
        genNameAccessors(type);
      }
    }
    for (Func& func : module.funcs)
      if (func.body and func.analysed) emit(func, 0);

    for (Import& import : module.imports) undefc(import);

    // puts(coverageFunc[genCoverage]);
    // puts(lineProfileFunc[genLineProfile]);
  }

  void genTests(Module& module) {
    emit(module);
    for (Test &test, module.tests) emit(test);
    // generate a func that main will call
    printf("\nvoid tests_run_%s() {\n", module.name);
    for (Test &test, module.tests) printf("    test_%s();\n", test->name);
    puts("}");
  }

  // Generates a couple of functions that allow setting an integral member
  // of a type at runtime by name, or getting a pointer to a member by
  // name.
  void genNameAccessors(Type& type) {
    // TODO: instead of a linear search over all members this should
    // generate a switch for checking using a prefix tree -> see
    // genrec.c
    if (not type.analysed or type.isDeclare) return;
    // genMemberRecognizer( type, "Int64 value",  )

    printf("  void* %s__memberNamed(%s self, const char* name) {\n",
      type.name, type.name);

    // TODO: skip bitfield members in this loop or it wont compile
    for (Var &var, type.body->vars) /*if (var.used) */ //
      printf("    if (CString_equals(name, \"%s\")) return "
             "&(self->%s);\n",
        var.name, var.name);
    printf("    return NULL;\n}\n");

    // this func sets bools or ints that may be part of bitfields
    printf("  void %s__setMemberNamed(%s self, const char* name, "
           "Int64 "
           "value) {\n",
      type.name, type.name);
    for (Var &var, type.body->vars) // if (var.used) //
      if (var.typeInfo.typeType >= TYBool
        and var.typeInfo.typeType <= TYReal64)
        printf("    if (CString_equals(name, \"%s\"))  {self->%s = "
               "*(%s*) "
               "&value;return;}\n",
          var.name, var.name, cname(var.typeInfo));
    printf("}\n");
  }

  // Generates some per-type functions that write out meta info of the
  // type to be used for reflection, serialization, etc.
  void genTypeInfoDecls(Type& type) {
    if (not type.analysed or type.isDeclare) return;

    printf("  const char* const %s__memberNames[] = {\n    ", type.name);
    if (type.body) //
      for (Var &var, varn, type.body->vars) {
        if (not var or not var.used) continue;
        printf("\"%s\", ", var.name);
        // emit(var, level + STEP, false);
      }
    printf("};\n");
  }

  void genTypeInfoDefs(Type& type) {
    // printf("  const char* const %s__memberNames[] = {\n",
    // type.name); for(Var& var, varn, type.body->vars)
    // {
    //     if (not  var) continue;
    //     printf("\"%s\",\n", var.name);
    //     // emit(var, level + STEP, false);
    //     printf("}; \\\n");
    // }
  }
};