#define genLineNumbers 0
#define genCoverage 0
#define genLineProfile 0

static void imp_emit(Import* import, int level, char ext) {
  if (!import->mod) return;
  // char* alias = import->aliasOffset + import->name;
  if (ext == 'c') {
    printf("#include \"%s\"\n", import->mod->out_c);
  } else {
    printf( //
      "#ifndef HAVE_%s_H\n"
      "#include \"%s\"\n"
      "#endif\n",
      import->mod->Cname, import->mod->out_h);
  }
}

static void spec_emit(
  TypeSpec* spec, int level, bool isconst, bool isconstptr) {
  isconst = false, isconstptr = false;

  if (spec->collType) {
    if (isconst) outl("const_");

    if (spec->collType) {
      if (spec->collType == CTYDict)
        // TODO: define DictS and DictU as string & uint dicts then you wont
        // need this special treatment
        outl("Dict(CString, ");
      else
        printf("%s(", Collectiontype_name(spec->collType));
    }
  }

  if (isconst) outl("const_");
  switch (spec->typeType) {
  case TYObject:
    printf("%s%s", spec->type->name, isconstptr ? "_const" : "");
    break;
  case TYUnknown:
    unreachable(
      "unresolved: '%s' at %d:%d", spec->name, spec->line, spec->col);
    printf("%s", *spec->name ? spec->name : "Error_Type");
    break;
  default: printf("%s", Typetype_name(spec->typeType)); break;
  }

  if (spec->collType) printf("%s", ")* restrict");
}

static void expr_emit(Expr* expr, int level);

//-----------------------------------------------------------------------//

static void var_genh(Var* var, int level) {
  if (!var->init) return;
  spec_emit(var->spec, level, !var->changed, !var->reassigned);
  if (!var->reassigned) printf(" const");
  printf(" %s = ", var->name);
  expr_emit(var->init, 0);
  outln("");
}

static void var_emit(Var* var, int level) {
  // for C the variables go at the top of the block, without init
  printf("%.*s", level, spaces);
  if (var->spec)
    spec_emit(var->spec, level + STEP, !var->changed, !var->reassigned);
  if (var->isMutableArg) puts(" * const restrict ");
  printf(" %s", var->name);
}

static void var_insertDrop(Var* var, int level) {
  iprintf(level, "DROP(%s,%s,%s,%s);\n", spec_name(var->spec), var->name,
    Collectiontype_name(var->spec->collType),
    StorageClassNames[var->storage]);
}

//-----------------------------------------------------------------------//

static void scope_emit(Scope* scope, int level) {

  foreach (Expr*, stmt, scope->stmts) {
    if (stmt->kind == tkComment) continue;

    if (genLineNumbers) printf("#line %d\n", stmt->line);

    // You need to know if the expr_emit will in fact generate something.
    // This is true in general unless it is an unused var init.
    if (stmt->kind != tkVarDefn || stmt->var->used) {

      if (genCoverage) {
        iprintf(level, "JET_COVERAGE_UP(%d); \n", stmt->line);
      }
      if (genLineProfile) {
        iprintf(level, "JET_PROFILE_LINE(%d);\n", stmt->line);
      }
      if (genCoverage || genLineProfile) outln("");
    }

    expr_emit(stmt, level);
    puts(!(isCtrlExpr(stmt) || stmt->kind == tkReturn) ? ";" : "");
    // convert this into a flag which is set in the resolution pass

    // here you see if any vars are to be dropped at this point (because
    // they were last used in this expr. A var can only be dropped in its
    // own scope, not an inner or outer scope, so just scan our own vars.
    // In a sense the stmt->line is a local ID for the statement within the
    // scope.
    Scope* sco = scope;
    do {
      foreach (Var*, var, sco->locals)
        if (var->used && var->lastUsage == stmt->line) {
          var_insertDrop(var, level);
          var->lastUsage = 0; // this means var has been dropped.
        }
    } while (!sco->isLoop    // if loop scope, don't walk up
      && (sco = sco->parent) // walk up to the last loop scope
      && sco->parent && !sco->parent->isLoop // until you hit a loop
      && sco->parent->parent); // or until you hit 1 below globscope

    // TODO:
    // Maybe scope should have a lineno. If a loop scope has the lastUsage
    // of a parent var, it cannot drop it inside the loop, but it should be
    // done just after the loop ends. Now it will be dropped anyway at
    // owning scope end which may be suboptimal. If you have the scope line,
    // which is the line of the cond expr of if / while etc., you change the
    // lastUsage to that line and it gets dropped just after the scope.

    if (stmt->throws) printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
  }
  // It's possible some vars were not detected in inner scopes and dropped.
  // So let's drop them here. No need to walk up the parent chain here.
  foreach (Var*, var, scope->locals)
    if (var->used && var->lastUsage) var_insertDrop(var, level);
  // ^ these are all the vars whose lastUsage could not be
  // matched. This may be because they are in an inner scope. In
  // this case they should be dropped at the end of the scope.
  // Optimize this later so that if there are multiple subscopes
  // and the last usage is in one of them, the drop happens
  // after that subscope and doesn't wait until the very end.
}

//-----------------------------------------------------------------------//

// todo: stop using these in type_emit, generate actual Funcs for type ctors
// & helpers. then move them inside func_emit.
static const char functionEntryStuff_UNESCAPED[]
  = "  STACKDEPTH_UP; DO_STACK_CHECK;\n";

static const char functionExitStuff_UNESCAPED[]
  = "\n"
    "  return DEFAULT_VALUE;\n"
    "uncaught: HANDLE_UNCAUGHT;\n"
    "backtrace: SHOW_BACKTRACE_LINE;\n"
    "return_: STACKDEPTH_DOWN;\n"
    "  return DEFAULT_VALUE;";

static void func_emit(Func* func, int level) {
  if (!func->body || !func->analysed || func->isDeclare) return;

  printf("#define DEFAULT_VALUE %s\n", getDefaultValueForType(func->spec));

  spec_emit(func->spec, level, false, false);
  printf(" %s(", func->sel);
  foreachn(Var*, arg, args, func->args) {
    var_emit(arg, level);
    printf(args->next ? ", " : "");
  }
  if (func->spec->typeType != TYVoid) {
    if (func->args) printf(", ");
    spec_emit(func->spec, level, false, false);
    printf(" ans");
  }
  if (func->yields) {
    if (func->args) printf(", ");
    outl(" void (*yield)(");
    spec_emit(func->spec, level, false, false);
    outl(")");
  }

  printf(" IFDEBUG("
         "  %c const char* callsite_ "
         ")",
    ((func->args || func->spec->typeType != TYVoid ? ',' : ' ')));

  outln(") {");
  printf("  IFDEBUG(static const char* sig_ = \"");
  printf("%s%s(", func->isStmt ? "" : "func ", func->name);

  foreachn(Var*, arg, args, func->args) {
    var_write(arg, level);
    printf(args->next ? ", " : "");
  }
  outl(")");
  if (func->spec->typeType != TYVoid) {
    outl(" ");
    spec_write(func->spec, level);
  }
  outln("\");");

  puts(functionEntryStuff_UNESCAPED);

  scope_emit(func->body, level + STEP);

  if (func->spec->typeType != TYVoid) printf("exitfunc: return ans;");
  puts(functionExitStuff_UNESCAPED);
  outln("}\n#undef DEFAULT_VALUE");
}

static void func_genh(Func* func, int level) {
  if (!func->body || !func->analysed || func->isDeclare) return;
  // if (!func->isExported) outl("static ");
  spec_emit(func->spec, level, false, false);
  printf(" %s(", func->sel);
  foreachn(Var*, arg, args, func->args) {
    var_emit(arg, level);
    printf(args->next ? ", " : "");
  }
  if (func->spec->typeType != TYVoid) {
    if (func->args) printf(", ");
    spec_emit(func->spec, level, false, false);
    printf(" ans");
  }
  printf(" IFDEBUG(%c const char* callsite_)",
    ((func->args || func->spec->typeType != TYVoid) ? ',' : ' '));
  outln(");\n");
}

//-----------------------------------------------------------------------//

static void type_genJson(Type* type) {
  printf("static void %s_json_(const %s self, int nspc) {\n", type->name,
    type->name);

  printf("  printf(\"{\\n\");\n");

  // TODO: move this part into its own func so that subclasses can ask the
  // superclass to add in their fields inline.
  foreachn(Var*, var, vars, type->body->locals) {
    if (!var) continue;
    printf(
      "  printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n", var->name);
    printf("  %s_json_(self->%s, nspc+4);\n    printf(\"",
      spec_name(var->spec), var->name);
    if (vars->next) outl(",");
    outln("\\n\");");
  }
  printf("  printf(\"%%.*s}\", nspc, _spaces_);\n");
  printf("}\nMAKE_JSON_WRAP_(%s)\n//MAKE_JSON_FILE(%s)\n", type->name,
    type->name);
}

static void type_genDrop(Type* type) {
  printf("static void %s_drop_(%s self) {\n", type->name, type->name);
  outln("  if (self->refc_--) return;");
  foreach (Var*, var, type->body->locals) {
    // FIXME: strings & ranges etc should also be dropped
    // besides, arrayys of numbers too
    if (var->spec->typeType == TYObject && !var->spec->type->isEnum)
      printf("  if (self->%s)", var->name);
    printf("  %s_drop_(self->%s);\n", spec_name(var->spec), var->name);
  }
  printf("  %s_free_(self);\n}\n", type->name);
}

static void type_genJsonReader(Type* type) { }

static void type_emit_fieldHead(Type* type) {
  const char* const name = type->name;
  printf("#define FIELDS_%s \\\n", name);

  if (type->super) {
    outl("  FIELDS_");
    spec_emit(type->super, 0, false, false);
    outln(" \\");
  }
  foreach (Var*, var, type->body->locals) {
    if (!var /*or not var->used*/) continue;
    // It's not so easy to just skip 'unused' type members.
    // what if I just construct an object and print it?
    // I expect to see the default members. But if they
    // haven't been otherwise accessed, they are left out. FIX: when a var
    // of type T is used, all members of T are marked used (recursively).
    var_emit(var, 4);
    outln("; \\");
  }
  outln("");
}

static void type_emit(Type* type, int level) {
  const char* const name = type->name;
  printf("\n\nstruct %s {\n  int refc_;\n", name);

  printf("  FIELDS_%s\n};\n\n", name);
  printf("static const char* %s_name_ = \"%s\";\n\n", name, name);
  printf("static %s %s_alloc_(void) {\n"
         "  return Pool_alloc(gPool, sizeof(struct %s));\n"
         "}\n\n",
    name, name, name);
  printf("static void %s_free_(%s self) {}\n", name, name);

  printf("static %s %s_init_(%s self) {\n", name, name, name);

  foreach (Var*, var, type->body->locals)
    printf("#define %s self->%s\n", var->name, var->name);

  foreach (Expr*, stmt, type->body->stmts) {
    if (!stmt || stmt->kind != tkVarDefn || !stmt->var->init) continue;
    printf("%.*s%s = ", level + STEP, spaces, stmt->var->name);
    expr_emit(stmt->var->init, 0);
    outln(";");
    if (stmt->var->init->throws) outln("  TRACE_IF_ERROR;");
  }
  foreach (Var*, var, type->body->locals)
    printf("#undef %s \n", var->name);

  outln("  return self;\n}\n");

  printf("#define DEFAULT_VALUE NULL\n"
         "monostatic %s %s_new_(IFDEBUG(const char* callsite_)) {\n"
         "IFDEBUG(static const char* sig_ = \"%s()\");\n",
    name, name, name);
  puts(functionEntryStuff_UNESCAPED);
  printf("  %s ret = %s_alloc_(); %s_init_(ret);\n"
         "  TRACE_IF_ERROR;\n"
         "  _err_ = NULL; STACKDEPTH_DOWN; return ret;\n",
    name, name, name);
  puts(functionExitStuff_UNESCAPED);
  outln("}\n#undef DEFAULT_VALUE\n");

  printf("#define %s_print(p) %s_print__(p, STR(p))\n", name, name);
  printf("monostatic void %s_print__(%s self, const char* name) {\n    "
         "printf(\"<%s '%%s' at %%p size %%luB>\\n\","
         "  name, self, sizeof(struct %s));\n}\n",
    name, name, name, name);
  outln("");

  foreach (Var*, var, type->body->locals)
    printf("%s* %s_addrof_%s(%s selfp) {return &(selfp->%s);}\n",
      spec_name(var->spec), name, var->name, name, var->name);

  type_genJson(type);
  type_genDrop(type);
  type_genJsonReader(type);
}

// TODO: move this to global (after all modules have been processed) aand
// combine types from all modules. It should go into its own header file.
// each type generates an exter int _TypeID_xxx which will resolve to the
// enum member (or a static const int since you have assigned id already).
static void type_genID(Type* type) {
  static int _id = 0;
  if (type->id) return;
  if (type->super && type->super->typeType == TYObject)
    type_genID(type->super->type);
  type->id = ++_id;
  printf("  _TypeID_%s,\n", type->name);
}

static void type_genh(Type* type, int level) {
  if (!type->body || !type->analysed || type->isDeclare) return;

  const char* const name = type->name;
  // printf("struct %s;\n", name);
  printf("typedef struct %s* %s;\n", name, name);
  // printf("typedef const struct %s* restrict const_%s;\n", name, name);
  // printf("typedef const struct %s* restrict %s_const;\n", name, name);
  // printf("typedef const struct %s* const restrict const_%s_const;\n",
  // name, name);
  printf("monostatic %s %s_alloc_(void); \n", name, name);
  printf("monostatic %s %s_init_(%s self);\n", name, name, name);
  printf("monostatic void %s_drop_(%s self);\n", name, name);
  printf("monostatic void %s_free_(%s self);\n", name, name);
  printf("monostatic %s %s_new_(IFDEBUG(const char* callsite_)); \n", name,
    name);
  printf("\nDECL_json_wrap_(%s)\n//DECL_json_file(%s)\n", name, name);
  printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
         "%s_json_wrap_(x); }\n\n",
    name, name);
  printf(
    "monostatic void %s_json_(const %s self, int nspc);\n", name, name);

  foreach (Var*, var, type->body->locals)
    printf("monostatic %s* %s_addrof_%s(%s selfp);\n", spec_name(var->spec),
      name, var->name, name);
  //^ note that for objects the accessor func returns T**. genc for the .
  // oper
  // adds a leading * in any case.
}

// Generates a couple of functions that allow setting an integral member
// of a type at runtime by name, or getting a pointer to a member by
// name.
void type_genNameAccessors(Type* type) {
  // TODO: instead of a linear search over all members this should
  // generate a switch for checking using a prefix tree -> see
  // genrec.c
  if (!type->analysed || type->isDeclare) return;
  // type_genMemberRecognizer( type, "Int64 value",  )

  printf("static void* %s__memberNamed(%s self, char* name) {\n",
    type->name, type->name);

  // TODO: skip bitfield members in this loop or it wont compile
  foreach (Var*, var, type->body->locals) /*if (var->used) */ //
    printf("  if (cstr_eq(name, \"%s\")) return "
           "&(self->%s);\n",
      var->name, var->name);
  outln("  return NULL;\n}");

  // this func sets bools or ints that may be part of bitfields
  printf("static void %s__setMemberNamed(%s self, char* name, "
         "Int64 value) {\n",
    type->name, type->name);
  foreach (Var*, var, type->body->locals)
    if (var->changed) //
      if (var->spec->typeType >= TYBool && var->spec->typeType <= TYReal64)
        printf("  if (cstr_eq(name, \"%s\")) {\n"
               "    self->%s = *(%s*) &value; return;\n"
               "  }\n",
          var->name, var->name, spec_cname(var->spec));
  outln("}");
}

// Generates some per-type functions that write out meta info of the
// type to be used for reflection, serialization, etc.
void type_genTypeInfoDecls(Type* type) {
  if (!type->analysed || type->isDeclare) return;

  printf(
    "static const char* const %s__memberNames[] = {\n    ", type->name);
  if (type->body) //
    foreach (Var*, var, type->body->locals) {
      if (var) printf("\"%s\", ", var->name);
    }
  outln("};");
}

void type_genTypeInfoDefs(Type* type) {
  // printf("static const char* const %s__memberNames[] = {\n",
  // type->name); foreachn(Var*, var, varn, type->body->locals)
  // {
  //     if (! var) continue;
  //     printf("\"%s\",\n", var->name);
  //     // var_emit(var, level + STEP, false);
  //     outln("}; \\");
  // }
}

static void enum_genh(Type* type, int level) {
  if (!type->body || !type->analysed) return;
  const char* const name = type->name;
  outln("typedef enum {");
  if (!type->isMultiEnum) {
    // multi enums generate 1,2,4,8... but 0 is valid.
    // single enums generate 1,2,3,4... 0 is invalid or "none".
    printf("  %s__none_ = 0,\n", name);
  }
  foreach (Var*, var, type->body->locals)
    printf("  %s_%s,\n", name, var->name);
  printf("} %s;\n", name);
  Expr* ex1 = type->body->stmts->item;
  const char* datType
    = ex1->kind == tkAssign ? expr_typeName(ex1->right) : NULL;
  if (datType)
    printf("monostatic %s %s__data[%d];\n", datType, name,
      li_count(type->body->locals));
  printf("monostatic const char* %s__fullnames[] ={\n", name);
  foreach (Var*, var, type->body->locals)
    printf("  \"%s.%s\",\n", name, var->name);
  outln("};");
  printf("monostatic const char* %s__names[] ={\n", name);
  foreach (Var*, var, type->body->locals)
    printf("  \".%s\",\n", var->name);
  outln("};");

  printf("monostatic void %s__init(void) {\n", name);

  foreach (Expr*, stmt, type->body->stmts) {
    if (!stmt || stmt->kind != tkAssign) continue;
    printf("%.*s%s__data[%s_%s] = ", level + STEP, spaces, name, name,
      stmt->left->str);
    expr_emit(stmt->right, 0);
    outln(";");
    if (stmt->right->throws) outln("  TRACE_IF_ERROR;");
  }
  outln("}");
}

static void test_emit(Test* test, Module* mod, int idx)
// TODO: should tests not return BOOL?
{
  if (!test->body) return;
  printf("\nstatic int test_%s_%d(void) {\n", mod->cname, idx);
  scope_emit(test->body, STEP);
  outln("  return 0;");
  outln("  backtrace: return 1;");
  outln("}");
}

//-----------------------------------------------------------------------//

static void expr_unmarkVisited(Expr* expr) {
  switch (expr->kind) {
  case tkIdentR:
  case tkVarDefn: expr->var->visited = false; break;
  case tkFuncCallR:
  case tkFuncCall: // shouldnt happen
  case tkSubscriptR:
  case tkSubscript:
  case tkIf:
  case tkFor:
  case tkElse:
  case tkWhile: expr_unmarkVisited(expr->left); break;
  default:
    if (expr->prec) {
      if (!expr->unary) expr_unmarkVisited(expr->left);
      expr_unmarkVisited(expr->right);
    }
  }
}

// given an expr, generate code to print all the resolved vars in it (only
// scalars). Used by checks to print the vars involved in the check expr, if
// the check fails.
static void expr_genPrintVars(Expr* expr, int level) {
  assert(expr);
  // TODO: what about func args?
  switch (expr->kind) {
  case tkIdent: break; // arg labels etc

  case tkIdentR:
  case tkVarDefn:
    if (expr->var->visited) break;
    if (!strcasecmp("yes", expr->var->name)) break;
    if (!strcasecmp("no", expr->var->name)) break;
    if (!strcasecmp("nil", expr->var->name)) break;
    printf("%.*sprintf(\"  %s = %s\\n\", %s);\n", level, spaces,
      expr->var->name, Typetype_format(expr->typeType, true),
      expr->var->name);
    expr->var->visited = true;
    break;

  case tkPeriod: break;

  case tkFuncCallR:
  case tkFuncCall: // shouldnt happen
  case tkSubscriptR:
  case tkSubscript:
  case tkIf:
  case tkElse:
  case tkFor:
  case tkWhile: expr_genPrintVars(expr->left, level); break;

  default:
    if (expr->prec) {
      if (!expr->unary) expr_genPrintVars(expr->left, level);
      expr_genPrintVars(expr->right, level);
    }
  }
}

/// Emits the equivalent C code for a subscript (that has been resolved to
/// its corresponding `Variable`). This function does all of the heavy
/// lifting to decide what the subscript actually does, based on the kind
/// of the subscript expression, number of dimensions and the context.
static void expr_emit_tkSubscriptR(Expr* expr, int level) {
  char* name = expr->var->name;
  Expr* index = expr->left;
  assert(index);
  // index = index->right;
  switch (index->kind) {
  case tkNumber: // indexing with a single number, can be a -ve number
    printf("Array_get_%s(%s, %s)", spec_cname(expr->var->spec), name,
      index->str);
    break;

  case tkString:
  case tkRawString: // indexing with single string or regex
    printf("Dict_getk_CString_%s(%s, \"%s\")", spec_cname(expr->var->spec),
      name, index->str + 1);
    break;

  case tkComma: // higher dims. validation etc. has been done by this
                // stage.

    // this is for cases like arr[2, 3, 4].
    printf("Tensor%dD_get_%s(%s, {", expr->var->spec->dims,
      spec_cname(expr->var->spec), name);
    expr_emit(index, 0);
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

  case tkColon:
    // a single range.
    printf("Array_getSlice_%s(%s, ", spec_name(expr->var->spec), name);
    expr_emit(index, 0);
    outl(")");
    break;
    // what about mixed cases, e.g. arr[2:3, 5, 3:end]
    // make this portion a recursive function then, or promote
    // all indexes to ranges first and then let opcomma handle it

  case tkEQ:
  case tkLE:
  case tkGE:
  case tkGT:
  case tkLT:
  case tkNE:
  case tkAnd:
  case tkOr:
  case tkNot:
    // indexing by a Boolean expression (filter)
    // by default this implies a copy, but certain funcs e.g. print
    // min max sum count etc. can be done in-place without a copy
    // since they are not mutating the array. That requires either
    // the user to call print(arr, filter = arr < 5) instead of
    // print(arr[arr < 5]), or the compiler to transform the second
    // into the first transparently.
    // Probably the tkFuncCall should check if its argument is
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
    printf("Array_copy_filter_%s(%s, ", spec_name(expr->var->spec), name);
    expr_emit(index, 0);
    outl(")");
    break;

  default: unreachable("bad kind: %s", TokenKind_names[expr->kind]); break;
  }
}

/// Emits the equivalent C code for a function call (that has been
/// resolved to its corresponding `Func`). Type constructors call a C
/// function that has `_new` appended to the type name. This function
/// passes a constructed string as the extra argument `callsite_` that is
/// used to generate accurate backtraces.
static void expr_emit_tkFuncCallR(Expr* expr, int level) {
  char* tmp = expr->func->sel;
  const Func* const func = expr->func;
  Expr* arg = expr->left;
  const char* tmpc = "";
  if (arg) {
    if (arg->kind == tkComma) arg = arg->left;
    tmpc = Collectiontype_name(arg->collType);
  }
  printf("%s%s", tmpc, tmp);
  if (*tmp >= 'A' && *tmp <= 'Z' && !strchr(tmp, '_')) outl("_new_");
  outl("(");

  // arg = expr->left;
  // if (arg) {
  //       if (arg->kind == tkComma) arg = arg->left;
  //   // arg is now the first argument (receiver, self, this,...)
  //   // if it is not exactly the expected type, means this is a parent
  //   type's
  //   // func that is being called because a more specific one wasnt
  //   defined.
  //   // In C all types are distinct so passing A for B is a warning (even
  //   if
  //   // Jet knows that B extends A). So silence the warning with a cast.
  //   if
  //   // it really is some unexpected type, then analysis would have caught
  //   it
  //   // and raised an error. if (arg->typeType==TYObject
  //   !=func->args->item) printf("(%s)", expr_typeName(arg));
  // }

  arg = expr->left;
  List(Var*)* fArgL = func->args;
  while (arg) {
    Expr* cArg = arg;
    if (cArg->kind == tkComma) cArg = cArg->left;
    if (cArg->kind == tkArgAssign) {
      outl("/* ");
      expr_emit(cArg->left, 0);
      outl("= */ ");
      cArg = cArg->right;
    }

    // Cast it so that (Jet-level) upcasts dont result in warnings
    TypeSpec* fSpec = ((Var*)fArgL->item)->spec;
    printf("(%s)", spec_name(fSpec));

    expr_emit(cArg, 0);
    arg = arg->kind == tkComma ? arg->right : NULL;
    if (arg) outl(", ");
    fArgL = fArgL->next; // analyse will have picked up mismatches
  }

  // if (expr->left) expr_emit(expr->left, 0);

  if (!func->isDeclare) {
    // ------ 8< ------------------------------------------------------
    // remove this and add an arg from the top expr. this is because only
    // there you know what the target should be, heap/stack/pool allocated,
    // another var, etc.
    const bool hasAns = func->spec->typeType != TYVoid && !func->isDefCtor;
    if (hasAns) { // insert ans
      if (expr->left) outl(", ");
      // TODO: if inplacing etc then you can just pass in the target var.
      // for now passing a new instance.
      // the user should assume that ans is always an allocated obj ready to
      // be modified. if you overwrite ans that (possibly optimal) obj is
      // lost.
      switch (func->spec->typeType) {
      case TYObject:
        if (!func->spec->type->isEnum) {
          printf("%s_new_(IFDEBUG(THISFILE\":%d:%d: (ans for '%s')\"))",
            spec_cname(func->spec), expr->line, expr->col, func->psel);
        } else {
          outl("0");
        }
        break;
      case TYString: outl("\"\""); break;
      default: outl("0"); break;
      }
    }
    // ------ 8< ------------------------------------------------------
    printf(" IFDEBUG(%c THISFILE \":%d:%d: ",
      expr->left || hasAns ? ',' : ' ', expr->line, expr->col);
    expr_write(expr, 0, false, true);
    printf("\")");
  }
  outl(")");
}

char* strchrnul(char* str, char ch) {
  while (*str && *str != ch) str++;
  return str;
}
static void expr_lineupmultilinestring(Expr* expr, int indent) {
  return;
  char* pos = expr->str;
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

static void expr_emit_tkString(Expr* expr, int level) {
  expr_lineupmultilinestring(expr, level + STEP);
  // outl("String_fromCString(");
  if (!expr->vars) {
    // outl("(char[]){"); // FIXME: only needed for mutable strings
    printmultilstr(expr->str + 1);
    // outl("}");
  } else {
    char *pos = expr->str, *last = pos;
    // Var* v;
    PtrList* p = expr->vars;
    Expr* e = p->item;
    outl("cstr_interp_h(64, ");
    while (*pos) {
      while (*pos && *pos != '$') pos++;
      *pos++ = 0;
      printf("%s", last);
      pos[-1] = '$';
      last = pos;
      while (*pos && (isalnum(*pos) || *pos == '.')) pos++;
      if (e) {
        while (e->kind == tkPeriod) e = e->right;
        assert(e->kind == tkIdentR);
        assert(e->var);
        printf("%s", Typetype_format(e->var->spec->typeType, false));
        last = pos;
        e = ((p = p->next)) ? p->item : NULL;
      }
    }
    printf("\"");
    foreach (Expr*, e, expr->vars) {
      outl(", ");
      expr_emit(e, 0);
    }
    outl(")");
  }
  // outl(")");
}

/// Emits the equivalent C code for a (literal) numeric expression.
/// Complex numbers follow C99 literal syntax, e.g. 1i generates
/// `_Complex_I * 1`.
static void expr_emit_tkNumber(Expr* expr, int level) {
  size_t ls = cstr_length(expr->str);
  if (expr->str[ls - 1] == 'i') {
    outl("_Complex_I*");
    expr->str[ls - 1] = 0;
  }
  printf("%s", expr->str);
  if (!strpbrk(expr->str, ".eE")) printf(".0");
}

static void expr_emit_tkCheck(Expr* expr, int level) {
  // TODO: need llhs and lrhs in case all 3 in 3way are exprs
  // e.g. check a+b < c+d < e+f
  Expr* checkExpr = expr->right; // now use checkExpr below
  Expr *lhsExpr = NULL, *rhsExpr = NULL;
  // TODO: check exprs must be booleans! ensure that in analyse
  switch (checkExpr->kind) {
  case tkIdentR:
  case tkFuncCallR:
  case tkSubscriptR: lhsExpr = checkExpr; break;
  default: lhsExpr = checkExpr->left; rhsExpr = checkExpr->right;
  }
  outln("{");
  if (lhsExpr) { //! checkExpr->unary) {
    printf("%.*s%s _lhs = ", level, spaces, expr_typeName(lhsExpr));
    expr_emit(lhsExpr, 0);
    outln(";");
  }
  if (rhsExpr) {
    printf("%.*s%s _rhs = ", level, spaces, expr_typeName(rhsExpr));
    expr_emit(rhsExpr, 0);
    outln(";");
  }
  printf("%.*sif (!(", level, spaces);
  // ----- use lhs rhs cached values instead of the expression
  // if (rhsExpr) printf("%s_rhs", TokenKind_srepr[checkExpr->kind]);

  expr_emit(checkExpr, 0);
  // how are you doing to deal with x < y < z? Repeat all the logic of
  // expr_emit?
  // ----------------------------------------------------------------
  // if (checkExpr->unary) {
  //     outl("_rhs");
  // } else {
  //     outl("_lhs %s _rhs");
  // }
  // -------------
  outln(")) {");
  printf("%.*sprintf(\"\\n%%s:%d:%d: error: check failed:\\n  %%s\\n\", "
         "THISFILE, \"",
    level + STEP, spaces, expr->line, expr->col + 6);
  expr_write(checkExpr, 0, true, true);
  outln("\");");
  printf("#ifndef NDEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

  expr_genPrintVars(checkExpr, level + STEP);
  // the `printed` flag on all vars of the expr will be set
  // (genPrintVars uses this to avoid printing the same var
  // twice). This should be unset after every toplevel call to
  // genPrintVars.
  // if (lhsExpr){//!checkExpr->unary) {
  // dont print literals or arrays
  if (lhsExpr && lhsExpr->collType == CTYNone
    && !ISIN(
      5, lhsExpr->kind, tkString, tkNumber, tkRawString, tkLE, tkLT)) {
    if (lhsExpr->kind != tkIdentR || !lhsExpr->var->visited) {
      printf("%.*s%s", level + STEP, spaces, "printf(\"  %s = ");
      printf("%s", Typetype_format(lhsExpr->typeType, true));
      printf("%s", "\\n\", \"");
      expr_write(lhsExpr, 0, true, true);
      printf("%s", "\", _lhs);\n");
    }
    // checks can't have tkVarDefn inside them
    // if ()
    //     lhsExpr->var->visited = true;
  }
  // }
  if (rhsExpr && rhsExpr->collType == CTYNone //
    && !ISIN(3, rhsExpr->kind, tkString, tkNumber, tkRawString)) {
    if (rhsExpr->kind != tkIdentR || !rhsExpr->var->visited) {
      printf("%.*s%s", level + STEP, spaces, "printf(\"  %s = ");
      printf("%s", Typetype_format(rhsExpr->typeType, true));
      printf("%s", "\\n\", \"");
      expr_write(rhsExpr, 0, true, true);
      printf("%s", "\", _rhs);\n");
    }
  }

  expr_unmarkVisited(checkExpr);

  printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
  printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
  printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
}

/// This should be a standard dispatcher that does nothing except the
/// actual dispatching (via a const function pointer table, not a switch).
static void expr_emit(Expr* expr, int level) {
  // generally an expr is not split over several lines (but maybe in
  // rare cases). so level is not passed on to recursive calls.

  printf("%.*s", level, spaces);
  switch (expr->kind) {
  case tkNo: outl("false"); break;
  case tkYes: outl("true"); break;
  case tkNil: outl("NULL"); break;
  case tkIdent: printf("%s", expr->str); break;
  case tkNumber: expr_emit_tkNumber(expr, level); break;

  case tkString:
  case tkRawString: expr_emit_tkString(expr, level); break;

  case tkIdentR:
    if (expr->var->isMutableArg) outl("(*");
    printf("%s", expr->var->name);
    if (expr->var->isMutableArg) outln(")");
    break;

  case tkRegexp: printf("\"%s\"", expr->str + 1); break;

  case tkComment: printf("// %s", expr->str); break;

  case tkFuncCall:
    unreachable("unresolved call to '%s'\n", expr->str);
    break;

  case tkFuncCallR: expr_emit_tkFuncCallR(expr, level); break;

  case tkSubscript:
    unreachable("unresolved subscript on '%s'\n", expr->str);
    break;

  case tkSubscriptR: expr_emit_tkSubscriptR(expr, level); break;

  case tkAssign:
  case tkPlusEq:
  case tkMinusEq:
  case tkTimesEq:
  case tkSlashEq:
  case tkPowerEq:
  case tkModEq:

    switch (expr->left->kind) {
    case tkSubscriptR:
      switch (expr->left->left->kind) {
      case tkNumber:
      case tkString:
      case tkRawString:
        // TODO: astexpr_typename should return Array_Scalar or
        // Tensor2D_Scalar or Dict_String_Scalar etc.
        printf("%s_set(%s, %s,%s, ", expr_typeName(expr->left),
          expr->left->var->name, expr->left->left->str,
          TokenKind_srepr[expr->kind]);
        expr_emit(expr->right, 0);
        outl(")");
        break;

      case tkColon:
        printf("%s_setSlice(%s, ", expr_typeName(expr->left),
          expr->left->var->name);
        expr_emit(expr->left->left, 0);
        printf(",%s, ", TokenKind_srepr[expr->kind]);
        expr_emit(expr->right, 0);
        outl(")");
        break;

      case tkEQ:
      case tkGE:
      case tkNE:
      case tkGT:
      case tkLE:
      case tkLT:
      case tkAnd:
      case tkOr:
      case tkNot:
        printf("%s_setFiltered(%s, ", expr_typeName(expr->left),
          expr->left->var->name);
        expr_emit(expr->left->left, 0);
        printf(",%s, ", TokenKind_srepr[expr->kind]);
        expr_emit(expr->right, 0);
        outl(")");
        break;

      case tkComma:
        // figure out the type of each element
        // there should be a RangeND just like TensorND and
        // SliceND then you can just pass that to _setSlice
        break;
      case tkIdentR:
        // lookup the var type. note that it need not be Number,
        // string, range etc. it could be an arbitrary object in
        // case you are indexing a Dict with keys of that type.
        break;
      case tkSubscriptR: // arr[arr2[4]] etc.
        break;
      case tkFuncCallR: // arr[func(x)]
        break;
      default:
        unreachable("%s\n", TokenKind_names[expr->left->kind]);
        assert(0);
      }
      break;
    case tkIdentR:
    case tkPeriod:
      expr_emit(expr->left, 0);
      printf("%s", TokenKind_srepr[expr->kind]);
      expr_emit(expr->right, 0);
      break;
    case tkIdent:
      unreachable("unresolved var %s", expr->left->str);
      break;
      break;
    case tkString: break;
    default:
      // TODO: you should at some point e,g, during resolution
      // check for assignments to invalid lvalues and raise an
      // error
      unreachable(
        "found token kind %s\n", TokenKind_names[expr->left->kind]);
    }
    // if (! inFuncArgs) {
    //     expr_emit(self->left, 0,
    //     escStrings); printf("%s", TokenKind_repr(tkAssign,
    //     spacing));
    // }
    // expr_emit(self->right, 0,     escStrings);
    // check various types of lhs  here, eg arr[9:87] = 0,
    // map["uuyt"]="hello" etc.
    break;

  case tkArrayOpen:
    // TODO: send parent Expr* as an arg to this function. Then
    // here do various things based on whether parent is a =,
    // funcCall, etc.
    if (!expr->right) {
      printf("Array_init(%s)()", expr_typeName(expr->right));
    } else {
      printf("Array_make(%s)(((%s[]) {", expr_typeName(expr->right),
        expr_typeName(expr->right)); // FIXME
      expr_emit(expr->right, 0);
      outl("})");
      printf(", %d)", expr_countCommaList(expr->right));
    }
    break;

  case tkBraceOpen: {
    const char* Ktype = "CString";
    const char* Vtype = "Real64";
    if (!expr->right)
      printf("Dict_init(%s,%s)()", Ktype, Vtype); // FIXME
    else {
      Vtype = expr_typeName(expr->right);
      printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
        expr_countCommaList(expr->right), Ktype);
      // ^ FIXME

      Expr* p = expr->right;
      while (p && p->kind == tkComma) {
        expr_emit(p->left->left, 0);
        outl(", ");
        p = p->right;
      };
      expr_emit(p->left, 0);
      printf("}, (%s[]){", Vtype);
      p = expr->right;
      while (p && p->kind == tkComma) {
        expr_emit(p->left->right, 0);
        outl(", ");
        p = p->right;
      };
      expr_emit(p->right, 0);
      outl("})");
    }
  } break;

  case tkColon: // convert 3:4:5 to range(...) must do bounds check first!
    printf("%s(", expr->left->kind != tkColon ? "range_to" : "range_to_by");
    if (expr->left->kind == tkColon) {
      expr->left->kind = tkComma;
      expr_emit(expr->left, 0);
      expr->left->kind = tkColon;
    } else
      expr_emit(expr->left, 0);
    outl(", ");
    expr_emit(expr->right, 0);
    outl(")");
    break;

  case tkVarDefn: // basically a tkAssign corresponding to a local var
    // var x as XYZ = abc... -> becomes an Var and an
    // Expr (to keep location). Send it to Var::gen.
    if (expr->var->init != NULL && expr->var->used) {
      var_emit(expr->var, 0);
      outl(" = ");
      // if (expr->var->init->kind == tkFuncCallR) {
      //   expr_tkFuncCall_pushArg((&Expr){.kind=tkAssign})
      // }
      expr_emit(expr->var->init, 0);
    } else {
      printf("/* %s %s at line %d */", expr->var->name,
        expr->var->used ? "null" : "unused", expr->line);
    }
    break;

  case tkElse:
    outln("else {");
    if (expr->body) scope_emit(expr->body, level + STEP);
    printf("%.*s}", level, spaces);
    break;

  case tkElif:
    outln("else if (");
    expr_emit(expr->left, 0);
    outln(") {");
    if (expr->body) scope_emit(expr->body, level + STEP);
    printf("%.*s}", level, spaces);
    break;

  case tkMatch: {
    printf("{%s __match_cond = ", expr_typeName(expr->left));
    expr_emit(expr->left, 0);
    if (expr->left->typeType > TYInt8
      || (expr->left->typeType == TYObject
        && expr_getObjectType(expr->left)->isEnum))
      outln("; switch (__match_cond) {");
    else
      outln("; { if (0) {}"); // cases will add else ifs
    if (expr->body) scope_emit(expr->body, level);
    printf("%.*s}}", level, spaces);
    break;
  }

  case tkCase: {

    Expr* cond = expr->left;
    if (cond->kind == tkComma) {
      if (cond->typeType > TYInt8
        || (cond->typeType == TYObject && expr_getEnumType(cond))) {
        // match has handled the cond with a 'switch'
        outl("case "), expr_emit(cond->left, 0), outl(": ");
        while (cond->right->kind == tkComma) {
          cond = cond->right;
          outl("case "), expr_emit(cond->left, 0), outl(": ");
        }
        outl("case "), expr_emit(cond->right, 0), outln(": {");
      } else if (cond->typeType == TYString) {
        outl("else if (!strcmp(__match_cond, ");
        expr_emit(cond->left, 0);
        outl(")");
        while (cond->right->kind == tkComma) {
          cond = cond->right;
          outl(" || !strcmp(__match_cond, ");
          expr_emit(cond->left, 0), outl(")");
        }
        outl(" || !strcmp(__match_cond, ");
        expr_emit(cond->right, 0), outln("))   {");
      } else if (cond->typeType == TYRegex) {
        outl("else if (rex_matches(__match_cond, ");
        expr_emit(cond->left, 0);
        outl(")");
        while (cond->right->kind == tkComma) {
          cond = cond->right;
          outl(" || rex_matches(__match_cond, ");
          expr_emit(cond->left, 0), outl(")");
        }
        outl(" || rex_matches(__match_cond, ");
        expr_emit(cond->right, 0), outln("))   {");
      } else {
        outl("else if (__match_cond == ");
        expr_emit(cond->left, 0);
        while (cond->right->kind == tkComma) {
          cond = cond->right;
          outl(" || __match_cond == ("), expr_emit(cond->left, 0);
        }
        expr_emit(cond->right, 0);
        outln(")) {");
      };

    } else {
      if (cond->typeType > TYInt8
        || (cond->typeType == TYObject && expr_getEnumType(cond))) {
        outl("case "); // match has handled the cond with a 'switch'
        expr_emit(cond, 0);
        outln(": {");
      } else if (cond->typeType == TYString) {
        outl("else if (!strcmp(__match_cond, ");
        expr_emit(cond, 0);
        outln("))   {");
      } else if (cond->typeType == TYRegex) {
        outl("else if (rex_matches(__match_cond, ");
        expr_emit(cond, 0);
        outln("))   {");
      } else {
        outl("else if (__match_cond == (");
        expr_emit(cond, 0);
        outln("))   {");
      };
    };
    if (expr->body) scope_emit(expr->body, level);
    printf("%.*s}", level, spaces);
    if (cond->typeType > TYInt8
      || (cond->typeType == TYObject && expr_getEnumType(cond)))
      outl(" break");
    else
      outl("  ");
    break;
  }

  case tkFor: {
    if (expr->left->right->kind == tkFuncCallR
      && expr->left->right->func->yields) {
      // this should just generate a call to the iterator w/ a ptr to a
      // func generated using the loop body
    } else {
      const char* ctypename
        = Collectiontype_name(expr->left->right->collType);
      printf("%s_for(%s, %s, ", ctypename, expr_typeName(expr->left->right),
        expr->left->var->name);
      expr_emit(expr->left->right, 0);
      outl(") {");
      scope_emit(expr->body, level + STEP);
      printf("%.*s}", level, spaces);
    }
  } break;

  case tkIf:
  case tkWhile:
    // if (expr->kind == tkFor)
    //   outl("FOR(");
    // else
    printf("%s (", TokenKind_repr[expr->kind]);
    expr_emit(expr->left, 0);
    outln(") {");
    scope_emit(expr->body, level + STEP);
    printf("%.*s}", level, spaces);
    break;

  case tkPower:
    outl("pow(");
    expr_emit(expr->left, 0);
    outl(",");
    expr_emit(expr->right, 0);
    outl(")");
    break;

  case tkReturn:
    outl("{_err_ = NULL; STACKDEPTH_DOWN; return ");
    if (expr->right) expr_emit(expr->right, 0);
    outln(";}");
    break;

  case tkCheck: expr_emit_tkCheck(expr, 0); break;

  case tkYield: outln("yield(ans);"); break;

  case tkPeriod: {
    if (expr->right->kind == tkFuncCallR) {
      Expr* args = expr->right->left;
      Expr* dummy = expr->left;
      if (args) {
        dummy = &(Expr) { //
          .kind = tkComma,
          .left = dummy,
          .right = args
        };
      }
      expr->right->left = dummy;
      expr_emit(expr->right, 0);
      expr->right->left = args;

    } else if (expr->left->typeType == TYObject
      && expr_getObjectType(expr->left)->isEnum) {
      expr_emit(expr->left, 0);
      outl("_");
      expr_emit(expr->right, 0);
    } else {
      printf("*%s_addrof_%s(", expr_typeName(expr->left),
        expr->right->var->name);
      expr_emit(expr->left, 0);
      outl(")");
    }
  } break;

  case tkNotin: outl("!"); fallthrough;
  case tkIn:
    // the RHS should be dims==1 or another kind of collection, you should
    // have checked it in the analysis phase.
    switch (expr->right->kind) {
    case tkArrayOpen:
      if (expr->right->right->kind == tkColon) goto inRangeOp; // x in [a:b]
      // now its a literal array. that makes it easy, you can either call
      // isin() or the macro ISIN() if you have relatively few items in
      // the array.

      {
        int c = expr_countCommaList(expr->right->right);
        // if (c <= 64) {
        // TODO: ISIN/isin must be specialized for types other than
        // int. in particular strings cannot be used yet
        printf("%s(%d, ", c <= 64 ? "ISIN" : "isin", c);
        expr_emit(expr->left, 0);
        outl(", ");
        expr_emit(expr->right->right, 0);
        outl(")");
        // }
      }

      break;
    case tkSubscript:
      // maybe slice or something
      break;
    case tkColon: // x in a:b
    inRangeOp:
      break;
    default:
      unreachable(
        "inside in operator: rhs is %s", TokenKind_repr[expr->right->kind]);
      // for anything else, figure it out.
    }
    break;

  case tkUnaryMinus:
    if (expr->right && expr->right->typeType == TYString) {
      // special case (in god mode): inline C
      puts(expr->right->str + 1);
    } else {
      // hope you checked for the operand being numeric in the analysis step
      puts("-");
      expr_emit(expr->right, 0);
    }
    break;

  case tkEQ:
  case tkNE:
  case tkGE:
  case tkLE:
  case tkGT:
  case tkLT:
    if ((expr->kind == tkLE || expr->kind == tkLT)
      && (expr->left->kind == tkLE | expr->left->kind == tkLT)) {
      printf("%s_cmp3way_%s_%s(", expr_typeName(expr->left->right),
        TokenKind_ascrepr(expr->kind, false),
        TokenKind_ascrepr(expr->left->kind, false));
      expr_emit(expr->left->left, 0);
      outl(", ");
      expr_emit(expr->left->right, 0);
      outl(", ");
      expr_emit(expr->right, 0);
      outl(")");
      break;
    } else if (expr->right->typeType == TYString) {
      printf("CString_cmp(%s, ", TokenKind_srepr[expr->kind]);
      expr_emit(expr->left, 0);
      outl(", ");
      expr_emit(expr->right, 0);
      outl(")");
      break;
    }
    fallthrough;
  default:
    if (!expr->prec) break;
    // not an operator, but this should be error if you reach here
    bool leftBr
      = expr->left && expr->left->prec && (expr->left->prec < expr->prec);
    //            || (expr->left->prec == expr->prec && !expr->rassoc));
    bool rightBr = expr->right && expr->right->prec
      && expr->right->kind != tkReturn && (expr->right->prec < expr->prec);
    //            || (expr->right->prec == expr->prec && expr->rassoc));
    // found in 'or return'

    // special case for (a==b)==(c==d). Don't want to arbitrarily enable
    // brackets for all nested binops since C parsers may be recursive
    // descent.
    if (expr->kind == tkEQ        //
      && expr->left->kind == tkEQ //
      && expr->right->kind == tkEQ)
      leftBr = rightBr = true;

    static const char* po = "(";
    static const char* pc = ")";

    if (leftBr) puts(po);
    if (expr->left) expr_emit(expr->left, 0);
    if (leftBr) puts(pc);

    if (expr->kind == tkArrayOpen)
      putc('{', stdout);
    else
      printf("%s", TokenKind_srepr[expr->kind]);

    // static const char *rpo = '(';
    // static const char* rpc = ')';
    if (rightBr) puts(po);
    if (expr->right) expr_emit(expr->right, 0);
    if (rightBr) puts(pc);

    if (expr->kind == tkArrayOpen) putc('}', stdout);
  }
}

//-----------------------------------------------------------------------//

static void mod_genTests(Module* mod) {
  int i = 0;
  foreach (Test*, test, mod->tests)
    test_emit(test, mod, ++i);

  printf("\nvoid jet_runTests_%s(int runDeps) {\n", mod->cname);
  printf("  static bool done = 0;\n  if (done) return;\n");

  // Deps run first.
  outln("  if (runDeps) {");
  foreach (Import*, imp, mod->imports)
    if (imp->mod) printf("    jet_runTests_%s(1);\n", imp->mod->cname);
  outln("  }\n");

  // Now run our own tsts
  outln("  clock_Time t0 = clock_getTime();\n"
        "  eprintf(\"%.*s\\n\",66, _dashes_);");
  printf("  eputs(\"\\e[34;1m%s\\e[0m\\n\");\n", mod->filename);
  i = 0;
  foreach (Test*, test, mod->tests) {
    bool skip = *test->name == '-';
    if (skip) test->name++;
    printf("  jet_runTest(test_%s_%d, \"%s\", %d);\n", //
      mod->cname, ++i, test->name, skip);
  }
  outln("double elap = clock_clockSpanMicro(t0);");
  outln("eprintf(\"    %-48s [%7.1f ms]\\n\", \"\", elap / 1e3);");
  outln("eputs(\"\");");

  outln("\n  done = 1;");
  outln("}");
}

int file_hash_equal(char* file1, char* file2);
bool file_move(char* src, char* target) { return !rename(src, target); }

static int mod_emit(Module* mod) {

  if (!(outfile = fopen(mod->out_hh, "w"))) {
    eprintf("%s:1:1-1: error: can't open file for writing\n", mod->out_hh);
    return 1;
  }

  printf("#ifndef HAVE_%s\n#define HAVE_%s\n\n", mod->Cname, mod->Cname);
  puts("#ifndef HAVE_JET_BASE_H\n"
       "#include \"jet/runtime.h\"\n"
       "#endif\n");

  // puts("DECL_COV_PROF(NUMLINES)\n");

  outln("#ifdef JET_MONOBUILD");
  foreach (Import*, import, mod->imports) { imp_emit(import, 0, 'c'); }
  outln("#else");
  foreach (Import*, import, mod->imports) { imp_emit(import, 0, 'h'); }
  outln("#endif");
  outln("");

  foreach (Var*, var, mod->scope->locals) {
    if (var->used) var_genh(var, 0);
  }
  foreach (Type*, type, mod->enums) {
    if (type->body && type->analysed) { enum_genh(type, 0); }
  }

  // FIXME: typeIDs should be generated over ALL types in all modules
  // printf("typedef enum {\n  TypeID_Null,");
  // // FIXME: add basic types here
  // foreach (Type*, type, mod->types) { type_genID(type); }
  // printf("  TypeID__end\n} _TypeID;\n");

  foreach (Type*, type, mod->types) {
    if (type->body && type->analysed) {
      type_genh(type, 0);
      type_genTypeInfoDecls(type);
    }
  }
  foreach (Func*, func, mod->funcs) {
    if (func->body && func->analysed) { func_genh(func, 0); }
  }
  printf("\nvoid jet_runTests_%s(int runDeps);\n", mod->cname);

  printf("#endif // HAVE_%s\n", mod->Cname);

  fclose(outfile);

  if (!file_hash_equal(mod->out_h, mod->out_hh)) {
    // header has changed, mark it so deps
    // can be recompiled
    // eprintf("%s: header updated\n", mod->out_h);
    mod->hmodified = true;
    unlink(mod->out_h);
    if (!file_move(mod->out_hh, mod->out_h)) {
      eprintf("%s:1:1-1: error: can't update file: %s\n", mod->out_h,
        strerror(errno));
      return 1;
    }
  } else
    unlink(mod->out_hh);

  // C file -------------------------------------------------------------

  if (!(outfile = fopen(mod->out_c, "w"))) {
    eprintf("%s:1:1-1: error: can't open file for writing\n", mod->out_c);
    return 1;
  }

  printf(
    "#include \"%s\"\n\n", cstr_base(mod->out_h, '/', strlen(mod->out_h)));

  if (genLineNumbers) printf("#line 1 \"%s\"\n", mod->filename);
  printf("#define THISFILE \"%s\"\n", mod->filename);
  printf("#define THISMODULE %s\n", mod->cname);
  printf("#define NUMLINES %d\n", mod->nlines);

  foreach (Type*, type, mod->types) {
    if (type->body && type->analysed && !type->isDeclare) {
      type_emit_fieldHead(type);
    }
  }

  foreach (Type*, type, mod->types) {
    if (type->body && type->analysed && !type->isDeclare) {
      type_emit(type, 0);
      type_genTypeInfoDefs(type);
      type_genNameAccessors(type);
    }
  }
  foreach (Func*, func, mod->funcs) {
    if (func->body && func->analysed) { func_emit(func, 0); }
  }

  mod_genTests(mod);

  if (mod->isRoot) {
    // fixme: start should be namespaced
    printf(
      "void (*_jet_entry_run_)(IFDEBUGELSE(const char*, void)) = start;\n"
      "void (*_jet_entry_test_)(int) = jet_runTests_%s;\n\n",
      mod->cname);
  }

  outln("#undef THISMODULE");
  outln("#undef THISFILE");
  outln("#undef NUMLINES");

  fclose(outfile);

  return 0;
}

// called only for the root module
static int mod_emit_mainwrapper(Module* mod) {
  if (!(outfile = fopen(mod->out_w, "w"))) {
    eprintf("%s:1:1-1: error: can't open file for writing\n", mod->out_w);
    return 1;
  }
  printf( //
    "#ifdef JET_MONOBUILD\n"
    "#include \"%s\"\n"
    "#ifdef JET_MODE_TEST\n"
    "#define JET_ENTRY %s_start\n"
    "#else\n"
    "#define JET_ENTRY jet_runTests_%s\n"
    "#endif\n"
    "#else\n"
    "#include \"%s\"\n" // entrypoint defined by -D...
    "#endif\n"
    "\n"
    "#ifdef JET_MODE_TEST\n"
    "#include \"jet/rt_test.c\"\n"
    "#else\n"
    "#include \"jet/rt_run.c\"\n"
    "endif\n"
    "\n",
    mod->out_c, mod->cname, mod->cname, mod->out_h);
  fclose(outfile);
  return 0;
}