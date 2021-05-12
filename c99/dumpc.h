
#define PRFIELD(ex, fi, fmt)                                               \
  if (ex->fi) printf("." #fi " = " fmt ",\n", ex->fi)

// _Thread_local static FILE* outfile = NULL;

// #define printf(...) fprintf(outfile, __VA_ARGS__)
// #define puts(s) fputs(s, outfile)

static void imp_dumpc(Import* import, int level) {
  printf("&(Import) {.name = \"%s\", .aliasOffset = "
         "%d}",
      import->name, import->aliasOffset);
}

static void spec_dumpc(TypeSpec* spec, int level) {
  printf("&(TypeSpec) {\n");
  PRFIELD(spec, typeType, "%d");
  PRFIELD(spec, collectionType, "%d");
  PRFIELD(spec, dims, "%d");
  PRFIELD(spec, nullable, "%d");
  if (spec->typeType == TYObject) {
    printf(".type = &%s_%s ", spec->type->isEnum ? "enum" : "type",
        spec->type->name);
  } else {
    printf(".name = \"%s\" ", spec->name);
  }
  printf("}\n");
}

static void expr_dumpc(
    Expr* expr, int level, bool spacing, bool escStrings);

static void var_dumpc(Var* var, int level);
static void var_dumpc(Var* var, int level) {
  printf("{\n%.*s.name = \"%s\",\n%.*s.spec = ", level, spaces, var->name,
      level, spaces);
  spec_dumpc(var->spec, 0);
  if (var->init) {
    printf(", .init = ");
    expr_dumpc(var->init, 0, yes, yes);
  }
  printf("}");
}

static void scope_dumpc(Scope* scope, int level) {
  foreachn(Expr*, expr, exprList, scope->stmts) {
    expr_dumpc(expr, 0, yes, yes);
    if (exprList->next) printf(", ");
    continue;

    switch (expr->kind) {
    case tkCase:
    case tkMatch:
      printf("%.*s", level, spaces);
      printf("%s ", TokenKind_repr[expr->kind]);
      if (expr->left) expr_dumpc(expr->left, 0, yes, yes);
      puts("");
      //, yes, escStrings);
      if (expr->kind == tkMatch) {
        if (expr->body) scope_dumpc(expr->body, level);
        printf("%.*send %s\n", level, spaces, ""); // "match");
      } else {
        if (expr->body) scope_dumpc(expr->body, level + STEP);
      }
      break;

    case tkFor:
    case tkIf:
    case tkElif:
    case tkElse:
    case tkWhile: {
      printf("%.*s", level, spaces);
      printf("%s ", TokenKind_repr[expr->kind]);
      if (expr->left) expr_dumpc(expr->left, 0, yes, yes);
      puts("");
      if (expr->body)
        scope_dumpc(expr->body, level + STEP); //, yes, escStrings);
      //            const char* tok = TokenKind_repr[expr->kind];
      //            if (expr->kind == tkElse || expr->kind ==
      //            tkElif)
      //                tok = "if";
      if (expr->kind == tkIf || expr->kind == tkElif)
        if (exprList->next) {
          Expr* next = exprList->next->item;
          if (next->kind == tkElse || next->kind == tkElif) break;
        }
      printf("%.*send %s\n", level, spaces, ""); // tok);
    } break;
    default: expr_dumpc(expr, level, yes, yes); puts("");
    }
  }
}

static void type_dumpc(Type* type, int level) {
  printf("static Type type_%s = {\n", type->name);
  printf("    .body = &(Scope) { .locals = ");
  PtrList* vars = type->body->locals;
  if (vars) do {
      printf("&(PtrList) {\n        .item = &(Var)");
      var_dumpc(vars->item, 8);
      if ((vars = vars->next)) { printf(",\n        .next = "); }
    } while (vars);
  for_to(i, li_count(type->body->locals) - 1) printf("}\n");
  printf("}}, .name =\"%s\"};\n", type->name);
}

static void JetEnum_dumpc(Type* type, int level) {
  printf("static Type enum_%s = { .name =\"%s\" , .isEnum = yes };\n",
      type->name, type->name);
}

static void func_dumpc(Func* func, int level) {
  printf("static Func func_%s = { .name = \"%s\",", func->sel, func->name);
  PRFIELD(func, sel, "\"%s\"");
  PRFIELD(func, psel, "\"%s\"");
  if (func->argCount) {
    printf(".args = ");
    PtrList* args = func->args;
    do {
      printf("&(PtrList) {\n .item = &(Var)");
      var_dumpc(args->item, 0);
      if ((args = args->next)) { printf(", .next = "); }
    } while (args);
    for_to(i, func->argCount - 1) printf("} ");
    printf("}, ");
  }
  printf(".intrinsic = %d };\n", func->intrinsic);
  // if (func->isDefCtor || func->intrinsic) return;

  // printf("~ [ ");
  // if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
  // if (func->throws) printf("throws ");
  // if (func->isCalledFromWithinLoop) printf("looped ");
  // if (func->isCalledAsync) printf("asyncable ");
  // printf("]\n");

  // if (func->isDeclare) printf("declare ");

  // printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

  // foreachn(Var*, arg, args, func->args) {
  //     var_dumpc(arg, level);
  //     printf(args->next ? ", " : "");
  // }
  // printf(")");

  // if (func->spec && !func->isStmt) {
  //     printf(" as ");
  //     spec_dumpc(func->spec, level);
  // }
  // if (func->isDeclare) {
  //     puts("");
  //     return;
  // } else if (!func->isStmt) {
  //     puts("");
  //     scope_dumpc(func->body, level + STEP);
  //     puts("end\n");
  // } else {
  //     Expr* def = func->body->stmts->item;
  //     def = def->right; // its a return expr
  //     printf(" := ");
  //     expr_dumpc(def, 0, yes, no);
  //     puts("\n");
  // }
}

static void JetTest_dumpc(JetTest* test, int level) {
  // printf("test '%s'\n", test->name);
  // scope_dumpc(test->body, level + STEP);
  // puts("end\n");
}

// generally an expr is not split over several lines (but maybe in
// rare cases). so level is not passed on to recursive calls.
static void expr_dumpc(
    Expr* expr, int level, bool spacing, bool escStrings) {

  printf("&(Expr) {\n .kind = %s,\n", TokenKind_names[expr->kind]);
  PRFIELD(expr, line, "%d");
  PRFIELD(expr, col, "%d");

  switch (expr->kind) {
  case tkNumber:
    //   case tkMultiDotNumber:
    //   case tkArgumentLabel:
  case tkIdent: printf(".str = \"%s\",\n", expr->str); break;

  case tkRegexp:
  case tkRawString:
  case tkString: printf(".str = \"%s\",\n", expr->str + 1); break;

  case tkIdentR:
    printf(".var = &var_%d_%s, ", expr->var->line, expr->var->name);
    break;

  case tkSubscript:
  case tkFuncCall:
    printf(".str = \"%s\",\n", expr->str);
    if (expr->left) {
      printf(".left = ");
      expr_dumpc(expr->left, 0, 0, escStrings);
      printf(",\n");
    }
    break;

  case tkNo: break;
  case tkYes: break;
  case tkNil: break;

  case tkComment:
    printf("%s%s", TokenKind_repr[tkComment], expr->str);
    break;

    printf("%s(", expr->str);
    if (expr->left) expr_dumpc(expr->left, 0, no, escStrings);
    printf(")");
    break;
  case tkFuncCallR:
    // char* tmp = (expr->kind == tkFuncCallR) ?
    //                                                    : expr->str;
    printf(".func = &func_%s, \n", expr->func->sel);
    if (expr->left) {
      printf(".left = ");
      expr_dumpc(expr->left, 0, 0, 0);
      printf(",\n");
    }

    break;

  case tkSubscriptR: {
    char* tmp = (expr->kind == tkSubscriptR) ? expr->var->name : expr->str;
    printf("%s[", tmp);
    if (expr->left) expr_dumpc(expr->left, 0, no, escStrings);
    printf("]");
  } break;

  case tkObjInit:
  case tkObjInitR:
    break;

    // case tkPeriod:
    //     if (expr->left && expr->left->typeType == TYObject
    //         && !expr->left->var->spec->type->isEnum)
    //         expr_dumpc(expr->left, 0, spacing, escStrings);
    //     printf(".");
    //     expr_dumpc(expr->right, 0, spacing, escStrings);
    //     break;

  case tkVarDefn:
    // var x as XYZ = abc... -> becomes an Var and an Expr
    // (to keep location). Send it to var_dumpc.
    assert(expr->var != NULL);
    var_dumpc(expr->var, 0);
    break;

    // case tkArrayOpen:
    // case tkBraceOpen:
    //     printf("%s", TokenKind_repr[expr->kind]);
    //     if (expr->right)
    //         expr_dumpc(
    //             expr->right, level, expr->kind != tkArrayOpen,
    //             escStrings);
    //     printf("%s",
    //     TokenKind_repr[TokenKind_reverseBracket(expr->kind)]); break;

    // case tkIn:
    // case tkNotin:
    //     // these seem to add precedence parens aruns expr->right if done
    //     as
    //     // normal binops. so ill do them separately here.
    //     expr_dumpc(expr->left, 0, spacing, escStrings);
    //     printf("%s", TokenKind_repr[expr->kind]);
    //     expr_dumpc(expr->right, 0, spacing, escStrings);
    //     break;

  default:
    if (!expr->prec) break;
    if (!expr->unary && expr->left) {
      printf(".left = ");
      expr_dumpc(expr->left, 0, 0, escStrings);
      printf(",\n");
    }
    if (expr->right) {
      printf(".right = ");
      expr_dumpc(expr->right, 0, 0, escStrings);
      printf(",\n");
    }
  }

  PRFIELD(expr, throws, "%d");
  PRFIELD(expr, prec, "%d");
  PRFIELD(expr, unary, "%d");
  PRFIELD(expr, rassoc, "%d");
  printf(".allTypeInfo = %d", expr->allTypeInfo);
  // printf(".throws = %d,\n", expr->throws);
  printf("}");
}

static void mod_dumpc(Module* mod) {
  int l = strlen(mod->filename);
  static thread_local char buf[512];
  // TODO: improve this later
  mod->out_xc = __cstr_interp__s(512, buf, "%s/.%s.x.c",
      cstr_dir_ip(cstr_clone(mod->filename)),
      cstr_base(mod->filename, '/', l));

  outfile = fopen(mod->out_xc, "w");

  puts( //
      "#include \"jet/base.h\"\n"
      "#include \"token.h\"\n"
      "#include \"types.h\"\n"
      "#include \"ast.h\"\n" //
  );

  // outfile = fopen("dumpc_out.c", "w");
  foreach (Import*, imp, mod->imports)
    imp_dumpc(imp, 0);

  puts("");

  puts("");

  //--------

  foreach (Type*, type, mod->types)
    printf("static Type type_%s;\n", type->name);
  foreach (Type*, en, mod->enums)
    printf("static Type enum_%s;\n", en->name);
  foreach (Func*, func, mod->funcs)
    printf("static Func func_%s;\n", func->sel);
  foreach (JetTest*, test, mod->tests)
    printf("static Test test_%d;\n", test->line);
  foreach (Var*, var, mod->scope->locals)
    printf("static Var var_%s;\n", var->name);

  //--------

  foreach (Type*, type, mod->types)
    type_dumpc(type, 0);

  foreach (Type*, en, mod->enums)
    JetEnum_dumpc(en, 0);

  foreach (Func*, func, mod->funcs)
    func_dumpc(func, 0);

  foreach (JetTest*, test, mod->tests)
    JetTest_dumpc(test, 0);

  foreach (Var*, var, mod->scope->locals)
    var_dumpc(var, 0), puts("");

  printf("static Module mod_%s = {\n", mod->cname);
  PRFIELD(mod, name, "\"%s\"");
  PRFIELD(mod, cname, "\"%s\"");
  PRFIELD(mod, Cname, "\"%s\"");
  PRFIELD(mod, filename, "\"%s\"");
  printf(".funcs = ");
  foreach (Func*, func, mod->funcs) {
    printf("&(PtrList) {\n.item = &func_%s, .next = \n", func->sel);
  }
  printf("NULL");
  for_to(i, li_count(mod->funcs)) { printf("}"); }
  printf("};");

  fclose(outfile);
}

// #undef puts
// #undef printf
