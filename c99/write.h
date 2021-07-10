
static void imp_write(Import* import, int level) {
  char* alias = import->name + import->aliasOffset;
  printf("import %s%s%s\n", import->name, alias ? " as " : "",
    alias ? alias : "");
}

static void spec_write(TypeSpec* spec, int level) {
  switch (spec->typeType) {
  case TYObject: printf("%s", spec->type->name); break;
  case TYUnknown: printf("%s", spec->name); break;
  default: printf("%s", Typetype_name(spec->typeType)); break;
  }

  switch (spec->collType) {
  case CTYDict: printf("[DICTK]"); break;
  case CTYArray: printf("[]"); break;
  case CTYTensor:
    if (spec->dims) {
      static const char* dimsstr = ":,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:";
      printf("[%.*s]", 2 * spec->dims - 1, dimsstr);
    } else {
      printf("[?]");
    }
    break;
  default:;
  }
}

static void expr_write(
  Expr* expr, int level, bool spacing, bool escapeStrings);

static void var_write(Var* var, int level) {
  printf("%.*s%s%s", level, spaces,
    var->isVar     ? "var "
      : var->isLet ? "let "
                   : "",
    var->name);
  TokenKind kind = var->init ? var->init->kind : tkUnknown;

  bool genType = false; // set it here to override
  // if ((var->init and var->init->kind == tkFuncCall
  //         and !strcmp(var->init->str, var->spec->name))) {
  // } else if ((var->init and var->init->kind == tkNumber
  //                or var->init->kind == tkRegexp or var->init->kind ==
  //                tkString or var->init->kind == tkRawString)) {
  if (kind == tkFuncCall // unresolved constructor
    && !strcmp(var->init->str, var->spec->name)) {
  } else if (kind == tkFuncCallR && var->spec->typeType == TYObject
    && !strcmp(var->init->func->name, var->spec->type->name)) {
    // resolved constructor, so type is also resolved
  } else if (ISIN(6, kind, tkNumber, tkRegexp, tkNo, tkYes, tkString,
               tkRawString)) { // simple literals
    // } else if () { // usual ops on numbers
  } else if (kind == tkArrayOpen && var->init->right) {
    Expr* e1 = var->init->right;
    if (e1->kind == tkComma) e1 = e1->left;
    if (!ISIN(6, e1->kind, tkNumber, tkRegexp, tkNo, tkYes, tkString,
          tkRawString))
      genType = true;
  } else if (var->init
    && (isBoolOp(var->init) || isCmpOp(var->init)
      || isArithOp(var->init))) { // simple stuff that gives Boolean
  }
  // else if (var->spec->typeType == TYObject
  //     && var->spec->type->isEnum) {
  // }
  else if (var->spec->typeType == TYError || var->spec->typeType == TYVoid
    || (var->spec->typeType == TYUnknown && *var->spec->name == '\0')) {
    genType = false;
  } else {
    genType = true;
  }

  if (genType) {
    printf(" "); // as ");
    spec_write(var->spec, level + STEP);
  }
  // }
  // else {
  //     // should make this expr_defaultType and it does it recursively
  //     for
  //     // [, etc
  //     const char* ctyp = TokenKind_defaultType(
  //         self->init ? self->init->kind : tkUnknown);
  //     if (self->init and self->init->kind == tkListLiteral)
  //         ctyp = TokenKind_defaultType(
  //             self->init->right ? self->init->right->kind : tkUnknown);
  //     if (self->init and self->init->kind == tkFuncCall
  //         and *self->init->name >= 'A' and *self->init->name <= 'Z')
  //         ctyp = NULL;
  //     if (ctyp) printf(" as %s", ctyp);
  // }
  if (var->init) {
    printf(" = ");
    expr_write(var->init, 0, true, false);
  }
}

static void scope_write(Scope* scope, int level) {
  foreachn(Expr*, expr, exprList, scope->stmts) {
    switch (expr->kind) {
    case tkCase:
    case tkMatch:
      printf("%.*s", level, spaces);
      printf("%s ", TokenKind_repr[expr->kind]);
      if (expr->left) expr_write(expr->left, 0, true, false);
      puts("");
      //, true, escapeStrings);
      if (expr->kind == tkMatch) {
        if (expr->body) scope_write(expr->body, level);
        printf("%.*send %s\n", level, spaces, ""); // "match");
      } else {
        if (expr->body) scope_write(expr->body, level + STEP);
      }
      break;

    case tkFor:
    case tkIf:
    case tkElif:
    case tkElse:
    case tkWhile: {
      printf("%.*s", level, spaces);
      printf("%s ", TokenKind_repr[expr->kind]);
      if (expr->left) expr_write(expr->left, 0, true, false);
      puts("");
      if (expr->body)
        scope_write(expr->body, level + STEP); //, true, escapeStrings);
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
    default: expr_write(expr, level, true, false); puts("");
    }
  }
}

static void type_write(Type* type, int level) {
  if (type->isDeclare) printf("declare ");
  printf("type %s", type->name);
  if (type->super) {
    printf(" extends ");
    spec_write(type->super, level);
  }
  puts("");
  if (!type->body) return;

  foreach (Expr*, stmt, type->body->stmts) {
    if (!stmt) continue;
    expr_write(stmt, level + STEP, true, false);
    puts("");
  }
  puts("end\n");
}

static void JetEnum_write(Type* type, int level) {
  // if (!type->body) printf("declare ");
  printf("enum %s\n", type->name);
  // if (type->super) {
  //     printf(" extends ");
  //     spec_write(type->super, level);
  // }
  // puts("");
  if (type->body) foreach (Expr*, stmt, type->body->stmts) {
      if (!stmt) continue;
      expr_write(stmt, level + STEP, true, false);
      puts("");
    }
  puts("end\n");
}

static void func_write(Func* func, int level) {
  if (func->isDefCtor || func->intrinsic) return;

  printf("~ [ ");
  if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
  if (func->throws) printf("throws ");
  if (func->isCalledFromWithinLoop) printf("looped ");
  if (func->isCalledAsync) printf("asyncable ");
  printf("]\n");

  if (func->isDeclare) printf("decl ");

  printf("%s%s(", func->isStmt ? "\n" : "func ", func->name);

  foreachn(Var*, arg, args, func->args) {
    var_write(arg, level);
    printf("%s", args->next ? ", " : "");
  }
  printf(")");

  if (func->spec && !func->isStmt
    && !(func->spec->typeType == TYObject
      && !strcasecmp(func->spec->type->name, func->name))) {
    printf(" ");
    spec_write(func->spec, level);
  }
  if (func->isDeclare) {
    puts("");
    return;
  } else if (!func->isStmt) {
    puts("");
    scope_write(func->body, level + STEP);
    puts("end\n");
  } else {
    Expr* def = func->body->stmts->item;
    def = def->right; // its a return expr
    printf(" := ");
    expr_write(def, 0, true, false);
    puts("\n");
  }
}

static void JetTest_write(Test* test, int level) {
  printf("test '%s'\n", test->name);
  scope_write(test->body, level + STEP);
  puts("end\n");
}

static void expr_write(
  Expr* expr, int level, bool spacing, bool escapeStrings) {
  // generally an expr is not split over several lines (but maybe in
  // rare cases). so level is not passed on to recursive calls.
  printf("%.*s", level, spaces);

  switch (expr->kind) {
  case tkNumber: printf("%s", expr->str); break;
  // case tkMultiDotNumber:
  case tkRawString: printf("'%s'", expr->str + 1); break;
  case tkRegexp: printf("`%s`", expr->str + 1); break;

  case tkIdent:
  // case tkArgumentLabel:
  case tkIdentR: {
    char* tmp = (expr->kind != tkIdentR) ? expr->str : expr->var->name;
    printf("%s", tmp);
  } break;

  case tkString:
    printf(escapeStrings ? "\\%s\\\"" : "%s\"", expr->str);
    break;
  case tkNo: printf("no"); break;
  case tkYes: printf("yes"); break;
  case tkNil: printf("nil"); break;

  case tkComment:
    printf("%s%s", TokenKind_repr[tkComment], expr->str);
    break;

  case tkFuncCall:
    printf("%s(", expr->str);
    if (expr->left) expr_write(expr->left, 0, false, escapeStrings);
    printf(")");
    break;
  case tkFuncCallR:
    // char* tmp = (expr->kind == tkFuncCallR) ?
    //                                                    : expr->str;
    printf("%s(", expr->func->name);
    if (expr->left) {
      Expr* carg = expr->left;
      foreachn(Var*, var, listp, expr->func->args) {
        if (!carg) break;
        Expr* arg = (carg->kind == tkComma) ? carg->left : carg;
        if (arg->kind == tkArgAssign) arg = arg->right;
        if (/*arg->kind != tkAssign &&*/ listp != expr->func->args)
          printf("%s=", var->name);
        expr_write(arg, 0, false, escapeStrings);
        if (listp->next) printf(", ");
        carg = (carg->kind == tkComma) ? carg->right : NULL;
      }
    }
    printf(")");
    break;

  case tkSubscript:
  case tkSubscriptR: {
    char* tmp = (expr->kind == tkSubscriptR) ? expr->var->name : expr->str;
    printf("%s[", tmp);
    if (expr->left) expr_write(expr->left, 0, false, escapeStrings);
    printf("]");
  } break;

  case tkObjInit:
  case tkObjInitR: break;

  case tkPeriod:
    if (!expr->left) break;
    if (expr->left->kind == tkPeriod) {
      expr_write(expr->left, 0, spacing, escapeStrings);
    } else {
      if (expr->left->typeType == TYObject
        && !expr->left->var->spec->type->isEnum)
        expr_write(expr->left, 0, spacing, escapeStrings);
      printf(".");
      expr_write(expr->right, 0, spacing, escapeStrings);
    }
    break;

  case tkVarDefn:
    // var x as XYZ = abc... -> becomes an Var and an Expr
    // (to keep location). Send it to var_write.
    assert(expr->var != NULL);
    var_write(expr->var, 0);
    break;

  case tkArrayOpen:
  case tkBraceOpen:
    printf("%s", TokenKind_repr[expr->kind]);
    if (expr->right)
      expr_write(
        expr->right, level, expr->kind != tkArrayOpen, escapeStrings);
    printf("%s", TokenKind_repr[TokenKind_reverseBracket(expr->kind)]);
    break;

  case tkIn:
  case tkNotin:
    // these seem to add precedence parens aruns expr->right if done as
    // normal binops. so ill do them separately here.
    expr_write(expr->left, 0, spacing, escapeStrings);
    printf("%s", TokenKind_srepr[expr->kind]);
    expr_write(expr->right, 0, spacing, escapeStrings);
    break;

  default:
    if (!expr->prec) break;
    // not an operator, but this should be error if you reach here
    bool leftBr
      = expr->left && expr->left->prec && expr->left->prec < expr->prec;
    bool rightBr = expr->right && expr->right->prec
      && expr->right->kind != tkReturn // found in 'or return'
      && expr->right->prec < expr->prec;

    if (expr->kind == tkColon) {
      // expressions like arr[a:x-3:2] should become
      // arr[a:(x-3):2]
      // or list literals [8, 9, 6, 77, sin(c)]
      if (expr->left) switch (expr->left->kind) {
        case tkNumber:
        case tkIdent:
        case tkString:
        case tkColon:
        // case tkMultiDotNumber:
        case tkUnaryMinus: break;
        default: leftBr = true;
        }
      if (expr->right) switch (expr->right->kind) {
        case tkNumber:
        case tkIdent:
        case tkString:
        case tkColon:
        // case tkMultiDotNumber:
        case tkUnaryMinus: break;
        default: rightBr = true;
        }
    }

    if (expr->kind == tkPower && !spacing) putc('(', stdout);

    char lpo = leftBr && expr->left->kind == tkColon ? '[' : '(';
    char lpc = leftBr && expr->left->kind == tkColon ? ']' : ')';
    if (leftBr) putc(lpo, stdout);
    if (expr->left)
      expr_write(expr->left, 0, spacing && !leftBr && expr->kind != tkColon,
        escapeStrings);
    if (leftBr) putc(lpc, stdout);

    printf("%s",
      spacing ? TokenKind_srepr[expr->kind] : TokenKind_repr[expr->kind]);

    char rpo = rightBr && expr->right->kind == tkColon ? '[' : '(';
    char rpc = rightBr && expr->right->kind == tkColon ? ']' : ')';
    if (rightBr) putc(rpo, stdout);
    if (expr->right)
      expr_write(expr->right, 0,
        spacing && !rightBr && expr->kind != tkColon, escapeStrings);
    if (rightBr) putc(rpc, stdout);

    if (expr->kind == tkPower && !spacing) putc(')', stdout);
  }
}

// #define SR(x) #x
// SR("func arty(yu Int, hj Num) String[:,:]")

static void mod_write(Module* module) {
  outfile = stdout; // fopen("linted.jet", "w");

  printf("~ module %s\n", module->name);

  foreach (Import*, import, module->imports) { imp_write(import, 0); }
  puts("");

  foreach (Var*, var, module->scope->locals) {
    var_write(var, 0), puts("");
  }
  puts("");

  foreach (Type*, type, module->types) { type_write(type, 0); }
  foreach (Type*, en, module->enums) { JetEnum_write(en, 0); }
  foreach (Func*, func, module->funcs) { func_write(func, 0); }
  foreach (Test*, test, module->tests) { JetTest_write(test, 0); }

  fclose(outfile);
}
