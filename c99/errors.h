#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                    \
  {                                                                        \
    eprintf(str, __VA_ARGS__);                                             \
    exit(1);                                                               \
  }

#define noPoison 0
static void par__errHeaderWithLoc(
    Parser* parser, int line, int col, int len) {
  eprintf("%s%s:%d:%d-%d: error: #%d;;", RELF(parser->filename), line, col,
      col + len, parser->issues.errCount + 1);
}
static void par__errHeader(Parser* parser) {
  par__errHeaderWithLoc(parser, parser->token.line, parser->token.col,
      parser->token.col + parser->token.matchlen);
}
static void par__warnHeaderWithLoc(
    Parser* parser, int line, int col, int len) {
  eprintf("%s%s:%d:%d-%d: warning: #%d;;", RELF(parser->filename), line,
      col, col + len, ++parser->issues.warnCount);
}

static void par__hintHeaderWithLoc(
    Parser* parser, int line, int col, int len) {
  eprintf("%s%s:%d:%d-%d: hint: #%d;;", RELF(parser->filename), line, col,
      col + len, ++parser->issues.warnCount);
}

static void par__errHeaderWithExpr(Parser* parser, Expr* expr) {
  int len;
  int col = expr->col;
  switch (expr->kind) {
  case tkIdent:
  case tkFuncCall:
  case tkSubscript:
  case tkObjInit:
  case tkNumber: len = strlen(expr->str); break;
  case tkString:
  case tkRawString:
  case tkRegexp: len = strlen(expr->str) + 1; break;

  case tkIdentR:
  case tkSubscriptR: len = strlen(expr->var->name); break;
  case tkFuncCallR: len = strlen(expr->func->name); break;
  case tkVarDefn:
    len = 4 + strlen(expr->var->name);
    col -= 3;
    break;

  //    case tkFor:
  // case tkWhile:
  // case tkIf:
  // case tkEnd:
  // case tkEnum:
  // case tkMatch:
  // case tkCase:
  // case tkFunc:
  // case tkDecl:
  // case tkTest:
  // case tkCheck:
  // case tkNot:
  // case tkNotin:
  // case tkAnd:
  // case tkYes:
  // case tkNo:
  // case tkNil:
  // case tkOr:
  // case tkIn:
  // case tkDo:
  // case tkThen:
  // case tkAs:
  // case tkElse:
  // case tkElif:
  // case tkType:
  // case tkReturn:
  // case tkYield:
  // case tkExtends:
  // case tkVar:
  // case tkLet:
  // case tkImport:
  default: len = strlen(TokenKind_repr[expr->kind]); break;
  }
  par__errHeaderWithLoc(
      parser, expr->line ? expr->line : parser->token.line, col, len);
}
static void err_increment(Parser* parser) {
  if (++parser->issues.errCount < parser->issues.errLimit) return;
  if (parser->mode == PMLint) {
    fatal(
        "\n*** too many errors (%d), quitting\n", parser->issues.errLimit);
  } else {
    fatal("\n*** %s has errors, please lint it first.\n", parser->filename);
  }
}
static const char* const carets
    = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      "^^^^"
      "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
#define _PRLINE(line)                                                      \
  if (line > 0 && line <= parser->orig.used)                               \
    eprintf("%4d | %s\n", line + offs,                                     \
        (char*)parser->orig.ref[line + offs - 1]);
void _PRREDLINE(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
  char* c = parser->orig.ref[line + offs - 1];
  if (!col) len = strlen(c), col = 1;
  eprintf("%4d | %.*s", line + offs, col - 1, c);
  eprintf("%.*s", len, c + col - 1);
  eprintf("%s\n", c + col + len - 1);
  eprintf("       %.*s%.*s %s\n", col - 1, spaces, len, carets, msg);
}

void par__printSourceLinesWithOffset(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
  _PRLINE(line - 2)
  _PRLINE(line - 1)
  _PRREDLINE(parser, line, col, len, offs, msg);
  _PRLINE(line + 1)
  _PRLINE(line + 2)

  eputs("\n\n");
}

void par__printSourceLines(
    Parser* parser, int line, int col, int len, char* msg) {
  par__printSourceLinesWithOffset(parser, line, col, len, 0, msg);
}

static void err_expectedToken(Parser* parser, TokenKind expected) {
  par__errHeader(parser);

  eprintf("expected '%s' (%s) but found '%s'\n", TokenKind_repr[expected],
      TokenKind_names[expected] + 2, TokenKind_repr[parser->token.kind]);

  char msg[128];
  msg[127] = 0;
  snprintf(msg, 127, "expected '%s' here", TokenKind_repr[expected]);

  par__printSourceLines(parser, parser->token.line, parser->token.col,
      parser->token.matchlen, msg);

  // when you have an unexpected token on one line, the rest are also
  // going to be unexpected. so skip to the next newline.
  while (!par_matches(parser, tkEOL) && !par_matches(parser, tkEOF))
    tok_advance(&parser->token);

  parser->issues.hasParseErrors = 1;
  err_increment(parser);
}

bool isKeywordKind(TokenKind kind) {
  return __tk__keywords__begin < kind && kind < __tk__keywords__end;
  // switch (kind) {
  // case tkFor:
  // case tkWhile:
  // case tkIf:
  // case tkEnd:
  // case tkEnum:
  // case tkMatch:
  // case tkCase:
  // case tkFunc:
  // case tkDecl:
  // case tkTest:
  // case tkCheck:
  // case tkNot:
  // case tkNotin:
  // case tkAnd:
  // case tkYes:
  // case tkNo:
  // case tkNil:
  // case tkOr:
  // case tkIn:
  // case tkDo:
  // case tkThen:
  // case tkAs:
  // case tkElse:
  // case tkElif:
  // case tkType:
  // case tkReturn:
  // case tkResult:
  // case tkExtends:
  // case tkVar:
  // case tkLet:
  // case tkImport: return true;
  // default:;
  // }
  // return false;
}

static void err_syntax(Parser* parser, Expr* expr, char* what) {

  par__errHeaderWithExpr(parser, expr);
  eprintf("invalid syntax at '%s' (%s);;", TokenKind_repr[expr->kind],
      what); //,

  if (isKeywordKind(expr->kind))
    eputs("info: keywords cannot be used as identifiers;;");

  parser->issues.hasParseErrors = 1;
  err_increment(parser);
}

static void err_invalidIdent(Parser* parser) {
  par__errHeader(parser);
  eprintf(
      "invalid name '%.*s'\n", parser->token.matchlen, parser->token.pos);
  err_increment(parser);
}

static void err_invalidTypeName(Parser* parser, Type* type) {
  par__errHeader(parser);
  eprintf("invalid type name '%s';;"
          "must be capitalized and without underscores\n",
      type->name);
  err_increment(parser);
}
static void err_invalidTypeMember(Parser* parser) {
  par__errHeader(parser);
  eputs("invalid member\n");
  err_increment(parser);
}

static void err_unrecognizedVar(Parser* parser, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);
  eprintf("unknown variable '%.*s'\n", 64, expr->str);

  par__printSourceLines(
      parser, expr->line, expr->col, strlen(expr->str), "unknown variable");

  err_increment(parser);
}

static void err_importNotFound(Parser* parser, Import* impo) {
  par__errHeaderWithLoc(parser, impo->line, impo->col, strlen(impo->name));
  eprintf("cannot load module '%.*s'\n", 64, impo->name);
  err_increment(parser);
}

static void err_unrecognizedMember(Parser* parser, Type* type, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);
  eprintf("%s '%s' has no member '%s';;", type->isEnum ? "enum" : "type",
      type->name, expr->str);

  if (type->body) {
    foreach (Var*, var, type->body->locals) {
      unsigned l1 = strlen(var->name);
      unsigned l2 = strlen(expr->str);

      if (leven(var->name, expr->str, l1, l2) <= 3)
        eprintf("did you mean: '%s'?;;", var->name);
    }
    eputs("members: ");
    foreachn(Var*, var, vi, type->body->locals) if (var->name && *var->name)
        eprintf("%s%s", var->name, vi->next ? ", " : "");
    eputs("\n");
  }
  err_increment(parser);
}

static void warn_unusedArg(Parser* parser, Var* var) {
  if (!parser->issues.warnUnusedArg) return;
  par__warnHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("unused or unnecessary argument '%s'\n", var->name);
}

static void warn_unusedVar(Parser* parser, Var* var) {
  if (!parser->issues.warnUnusedVar) return;
  par__warnHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("unused or unnecessary variable '%s'\n", var->name);
}

static void hint_varRefineScope(Parser* parser, Var* var) {
  if (!parser->issues.warnUnusedVar) return;
  par__hintHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("variable '%s' can be moved to an inner scope\n", var->name);
}

static void hint_varEscapes(Parser* parser, Var* var) {
  // if (!parser->issues.hints) return;
  par__hintHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("var '%s' escapes func\n", var->name);
}

static void hint_varPromoteScope(Parser* parser, Var* var) {
  // if (!parser->issues.hints) return;
  par__hintHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("var '%s' escapes scope, will be promoted up %d levels\n",
      var->name, var->promote);
}

static void warn_unusedFunc(Parser* parser, Func* func) {
  if (!parser->issues.warnUnusedFunc) return;
  par__warnHeaderWithLoc(parser, func->line, func->col, strlen(func->name));
  eprintf("unused or unnecessary function '%s';;selector: '%s'\n",
      func->name, func->sel);
}

static void warn_unusedType(Parser* parser, Type* type) {
  if (!parser->issues.warnUnusedType) return;
  par__warnHeaderWithLoc(parser, type->line, type->col, strlen(type->name));
  eprintf("unused or unnecessary type '%s'\n", type->name);
}

static void warn_sameExpr(Parser* parser, Expr* e1, Expr* e2) {
  if (!parser->issues.warnUnusedType) return;
  par__warnHeaderWithLoc(parser, e1->line, e1->col, 1);
  eprintf("CSE candidate '%s' for %d:%d\n", TokenKind_repr[e1->kind],
      e2->line, e2->col);

  par__warnHeaderWithLoc(parser, e2->line, e2->col, 1);
  eprintf("CSE candidate '%s' for %d:%d\n", TokenKind_repr[e2->kind],
      e1->line, e2->col);
}

static void err_duplicateVar(Parser* parser, Var* var, int line, int col) {
  par__errHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
  eprintf("duplicate variable '%s' already declared at %s%s:%d:%d\n",
      var->name, RELF(parser->filename), line, col);
  err_increment(parser);
}

static void err_duplicateType(Parser* parser, Type* type, Type* orig) {
  par__errHeaderWithLoc(parser, type->line, type->col, strlen(type->name));
  if (orig)
    eprintf("duplicate type '%s' already declared at %s%s:%d:%d\n",
        type->name, RELF(parser->filename), orig->line, orig->col);
  else
    eprintf(
        "invalid type name '%s' refers to a built-in type\n", type->name);
  err_increment(parser);
}

static void err_duplicateEnum(Parser* parser, Type* en, Type* orig) {
  par__errHeaderWithLoc(parser, en->line, en->col, strlen(en->name));
  if (orig)
    eprintf("duplicate enum '%s' already declared at %s%s:%d:%d\n",
        en->name, RELF(parser->filename), orig->line, orig->col);
  else
    eprintf("invalid enum name '%s' refers to a built-in type\n", en->name);
  err_increment(parser);
}

static void err_typeInheritsSelf(Parser* parser, Type* type) {
  par__errHeader(parser);
  eprintf("type inherits from self %s at %s%s:%d:%d\n", type->name,
      RELF(parser->filename), type->line, type->col);
  err_increment(parser);
}

static void err_ctorHasType(Parser* parser, Func* func, Type* orig) {
  par__errHeader(parser);
  eprintf("constructor needs no return "
          "type: %s at %s%s:%d:%d\n"
          "             type declared at %s%s:%d:%d\n"
          "             remove the return type specification\n",
      func->name, RELF(parser->filename), func->line, 1,
      RELF(parser->filename), orig->line, orig->col);
  err_increment(parser);
}

static void warn_ctorCase(Parser* parser, Func* func) {
  Type* orig = func->spec->type;
  par__errHeader(parser);
  eprintf("\n(%d) warning: wrong case "
          "%s for constructor at %s%s:%d:%d\n"
          "             type declared at %s%s:%d:%d\n"
          "             change it to %s or lint the "
          "file\n",
      ++parser->issues.warnCount, func->name, RELF(parser->filename),
      func->line, 1, RELF(parser->filename), orig->line, orig->col,
      orig->name);
}

static void err_duplicateFunc(Parser* parser, Func* func, Func* orig) {
  par__errHeaderWithLoc(parser, func->line, 6, strlen(func->name));
  eprintf("duplicate function "
          "'%s' already declared at %s%s:%d:%d with selector %s\n",
      func->name, RELF(parser->filename), orig->line, 6, func->psel);
  err_increment(parser);
}

static void err_duplicateTest(Parser* parser, Test* test, Test* orig) {
  par__errHeaderWithLoc(parser, test->line, 5, strlen(test->name));
  eprintf("duplicate test \"%s\" already declared at %s%s:%d:%d\n",
      test->name, RELF(parser->filename), orig->line, 1);
  err_increment(parser);
}

static void err_unrecognizedFunc(
    Parser* parser, Expr* expr, char* selector) {
  if (noPoison && *selector == '<') return; // invalid type; already error'd
  par__errHeaderWithExpr(parser, expr);

  eprintf(
      "cannot resolve call to '%s' (selector: %s);;", expr->str, selector);

  // par__printSourceLines(
  //     parser, expr->line, expr->col, strlen(expr->str), "unknown
  //     function");

  // eprintf("info: no function with selector '%s'\n", selector);

  err_increment(parser);
}

// when a func is not exactly found based on selector, issue a warning and
// take the closest matching func by type. Generally the linter will add
// labels automatically so this warning will only appear on unlinted files.
static void warn_unrecognizedSelector(
    Parser* parser, Expr* expr, char* selector, Func* selected) {
  if (noPoison && *selector == '<') return; // invalid type; already error'd
  par__warnHeaderWithLoc(parser, expr->line, expr->col, strlen(expr->str));

  eprintf("no exact match for function '%s' with selector '%s', taking "
          "closest match '%s' (at %s%s:%d:%d)\n",
      expr->str, selector, selected->sel, RELF(parser->filename),
      expr->line, expr->col);

  par__printSourceLines(
      parser, expr->line, expr->col, strlen(expr->str), "unknown function");

  // eprintf("info: no function with selector '%s'\n", selector);

  // err_increment(parser);
}

static void warn_templateHit(
    Parser* parser, Expr* expr, char* selector, Func* selected) {
  if (noPoison && *selector == '<') return; // invalid type; already error'd
  par__warnHeaderWithLoc(parser, expr->line, expr->col, strlen(expr->str));

  eprintf("no exact match for function '%s' with selector '%s', made "
          "'%s' from template at %s%s:%d:%d\n",
      expr->str, selector, selected->sel, RELF(parser->filename),
      selected->line, selected->col);

  // par__printSourceLines(
  //     parser, expr->line, expr->col, strlen(expr->str), "unknown
  //     function");

  // eprintf("info: no function with selector '%s'\n", selector);

  // err_increment(parser);
}

static void err_stringInterp(Parser* parser, Expr* expr, char* pos) {
  par__errHeader(parser);
  eprintf("\n ERROR                                    "
          "           "
          "                       \n %s%s:%d:%d:\n There is "
          "a syntax "
          "error within"
          " this string:\n"
          "     %s\"\n"
          " $ is used to specify a variable whose value is to be "
          "interpolated into the\n"
          " string. If you want a literal $ sign in the string, "
          "write it as "
          "\\$.\n",
      RELF(parser->filename), expr->line,
      expr->col + (int)(pos - expr->str), expr->str);
  err_increment(parser);
}

static void err_callingFuncWithVoid(Parser* parser, Expr* expr, Expr* arg) {
  par__errHeader(parser);
  eprintf("\n ERROR                                    "
          "           "
          "                       \n %s%s:%d:%d:\n The "
          "%s function does not return a value.\n"
          " You cannot use it as an argument in the call to "
          "%s.\n",
      RELF(parser->filename), expr->line, expr->col, arg->func->name,
      expr->str);
  err_increment(parser);
}

static void err_inheritanceCycle(Parser* parser, Type* type) {
  par__errHeader(parser);
  eprintf("\n ERROR                                    "
          "           "
          "                       \n %s%s:%d:%d:\n Type "
          "%s has a cycle in its inheritance graph.",
      RELF(parser->filename), type->line, type->col, type->name);
  Type* super = type->super->type;
  eputs("");
  do {
    eprintf("\n extends %s (defined at %s%s:%d:%d)", super->name,
        RELF(parser->filename), super->line, super->col);
    if (super == super->super->type || super == type) {
      if (type != super) eprintf("\n extends %s", super->name);
      break;
    }
    super = super->super->type;
  } while (1);
  eputs("\n ...\n");
  err_increment(parser);
}

static void err_constructorHasCycle(Parser* parser, Type* type) {
  par__errHeader(parser);
  eprintf("\n ERROR                                    "
          "           "
          "                       \n %s%s:%d:%d:\n Type "
          "%s has an endless cycle in its "
          "initialization.\n",
      RELF(parser->filename), type->line, type->col, type->name);
  err_increment(parser);
}

static void err_argsCountMismatch(Parser* parser, Expr* expr) {
  assert(expr->kind == tkFuncCallR);
  par__errHeader(parser);
  eprintf("arg count mismatch for "
          "%s at %s%s:%d:%d\n"
          "          have %d args, need %d, func defined at "
          "%s%s:%d\n",
      expr->func->name, RELF(parser->filename), expr->line, expr->col,
      expr_countCommaList(expr->left), expr->func->argCount,
      RELF(parser->filename), expr->func->line);
  err_increment(parser);
}

static void err_indexDimsMismatch(Parser* parser, Expr* expr, int nhave) {
  assert(expr->kind == tkSubscriptR);
  if (expr->var->spec->typeType == TYError
      || expr->var->spec->typeType == TYUnknown)
    return;
  // ^ type resolution failed (and must have raised error) so
  // don't process further
  int reqdDims = expr->var->spec->dims;
  par__errHeaderWithExpr(parser, expr);
  if (!reqdDims)
    eprintf("can't index a scalar '%s' with %d dims (defined at %s%s:%d)\n",
        expr->var->name, nhave, RELF(parser->filename),
        expr->var->spec->line);
  else {
    eprintf("dims mismatch for '%s': have %d indexes, need %d (defined "
            "at %s%s:%d)\n",
        expr->var->name, nhave, reqdDims, RELF(parser->filename),
        expr->var->spec->line);
  }
  err_increment(parser);
}

static void err_missingInit(Parser* parser, Expr* expr) {
  assert(expr->kind == tkVarDefn);
  par__errHeaderWithExpr(parser, expr);
  eprintf("missing initializer for "
          "%s\n",
      expr->var->name);
  parser->issues.hasParseErrors = 1;
  err_increment(parser);
}

static void err_unrecognizedType(Parser* parser, TypeSpec* spec) {
  par__errHeaderWithLoc(parser, spec->line, spec->col, strlen(spec->name));
  eprintf("unknown typespec '%s'\n", spec->name);
  err_increment(parser);
}

static void err_unrecognizedCtor(Parser* parser, Func* func) {
  par__errHeaderWithLoc(parser, func->line, 5, strlen(func->name));
  eprintf("unknown type '%s' for constructor\n", func->name);
  err_increment(parser);
}

static void err_invalidTestName(Parser* parser) {
  par__errHeader(parser);
  eprintf("invalid test name "
          "'%.*s'; must be a string\n",
      parser->token.matchlen, parser->token.pos);
  err_increment(parser);
}

static void err_typeMismatchBinOp(Parser* parser, Expr* expr) {
  // if one of the types is "<invalid>", an error has already been
  // reported for it; so don't bother
  const char* leftTypeName = expr_typeName(expr->left);
  const char* rightTypeName = expr_typeName(expr->right);
  if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
  par__errHeaderWithExpr(parser, expr);
  eprintf("type mismatch; can't do '%s %s %s'\n", leftTypeName,
      TokenKind_repr[expr->kind], rightTypeName);
  err_increment(parser);
}

static void err_typeMismatch(Parser* parser, Expr* e1, Expr* e2) {
  const char* leftTypeName = expr_typeName(e1);
  const char* rightTypeName = expr_typeName(e2);
  if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
  par__errHeaderWithExpr(parser, e2);
  eprintf("type mismatch: '%s' here must be '%s' (from %s%s:%d:%d)\n",
      rightTypeName, leftTypeName, RELF(parser->filename), e1->line,
      e1->col);
  err_increment(parser);
}

static void err_typeWrong(Parser* parser, Expr* e1, TypeTypes correct) {
  const char* leftTypeName = expr_typeName(e1);
  const char* rightTypeName = Typetype_name(correct);
  if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
  par__errHeaderWithExpr(parser, e1);
  eprintf("expected an expression of type '%s', not '%s'\n", rightTypeName,
      leftTypeName);
  err_increment(parser);
}

static void err_initMismatch(Parser* parser, Expr* expr) {
  const char* leftTypeName = spec_name(expr->var->spec);
  const char* rightTypeName = expr_typeName(expr->var->init);
  //    if (*leftTypeName == '<' or *rightTypeName == '<') return;

  // for collections, RHS is allowed to be an empty [] or {} to
  // indicate that the array starts out empty. Any-dim arrays can
  // be initialized with []. e.g. var arr[:,:,:] as Number = [] of
  // course, the LHS must have a type, you cannot have e.g. var
  // arr[:,:,:] = [] that would be an error.
  if ((expr->var->init->kind == tkArrayOpen
          || expr->var->init->kind == tkBraceOpen)
      && expr->var->spec->collType != CTYNone && !expr->var->init->right
      && expr->var->spec->typeType != TYUnknown)
    return;
  par__errHeaderWithExpr(parser, expr);

  eprintf("can't init '%s' with an expression of type '%s' (hint: just "
          "remove the type annotation.)\n",
      leftTypeName, rightTypeName);
  err_increment(parser);
}

static void err_initDimsMismatch(Parser* parser, Expr* expr, int dims) {

  par__errHeaderWithExpr(parser, expr);
  eprintf("can't init %dD array '%s' with a %dD literal. "
          "(hint: just remove the dimension specification.)\n",
      expr->var->spec->dims, expr->var->name, dims);
  err_increment(parser);
}

static void err_binOpDimsMismatch(Parser* parser, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);

  eprintf("can't do '%dD array %s %dD array'\n", expr->left->dims,
      TokenKind_repr[expr->kind], expr->right->dims);
  err_increment(parser);
}

static void err_readOnlyVar(Parser* parser, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);
  eprintf("can't mutate read-only variable '%s'\n", expr->var->name);
  err_increment(parser);
}

static void err_noEnumInferred(Parser* parser, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);
  eprintf("could not infer enum type for '.%s'\n", expr->str);
  err_increment(parser);
}

static void err_invalidTypeForOp(Parser* parser, Expr* expr) {
  if (expr->left->typeType == TYError || expr->right->typeType == TYError)
    return;
  par__errHeaderWithExpr(parser, expr);

  eprintf("invalid types for '%s'\n", TokenKind_repr[expr->kind]);
  err_increment(parser);
}

static void err_argTypeMismatch(Parser* parser, Expr* expr, Var* var) {
  par__errHeaderWithExpr(parser, expr);

  eprintf("type '%s' for argument '%s' should be '%s' instead (from "
          "%s%s:%d:%d)\n",
      expr_typeName(expr), var->name, spec_name(var->spec),
      RELF(parser->filename), var->line, var->col);
  // parser->issues.hasParseErrors = 1;
  err_increment(parser);
}

static void err_argLabelMismatch(Parser* parser, Expr* expr, Var* var) {
  par__errHeaderWithExpr(parser, expr);

  eprintf("label '%s' should be '%s' instead (from %s%s:%d:%d)\n",
      expr->str, var->name, RELF(parser->filename), var->line, var->col);

  err_increment(parser);
}

static void err_unexpectedToken(Parser* parser, char* msg) {
  par__errHeader(parser);

  eprintf("unexpected token '%.*s'\n", parser->token.matchlen,
      parser->token.pos);

  par__printSourceLines(parser, parser->token.line, parser->token.col,
      parser->token.matchlen, msg);

  // when you have an unexpected token on one line, the rest are also going
  // to be unexpected. so skip to the next newline.
  while (!par_matches(parser, tkEOL) && !par_matches(parser, tkEOF))
    tok_advance(&parser->token);

  parser->issues.hasParseErrors = 1;
  err_increment(parser);
}

static void err_unexpectedExpr(Parser* parser, Expr* expr) {
  par__errHeaderWithExpr(parser, expr);
  eprintf("unexpected expr '%s' (%s)\n",
      expr->prec ? TokenKind_repr[expr->kind] : expr->str,
      TokenKind_names[expr->kind] + 2);
  err_increment(parser);
}
