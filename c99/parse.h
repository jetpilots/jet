
static Expr* exprFromCurrentToken(Parser* parser) {
  Expr* expr = expr_fromToken(&parser->token);
  tok_advance(&parser->token);
  return expr;
}

static Expr* next_token_node(
    Parser* parser, TokenKind expected, const bool ignore_error) {
  if (parser->token.kind == expected) {
    return exprFromCurrentToken(parser);
  } else {
    if (!ignore_error) err_expectedToken(parser, expected);
    return NULL;
  }
}
// these should all be part of tok_ when converted back to C
// in the match case, self->token should be advanced on error
static Expr* par_match(Parser* parser, TokenKind expected) {
  return next_token_node(parser, expected, false);
}

// this returns the match node or null
static Expr* par_trymatch(Parser* parser, TokenKind expected) {
  return next_token_node(parser, expected, true);
}

// just yes or no, simple
static bool par_matches(Parser* parser, TokenKind expected) {
  return (parser->token.kind == expected);
}

static bool par_ignore(Parser* parser, TokenKind expected) {
  bool ret;
  if ((ret = par_matches(parser, expected))) tok_advance(&parser->token);
  return ret;
}

// this is same as match without return
static void par_consume(Parser* parser, TokenKind expected) {
  if (!par_ignore(parser, expected)) err_expectedToken(parser, expected);
}

static char* parseIdent(Parser* parser) {
  if (parser->token.kind != tkIdent) err_expectedToken(parser, tkIdent);
  char* p = parser->token.pos;
  tok_advance(&parser->token);
  return p;
}

// --------------------------------------------------------------------- //
static Expr* parseExpr(Parser* parser) {
  // there are 2 steps to this madness.
  // 1. parse a sequence of tokens into RPN using shunting-yard.
  // 2. walk the rpn stack as a sequence and copy it into a result
  // stack, collapsing the stack when you find nonterminals (ops, func
  // calls, array index, ...)

  // I guess if you want to parse something like if x == 3 x = 4 -- NO WAY
  // NEVER then you have to fold the rpn as soon as you have two consecutive
  // non-ops on the stack and are pushing a third. (x 3) opstack (==)
  // pushing x -> fold. but dont allow this monstrosity! this is why `if x
  // == 3 then x = 4` is needed

  static PtrArray rpn, ops, result;
  int prec_top = 0;
  Expr* p = NULL;
  TokenKind revBrkt = tkUnknown;

  // ******* STEP 1 CONVERT TOKENS INTO RPN

  while (parser->token.kind != tkEOF //
      && parser->token.kind != tkEOL
      && parser->token.kind != tkComment) { // build RPN

    // you have to ensure that ops have a space around them, etc.
    // so don't just skip the one spaces like you do now.
    if (parser->token.kind == tkOneSpace) tok_advance(&parser->token);
    if (parser->token.kind == tkIdent
        && memchr(parser->token.pos, '_', parser->token.matchlen))
      err_invalidIdent(parser); // but continue parsing

    Expr* expr = par_matches(parser, tkParenOpen) //
        ? lparen
        : par_matches(parser, tkParenClose)
            ? rparen
            : expr_fromToken(&parser->token);
    // dont advance yet

    int prec = expr->prec;
    bool rassoc = prec ? expr->rassoc : false;
    char lookAheadChar = tok_peekCharAfter(&parser->token);

    switch (expr->kind) {
    case tkIdent:
      if (memchr(parser->token.pos, '_', parser->token.matchlen))
        err_invalidIdent(parser); // but continue parsing
      expr->slen = parser->token.matchlen;
      switch (lookAheadChar) {
      // TODO: need a general lookahead that skips whitespace.
      // case '!':
      //     if (parser->token.pos[2] != '(') goto defaultCase;
      case '(':
        expr->kind = tkFuncCall, expr->prec = 60, arr_push(&ops, expr);
        break;
      case '[':
        expr->kind = tkSubscript, expr->prec = 60, arr_push(&ops, expr);
        break;
      case ' ':
        if (parser->token.pos[2] != '{') goto defaultCase;
        // otherwise fall through
      case '{':
        expr->kind = tkObjInit, expr->prec = 60, arr_push(&ops, expr);
        break;

      default:
      defaultCase:
        arr_push(&rpn, expr);
        break;
      }
      break;

    case tkParenOpen:
      arr_push(&ops, expr);
      if (!arr_empty(&ops) && arr_topAs(Expr*, &ops)->kind == tkFuncCall)
        arr_push(&rpn, expr);
      if (lookAheadChar == ')') arr_push(&rpn, NULL);
      // for empty func() push null for no args
      if (lookAheadChar == '&') tok_advance(&parser->token);
      // ^ for mutating funcs, & is applied to the first arg on a call
      break;

    case tkArrayOpen:
      arr_push(&ops, expr);
      if (!arr_empty(&ops) && arr_topAs(Expr*, &ops)->kind == tkSubscript)
        arr_push(&rpn, expr);
      if (lookAheadChar == ']') arr_push(&rpn, NULL);
      // for empty arr[] push null for no args
      break;

    case tkBraceOpen:
      arr_push(&ops, expr);
      if (!arr_empty(&ops) && arr_topAs(Expr*, &ops)->kind == tkObjInit)
        arr_push(&rpn, expr);
      if (lookAheadChar == '}') arr_push(&rpn, NULL);
      // for empty Obj {} push null for no args
      break;

    case tkParenClose:
    case tkArrayClose:
    case tkBraceClose:

      revBrkt = TokenKind_reverseBracket(expr->kind);
      if (arr_empty(&ops)) {
        // need atleast the opening bracket of the current kind
        err_syntax(parser, expr, "mismatched bracket");
        goto error;
      }

      else
        while (!arr_empty(&ops)) {
          p = arr_pop(&ops);
          if (p->kind == revBrkt) break;
          arr_push(&rpn, p);
        }

      // tkArrayOpen is a unary op.
      if ((p && p->kind == tkArrayOpen))
        if ((arr_empty(&ops)
                || (arr_top(&rpn)
                    && arr_topAs(Expr*, &ops)->kind != tkSubscript))
            // don't do this if its part of a subscript
            || (arr_empty(&rpn)
                || (arr_top(&rpn)
                    && arr_topAs(Expr*, &rpn)->kind != tkColon)))
          // or aa range. range exprs are handled separately. by
          // themselves they don't need a surrounding [], but for
          // grouping like 2+[8:66] they do.
          arr_push(&rpn, p);

      // a dict literal (another unary op).
      if ((p && p->kind == tkBraceOpen)
          && (arr_empty(&ops)
              || (arr_top(&rpn)
                  && arr_topAs(Expr*, &ops)->kind != tkObjInit)))
        // again, not if it is an object init
        arr_push(&rpn, p);
      // Object { member1 = 3, member3 = "train" }
      // ^^ this is an object init, not a dict
      // { "bing" = 34, "whang" = 33 }
      // ^^ this is a dict

      break;

    case tkYield:
    case tkCheck: arr_push(&ops, expr); break;
    case tkExcl:
      if (arr_empty(&rpn) || arr_topAs(Expr*, &rpn)->kind != tkIdent) {
        err_syntax(parser, expr, "invalid use of '!'");
        // TODO: change error to "invalid use of ! operator or
        // something"
      }
      break;

    case tkReturn:
    case tkBreak:
    case tkContinue:
    case tkThrow:
    case tkCatch:
      // for empty return, push a NULL if there is no expr coming.
      arr_push(&ops, expr);
      if (lookAheadChar == '!' || lookAheadChar == '\n')
        arr_push(&rpn, NULL);
      break;

    case tkUnaryDot:
      expr->kind = tkPeriod;
      expr->unary = false;
      // if (arr_empty(&rpn))
      arr_push(&rpn, NULL);
      // push a NULL which will later be substituted with a dummy var of
      // the inferred enum type.

      // if (!arr_empty(&ops)) {
      //     TokenKind kt = arr_topAs(Expr*, &ops)->kind;
      //     if (!ISIN(2, kt, tkPeriod, tkFuncCall))
      //         arr_push(&rpn, NULL);
      //     // && rpn.used > 1
      //     //                    && arr_topAs(Expr*,
      //     //                    &rpn)->kind != tkIdent))
      //     // fallthru
      // }
      fallthrough;
    default:
      if (prec) {
        if (expr->kind == tkColon) {
          if (arr_empty(&rpn)
              || (!arr_top(&rpn) && !arr_empty(&ops)
                  && arr_topAs(Expr*, &ops)->kind != tkColon)
              || (arr_topAs(Expr*, &rpn)->kind == tkColon
                  && !arr_empty(&ops)
                  && (arr_topAs(Expr*, &ops)->kind == tkComma
                      || arr_topAs(Expr*, &ops)->kind == tkArrayOpen)))
            // TODO: better way to parse :, 1:, :-1, etc.
            // while passing tokens to RPN, if you see a :
            // with nothing on the RPN or comma or [, push a
            // NULL. while unwinding the op stack, if you
            // pop a : and see a NULL or comma on the rpn,
            // push another NULL.
            arr_push(&rpn, &expr_const_0);
          // indicates empty operand
        }
        while (!arr_empty(&ops)) {
          prec_top = arr_topAs(Expr*, &ops)->prec;
          if (!prec_top) break; // left parenthesis
          if (prec > prec_top) break;
          if (prec == prec_top && rassoc) break;
          p = arr_pop(&ops);

          if (p->kind != tkComma && p->kind != tkSemiColon
              && p->kind != tkFuncCall && p->kind != tkSubscript
              && arr_topAs(Expr*, &rpn)
              && arr_topAs(Expr*, &rpn)->kind == tkComma) {
            err_unexpectedToken(parser, "unsupported use of comma");
            // TODO: make this an error of unexpected expr instead
            goto error;
          }

          if (!(p->prec || p->unary) && p->kind != tkFuncCall
              && p->kind != tkColon && p->kind != tkSubscript
              && rpn.used < 2) {
            err_unexpectedToken(parser, "need 2 operands to binary op");
            // TODO: make this errorUnexpectedExpr
            goto error;
          }

          arr_push(&rpn, p);
        }

        if (arr_empty(&rpn) && !expr->unary) {
          err_unexpectedToken(parser, "binary op with no left operand");
          // TODO: again unexpected Expr
          goto error;
        }
        if (expr->kind == tkColon
            && (lookAheadChar == ',' || lookAheadChar == ':'
                || lookAheadChar == ']' || lookAheadChar == ')'))
          arr_push(&rpn, &expr_const_0);

        arr_push(&ops, expr);
      } else {
        arr_push(&rpn, expr);
      }
    }
    tok_advance(&parser->token);
    if (parser->token.kind == tkOneSpace) tok_advance(&parser->token);
  }
exitloop:

  while (!arr_empty(&ops)) {
    p = arr_pop(&ops);

    if (p->kind != tkComma && p->kind != tkFuncCall
        && p->kind != tkSubscript && p->kind != tkArrayOpen
        && arr_topAs(Expr*, &rpn)
        && arr_topAs(Expr*, &rpn)->kind == tkComma) {
      err_unexpectedExpr(parser, arr_topAs(Expr*, &rpn));
      goto error;
    }

    if (!(p->prec || p->unary)
        && (p->kind != tkFuncCall && p->kind != tkSubscript)
        && rpn.used < 2) {
      err_syntax(parser, p, "invalid use of comma");
      goto error;
      // TODO: even if you have more than two, neither of the top
      // two should be a comma
    }

    arr_push(&rpn, p);
  }

  // *** STEP 2 CONVERT RPN INTO EXPR TREE

  Expr* arg;
  for (int i = 0; i < rpn.used; i++) {
    if (!(p = rpn.ref[i])) goto justpush;
    switch (p->kind) {
    case tkFuncCall:
    case tkSubscript:
      if (result.used > 0) {
        arg = arr_pop(&result);
        if (arg && p->kind == tkSubscript) {
          // assert(arg->kind == tkArrayOpen);
          if (arg->kind == tkArrayOpen) arg = arg->right;
        }
        p->left = arg;
      }
      break;

    case tkNumber:
    case tkString:
    case tkRawString:
    case tkRegexp:
    case tkUnits:
    // case tkMultiDotNumber:
    case tkIdent:
    case tkNo:
    case tkNil:
    case tkYes:
    case tkParenOpen:
    case tkComment: break;

    default:
      // everything else is a nonterminal, needs left/right
      if (!p->prec) {
        err_syntax(parser, p, "unexpected token type with no precedence");
        goto error;
      }

      if (arr_empty(&result)) {
        err_syntax(parser, p, "no operands");
        goto error;
      }

      p->right = arr_pop(&result);

      if (!p->unary) {
        if (arr_empty(&result)) {
          err_syntax(parser, p, "need 2 operands to binary op");
          goto error;
        }
        p->left = arr_pop(&result);
      }
    }
  justpush:
    arr_push(&result, p);
  }
  if (!result.used) {
    err_unexpectedToken(parser, "nothing parsed"); //    (parser, p);
    goto error;
  } else if (result.used != 1) {
    if (arr_topAs(Expr*, &result)->kind != tkComment) {
      err_syntax(parser, p, "more than 1 result");
      goto error;
    }
  }

  ops.used = 0;
  rpn.used = 0;
  result.used = 0;
  return result.ref[0];

error:

  while (parser->token.pos < parser->end
      && (parser->token.kind != tkEOL && parser->token.kind != tkComment
          && parser->token.kind != tkEOF))
    tok_advance(&parser->token);

  if (ops.used) {
    eputs("ops: [ ");
    for (int i = 0; i < ops.used; i++)
      eprintf("%s ", TokenKind_repr[((Expr*)ops.ref[i])->kind]);
    eputs("];;");
  }

  if (rpn.used) {
    eputs("rpn: [ ");
    for (int i = 0; i < rpn.used; i++)
      if (!rpn.ref[i])
        eputs("NUL ");
      else {
        Expr* e = rpn.ref[i];
        eprintf("%.*s ", 32, e->prec ? TokenKind_repr[e->kind] : e->str);
      }
    eputs("];;");
  }

  if (result.used) {
    eputs("result: [ ");
    for (int i = 0; i < result.used; i++)
      if (!result.ref[i])
        eputs("NUL ");
      else {
        Expr* e = result.ref[i];
        eprintf("%.*s ", 32, e->prec ? TokenKind_repr[e->kind] : e->str);
      }
    eputs("];;");
  }

  if (p) {
    eprintf("p: %.*s ", 32, p->prec ? TokenKind_repr[p->kind] : p->str);
    // eprintf("");
  }
  eputs("\n");

  ops.used = 0; // "reset" stacks
  rpn.used = 0;
  result.used = 0;
  return NULL;
}

// --------------------------------------------------------------------- //
static TypeSpec* parseTypeSpec(Parser* parser) {
  parser->token.mergeArrayDims = true;

  TypeSpec* spec = NEW(TypeSpec);
  spec->line = parser->token.line;
  spec->col = parser->token.col;

  if (memchr(parser->token.pos, '_', parser->token.matchlen))
    err_invalidIdent(parser);

  if (!isalpha(*parser->token.pos)) err_invalidIdent(parser);

  spec->name = parseIdent(parser);

  if (par_matches(parser, tkArrayDims)) {
    if (isalpha(*parser->token.pos)) {
      // Dict
    } else {
      for_to_where(i, parser->token.matchlen, parser->token.pos[i] == ':')
          spec->dims++;
      if (!spec->dims) spec->dims = 1;
      spec->collType = spec->dims == 1 ? CTYArray : CTYTensor;
    }
    tok_advance(&parser->token);
  }

  par_ignore(parser, tkUnits);

  assert(parser->token.kind != tkUnits);
  assert(parser->token.kind != tkArrayDims);

  parser->token.mergeArrayDims = false;
  return spec;
}

// --------------------------------------------------------------------- //
static Var* parseVar(Parser* parser) {
  Var* var = NEW(Var);
  var->isVar = (parser->token.kind == tkVar);
  var->isLet = (parser->token.kind == tkLet);

  if (var->isVar) par_consume(parser, tkVar);
  if (var->isLet) par_consume(parser, tkLet);
  if (var->isVar || var->isLet) par_consume(parser, tkOneSpace);

  var->line = parser->token.line;
  var->col = parser->token.col;

  parser->token.mergeArrayDims = true;

  if (memchr(parser->token.pos, '_', parser->token.matchlen))
    err_invalidIdent(parser);
  if (*parser->token.pos < 'a' || *parser->token.pos > 'z')
    err_invalidIdent(parser);
  var->name = parseIdent(parser);

  // if (matches(parser, tkExcl)) {
  //     // ! is only used in function arguments. We set isVar here, but the
  //     // caller parseFunc should set isArg on each of its parsed
  //     arguments.
  //     // Then the linter/emitter know how to generate the var.
  //     var->isVar = true;
  //     tok_advance(&parser->token);
  // }

  int dims = 0;
  // if (matches(parser, tkArrayDims)) {
  //     for (int i = 0; i < parser->token.matchlen; i++)
  //         if (parser->token.pos[i] == ':') dims++;
  //     if (! dims) dims = 1;
  //     tok_advance(&parser->token);
  // }
  parser->token.mergeArrayDims = false;

  if (par_ignore(parser, tkOneSpace) && par_matches(parser, tkIdent))
  //        && par_ignore(parser, tkAs)) {
  //      par_consume(parser, tkOneSpace);
  {
    var->spec = parseTypeSpec(parser);
  } else {
    var->spec = NEW(TypeSpec);
    var->spec->line = parser->token.line;
    var->spec->col = parser->token.col;
    var->spec->name = "";
  }

  par_ignore(parser, tkOneSpace);
  if (par_ignore(parser, tkAssign)) var->init = parseExpr(parser);

  return var;
}

static List(Var) * parseArgs(Parser* parser) {
  List(Var)* args = NULL;
  par_consume(parser, tkParenOpen);
  if (par_ignore(parser, tkParenClose)) return args;

  do {
    Var* arg = parseVar(parser);
    arg->isArg = true;
    li_push(&args, arg);
  } while (par_ignore(parser, tkComma));

  par_consume(parser, tkParenClose);
  return args;
}

static Scope* parseScope(
    Parser* parser, Scope* parent, bool isTypeBody, bool isLoop);

static Scope* parseScopeCases(Parser* parser, Scope* parent) {

  Scope* scope = NEW(Scope);
  scope->parent = parent;
  List(Var)** stmts = &scope->stmts;
  Expr* expr;

  while (parser->token.pos < parser->end) {
    switch (parser->token.kind) {

    case tkCase:
      expr = par_match(parser, tkCase);
      expr->left = parseExpr(parser);
      tok_advance(&parser->token); // trample null
      if (expr->left) resolveVars(parser, expr->left, scope, false);

      expr->body = parseScope(parser, scope, false, false);
      // match's body is a scope full of cases
      // case's body is a scope

      // 'case' and 'else' should never consume the 'end', leave it for
      // 'match' or 'if' resp. see later for 'else' as well

      stmts = li_push(stmts, expr);

      break;

    case tkEOL:
    case tkOneSpace:
    case tkComment: tok_advance(&parser->token); break;

    case tkEnd: goto exitloop;

    default:
      err_unexpectedToken(parser, "expected 'case' or 'end'");
      tok_advance(&parser->token);
    }
  }
exitloop:
  return scope;
}

// --------------------------------------------------------------------- //
static Scope* parseScope(
    Parser* parser, Scope* parent, bool isTypeBody, bool isLoop) {
  Scope* scope = NEW(Scope);

  Var *var = NULL, *orig = NULL;
  Expr* expr = NULL;
  TokenKind tt = tkUnknown;
  Scope* forScope = NULL;

  scope->parent = parent;
  scope->isLoop = isLoop;

  bool startedElse = false;
  bool startedCase = false;

  List(Var)** locals = &scope->locals;
  List(Var)** stmts = &scope->stmts;

  while (parser->token.kind != tkEnd) {

    switch (parser->token.kind) {

    case tkEOF: err_expectedToken(parser, tkUnknown); goto exitloop;

    case tkVar:
    case tkLet: {
      int col = parser->token.col + parser->token.matchlen;
      var = parseVar(parser);
      if (!var)
        continue;
      else
        tok_advance(&parser->token);
      if ((orig = scope_getVar(scope, var->name)))
        err_duplicateVar(parser, var, orig->line, orig->col);

      // resolveType(var->spec, scope);
      if (var->init) resolveVars(parser, var->init, scope, false);
      // resolve BEFORE it is added to the list! in
      // `var x = x + 1` x should not resolve
      // if var->spec is NULL then set the type
      // if it isn't NULL then check the types match
      expr = NEW(Expr);
      expr->kind = tkVarDefn;
      expr->line = var->init ? var->init->line : var->line;
      expr->col = col;
      expr->prec = TokenKind_getPrecedence(tkAssign);
      expr->var = var;

      // and (var->init->prec or var->init->kind == tkIdent))
      // TODO: you actually need to send the PtrList item which is
      // generated in the next line as the topExpr, not the expr
      // itself

      locals = li_push(locals, var);
      stmts = li_push(stmts, expr);
    } break;

    case tkCase: goto exitloop;

    case tkMatch:
      if (isTypeBody) err_invalidTypeMember(parser);
      expr = par_match(parser, tkMatch);
      expr->left = parseExpr(parser);
      tok_advance(&parser->token);
      if (expr->left) resolveVars(parser, expr->left, scope, false);

      expr->body = parseScopeCases(parser, scope);
      // match's body is a scope full of cases
      // case's body is a scope

      // 'case' and 'else' should never consume the 'end', leave it
      // for 'match' or 'if' resp. see later for 'else' as well
      // if (tt == tkMatch) {
      par_consume(parser, tkEnd);
      par_ignore(parser, tkOneSpace);
      par_ignore(parser, tkMatch);
      // }
      stmts = li_push(stmts, expr);

      break;

    case tkElse:
    case tkElif:
      if (!startedElse) goto exitloop;
    case tkIf:
    case tkFor:
    case tkWhile:
      if (isTypeBody) err_invalidTypeMember(parser);
      tt = parser->token.kind;
      expr = par_match(parser, tt);
      expr->left = tt != tkElse ? parseExpr(parser) : NULL;

      // because we are going to be calling resolveVars right now, we
      // need to trample the newline
      tok_advance(&parser->token);

      if (tt == tkFor) {
        // TODO: new par_error
        Var* fvar = NULL;
        if (!expr->left)
          unreachable("Missing for-loop condition at %d:%d\n", expr->line,
              expr->col);
        else {
          if (expr->left->kind != tkAssign)
            unreachable("Invalid for-loop condition: %s\n",
                TokenKind_repr[expr->left->kind]);

          resolveVars(parser, expr->left->right, scope, false);

          fvar = NEW(Var);
          fvar->name = expr->left->left->str;
          fvar->line = expr->left->line;
          fvar->col = expr->left->left->col;
          fvar->isVar = true;
          fvar->init = expr->left->right;
          fvar->spec = NEW(TypeSpec);
          fvar->spec->typeType = TYReal64;

          if ((orig = scope_getVar(scope, fvar->name)))
            err_duplicateVar(parser, fvar, orig->line, orig->col);
        }
        forScope = NEW(Scope);
        if (fvar) li_shift(&forScope->locals, fvar);
        forScope->parent = scope;

      } else if (expr->left) {
        resolveVars(parser, expr->left, scope, false);
      }
      // Mark the scope as a loop scope if it is a 'for' or 'while'.
      bool isLoop = tt == tkFor || tt == tkWhile;
      if (tt == tkFor) {
        expr->body = parseScope(parser, forScope, isTypeBody, isLoop);
      } else {
        expr->body = parseScope(parser, scope, isTypeBody, isLoop);
      }

      if (par_matches(parser, tkElse) || //
          par_matches(parser, tkElif)) {
        startedElse = true;
      } else {
        par_consume(parser, tkEnd);
        par_ignore(parser, tkOneSpace);
        par_ignore(parser, tt == tkElse || tt == tkElif ? tkIf : tt);
      }
      stmts = li_push(stmts, expr);
      break;

    case tkEOL:
    case tkOneSpace: tok_advance(&parser->token); break;

    case tkFunc:
    case tkType:
    case tkEnum:
    case tkTest:
      // in this case, it is probably an error propagating all through the
      // file because an early func or type missed an end. How about we
      // stop parsing the scope here.
      goto exitloop;

    case tkComment:
      if (parser->generateCommentExprs) {
        expr = expr_fromToken(&parser->token);
        stmts = li_push(stmts, expr);
      }
      tok_advance(&parser->token);
      break;

    default:
      expr = parseExpr(parser);
      if (expr && isTypeBody) {
        err_invalidTypeMember(parser);
        expr = NULL;
      }
      if (!expr) break;
      stmts = li_push(stmts, expr);
      tok_advance(&parser->token); // eat the newline
      resolveVars(parser, expr, scope, false);
      break;
    }
  }
exitloop:
  return scope;
}

static Scope* parseEnumBody(Parser* parser, Scope* globScope) {
  Scope* scope = NEW(Scope);
  scope->parent = globScope;
  Expr* expr = NULL;
  Var* var = NULL;
  List(Var)** vars = &scope->locals;
  List(Var)** stmts = &scope->stmts;

  while (parser->token.kind != tkEnd) {
    switch (parser->token.kind) {

    case tkEOF: err_expectedToken(parser, tkUnknown); goto exitloop;

    case tkEOL:
    case tkOneSpace: tok_advance(&parser->token); break;

    case tkComment:
      if (parser->generateCommentExprs) {
        expr = expr_fromToken(&parser->token);
        li_push(&scope->stmts, expr);
      }
      tok_advance(&parser->token);
      break;

    case tkIdent:

      expr = parseExpr(parser);

      if (!expr) break;
      if (expr->kind != tkIdent && expr->kind != tkAssign) {
        err_invalidTypeMember(parser);
        unreachable("%s\n", TokenKind_names[expr->kind]);
        expr = NULL;
      }
      stmts = li_push(stmts, expr);
      tok_advance(&parser->token); // eat the newline

      var = NEW(Var);
      var->spec = NEW(TypeSpec);
      var->line = expr->line;
      var->col = (expr->kind == tkAssign) ? expr->left->col : expr->col;
      var->name = (expr->kind == tkAssign) ? expr->left->str : expr->str;
      var->init = expr->right;
      vars = li_push(vars, var);

      if (expr->kind == tkAssign)
        resolveVars(parser, expr->right, scope, false);

      break;

    default: err_expectedToken(parser, parser->token.kind); goto exitloop;
    }
  }
exitloop:
  return scope;
}

// --------------------------------------------------------------------- //
static List(Var) * parseParams(Parser* parser) {
  par_consume(parser, tkLT);
  List(Var) * params;
  Var* param;
  do {
    param = NEW(Var);
    param->name = parseIdent(parser);
    if (par_ignore(parser, tkAs)) param->spec = parseTypeSpec(parser);
    if (par_ignore(parser, tkAssign)) param->init = parseExpr(parser);
    li_push(&params, param);
  } while (par_ignore(parser, tkComma));
  par_consume(parser, tkGT);
  return params;
}

// --------------------------------------------------------------------- //
static Func* parseFunc(
    Parser* parser, Scope* globScope, bool shouldParseBody) {
  par_consume(parser, tkFunc);
  par_consume(parser, tkOneSpace);
  Func* func = NEW(Func);

  func->line = func->endline = parser->token.line;
  func->col = parser->token.col;

  func->nameLen = parser->token.matchlen;
  if (memchr(parser->token.pos, '_', parser->token.matchlen))
    err_invalidIdent(parser);
  func->name = parseIdent(parser); // parser->token.pos; //
  // tok_advance_notrample(&parser->token);
  func->isDeclare = !shouldParseBody;

  bool mutator = func->name[func->nameLen - 1] == '!';
  if (mutator) func->nameLen--;
  // par_ignore(parser, tkExcl);

  func->args = parseArgs(parser);
  func->argCount = li_count(func->args);

  if (mutator && func->argCount) {
    Var* arg1 = func->args->item;
    arg1->isVar = true;
    arg1->isMutableArg = true;
    func->mutator = true;
  }

  if (par_ignore(parser, tkOneSpace) && par_matches(parser, tkIdent)) {
    // par_consume(parser, tkOneSpace);
    func->spec = parseTypeSpec(parser);
  }
  if (!func->spec) {
    func->spec = NEWW(TypeSpec, //
        .name = "", //
        .line = parser->token.line, //
        .col = parser->token.col //
    );
  }

  Var* ans = NEWW(Var, //
      .name = "ans", //
      .line = func->spec->line, //
      .col = func->spec->col, //
      .isVar = true, //
      // .isMutableArg = true, // NOPE, not needed since it will be returned
      .init = NULL, // FIXME
      .spec = func->spec // NEW(TypeSpec);
  );

  if (shouldParseBody) {
    par_ignore(parser, tkComment);
    par_ignore(parser, tkEOL);

    Scope* funcScope = NEW(Scope);
    funcScope->parent = globScope;
    funcScope->locals = func->args;
    li_shift(&funcScope->locals, ans);
    func->body = parseScope(parser, funcScope, false, false);

    func->endline = parser->token.line;
    par_consume(parser, tkEnd);
    par_ignore(parser, tkOneSpace);
    par_ignore(parser, tkFunc);
  }

  return func;
}

static Func* parseStmtFunc(Parser* parser, Scope* globScope) {
  Func* func = NEW(Func);

  func->line = parser->token.line;
  func->isStmt = true;

  func->nameLen = parser->token.matchlen;
  if (memchr(parser->token.pos, '_', parser->token.matchlen))
    err_invalidIdent(parser);
  func->name = parseIdent(parser);

  bool mutator = func->name[func->nameLen - 1] == '!';
  if (mutator) func->nameLen--;
  // par_ignore(parser, tkExcl);

  func->args = parseArgs(parser);
  func->argCount = li_count(func->args);

  if (mutator && func->argCount) {
    Var* arg1 = func->args->item;
    arg1->isVar = true;
    func->mutator = true;
  }

  par_ignore(parser, tkOneSpace);

  func->spec = NEW(TypeSpec);
  func->spec->line = parser->token.line;
  func->spec->col = parser->token.col;
  func->spec->name = "";

  Expr* ret = exprFromCurrentToken(parser);

  // if you have toplevel code (eg func call) it tends to reach here
  if (ret->kind != tkColEq) return NULL;

  ret->kind = tkReturn;
  ret->unary = true;

  ret->right = parseExpr(parser);
  Scope* scope = NEW(Scope);
  li_push(&scope->stmts, ret);

  Scope* funcScope = NEW(Scope);
  funcScope->locals = func->args;
  scope->parent = funcScope;
  func->body = scope;

  par_consume(parser, tkEOL);
  resolveVars(parser, ret->right, funcScope, false);

  return func;
}

// --------------------------------------------------------------------- //
static Test* parseTest(Parser* parser, Scope* globScope) {
  par_consume(parser, tkTest);
  par_consume(parser, tkOneSpace);
  Test* test = NEW(Test);

  test->line = parser->token.line;

  if (parser->token.kind != tkString && parser->token.kind != tkRawString)
    err_invalidTestName(parser);
  test->name = parser->token.pos + 1;
  tok_advance(&parser->token);

  par_consume(parser, tkEOL);

  test->body = parseScope(parser, NULL, false, false);

  par_consume(parser, tkEnd);
  par_ignore(parser, tkOneSpace);
  par_ignore(parser, tkTest);

  return test;
}

// --------------------------------------------------------------------- //
static JetUnits* parseUnits(Parser* parser) { return NULL; }

// --------------------------------------------------------------------- //
static Type* parseType(
    Parser* parser, Scope* globScope, bool shouldParseBody) {
  Type* type = NEW(Type);

  par_consume(parser, tkType);
  par_consume(parser, tkOneSpace);

  type->line = parser->token.line;
  type->col = parser->token.col;
  type->name = parseIdent(parser);

  if (par_ignore(parser, tkOneSpace) && par_ignore(parser, tkExtends)) {
    par_consume(parser, tkOneSpace);
    type->super = parseTypeSpec(parser);
  }
  par_ignore(parser, tkEOL);

  type->body = parseScope(parser, globScope, true, false);

  type->endline = parser->token.line;
  par_consume(parser, tkEnd);
  par_ignore(parser, tkOneSpace);
  par_ignore(parser, tkType);

  return type;
}

static Type* parseEnum(Parser* parser, Scope* globScope) {
  Type* en = NEW(Type);

  par_consume(parser, tkEnum);
  par_consume(parser, tkOneSpace);

  en->line = parser->token.line;
  en->col = parser->token.col;
  en->isEnum = true;

  // if (memchr(parser->token.pos, '_', parser->token.matchlen))
  //     err_invalidIdent(parser);
  // if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
  //     err_invalidIdent(parser);
  en->name = parseIdent(parser);

  par_consume(parser, tkEOL);

  // if (Typetype_byName(en->name) != TYUnknown) {
  //     // conflicts with a primitive type name
  //     err_duplicateEnum(parser, en, NULL);
  //     return en;
  // }

  en->body = parseEnumBody(parser, globScope);

  par_consume(parser, tkEnd);
  par_ignore(parser, tkOneSpace);
  par_ignore(parser, tkEnum);

  return en;
}

static Import* parseImport(Parser* parser, Module* ownerMod) {
  Import* imp = NEW(Import);
  char* tmp;
  par_consume(parser, tkImport);
  par_consume(parser, tkOneSpace);

  if (memchr(parser->token.pos, '_', parser->token.matchlen))
    err_invalidIdent(parser);

  if (!par_matches(parser, tkIdent)) {
    err_expectedToken(parser, tkIdent);
    return NULL;
  }
  imp->name = parser->token.pos;

  char* cend = imp->name;
  while (*cend && (isalnum(*cend) || *cend == '.')) cend++;

  parser->token.pos = cend;
  tok_detect(&parser->token);

  par_ignore(parser, tkOneSpace);
  if (par_ignore(parser, tkAs)) {

    par_ignore(parser, tkOneSpace);

    assert(par_matches(parser, tkIdent));
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
      err_invalidIdent(parser);

    imp->aliasOffset = parser->token.pos - imp->name;
    parseIdent(parser);

  } else {
    imp->aliasOffset
        = cstr_base(imp->name, '.', parser->token.pos - imp->name)
        - imp->name;
  }

  char endchar = *parser->token.pos;
  *parser->token.pos = 0;
  if (mod_getImportByAlias(ownerMod, imp->name + imp->aliasOffset)
      || mod_getFuncByName(ownerMod, imp->name + imp->aliasOffset)
      || mod_getVar(ownerMod, imp->name + imp->aliasOffset)) {
    unreachable(
        "import name already used: %s", imp->name + imp->aliasOffset);
    imp = NULL;
  }
  *parser->token.pos = endchar;
  // ^ need to restore the nl since it is used for line counting

  // par_ignore(parser, tkOneSpace);

  // if (parser->token.kind != tkComment && parser->token.kind !=
  // tkEOL)
  //     err_unexpectedToken(parser);
  // while (
  //     parser->token.kind != tkComment //
  //     && parser->token.kind != tkEOL//
  //     && parser->token.kind!=tkEOF
  //     )
  //     tok_advance(&parser->token);
  return imp;
}

void analyseModule(Parser* parser, Module* mod);

static Func* type_makeDefaultCtor(Type* type) {
  Func* ctor = NEW(Func);
  ctor->line = type->line;
  ctor->col = 6;
  ctor->isDefCtor = true;
  // Ctors must AlWAYS return a new object.
  // even Ctors with args.
  ctor->returnsNewObjectAlways = true;
  ctor->name = type->name;
  char buf[128];
  int l = snprintf(buf, 128, "%s_new_", type->name);
  ctor->sel = cstr_pndup(buf, l);
  TypeSpec* tspec = spec_new(TYObject, CTYNone);
  tspec->type = type;
  ctor->spec = tspec;
  return ctor;
}

// this func should just be replaced by parseScope handling function type
// etc

//,
//        .init = (Expr[]) { { .kind = tkNumber, .str = "1" } } }
//}
//;

// vno->vno->;
// Var* vyes = NEW(Var);
// vyes->name = "yes";
// vyes->typeType = TYBool;

// static Module* par_lookupModuleByAlias(PtrList* existingModules, char*
// name) {
//     foreach (Module*, mod, existingModules) {
//         // eprintf("lookup %s %s\n", name, mod->name);
//         if (!strcasecmp(name, mod->name)) return mod;
//     }
//     return NULL;
// }
static Module* par_lookupModule(PtrList* existingModules, char* name) {
  foreach (Module*, mod, existingModules)
    if (!strcasecmp(name, mod->name)) return mod;
  return NULL;
}

static Module* parseModule(
    Parser* parser, PtrList** existingModulesPtr, Module* importer) {
  FUNC_ENTRY
  Module* root = NEW(Module); // FIXME RENAME ROOT TO MOD, IT IS NOT ROOT!

  // ret->noext = cstr_noext(cstr_clone(filename));
  root->name
      = cstr_tr_ip(cstr_noext_ip(cstr_clone(parser->filename)), '/', '.');
  root->cname = cstr_tr_ip(cstr_clone(root->name), '.', '_');
  root->Cname = cstr_upper_ip(cstr_clone(root->cname));
  root->filename = parser->filename;

  int l = strlen(root->filename);
  static char buf[512];
  // TODO: improve this later
  root->out_h = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.h",
      cstr_dir_ip(cstr_pclone(root->filename)),
      cstr_base(root->filename, '/', l)));
  root->out_c = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.c",
      cstr_dir_ip(cstr_pclone(root->filename)),
      cstr_base(root->filename, '/', l)));
  root->out_xc = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.x.c",
      cstr_dir_ip(cstr_clone(root->filename)),
      cstr_base(root->filename, '/', l)));
  root->out_o = cstr_pclone(root->out_c);
  root->out_o[strlen(root->out_o) - 1] = 'o';

  // To break recursion, add the root to the existingModules list right
  // away. The name is all that is required for this module to be found
  // later.
  li_shift(existingModulesPtr, root);
  // eprintf("Parsing %s\n", root->name);
  // root->scope = NEW(Scope);

  // Token parser->token = { .pos = data.ref, .end = data.ref + data.len };
  tok_advance(&parser->token); // maybe put this in parser ctor

  Import* imp = NULL;
  // The take away is (for C gen):
  // Every caller who calls append(List) should keep a local List*
  // to follow the list top as items are appended. Each actual append
  // call must be followed by an update of this pointer to its own
  // ->next. Append should be called on the last item of the list, not
  // the first. (it will work but seek through the whole list every
  // time).

  List(Func)** funcs = &root->funcs;
  List(Import)** imports = &root->imports;
  List(Type)** types = &root->types;
  List(Test)** tests = &root->tests;
  List(JetEnum)** enums = &root->enums;
  Scope* gscope = root->scope;
  List(Var)** gvars = &gscope->locals; // globals
  List(Expr)** gexprs = &gscope->stmts; // globals

  // for_to(i, countof(vnoyes))
  // gvars = li_push(gvars, expr_const_empty);
  // gvars = li_push(gvars, expr_const_yes);
  // gvars = li_push(gvars, expr_const_no);
  // gvars = li_push(gvars, expr_const_nil);

  while (parser->token.kind != tkEOF) {

    if (parser->mode == PMTokenize) {
      printf("%s %2d %3d %3d %-20s\t%.*s\n", parser->filename,
          parser->token.line, parser->token.col, parser->token.matchlen,
          TokenKind_names[parser->token.kind],
          parser->token.kind == tkEOL ? 0 : parser->token.matchlen,
          parser->token.pos);
      tok_advance(&parser->token);
      continue;
    }

    switch (parser->token.kind) {

    case tkDecl:
      tok_advance(&parser->token);
      par_consume(parser, tkOneSpace);
      if (parser->token.kind == tkFunc) {
        Func* func = parseFunc(parser, gscope, false);
        func->isDeclare = true;
        funcs = li_push(funcs, func);
      }
      if (parser->token.kind == tkType) {
        Type* type = parseType(parser, gscope, false);
        type->isDeclare = true;
        types = li_push(types, type);
      }
      break;

    case tkFunc:
      funcs = li_push(funcs, parseFunc(parser, gscope, true));
      break;

    case tkEnum: {
      Type* en = parseEnum(parser, gscope);
      enums = li_push(enums, en);
      // add a global of the corresponding type so that it can be used
      // to access members.
      assert(en);
      Var* enumv = NEWW(Var, //
          .name = en->name, //
          .line = en->line, //
          .col = en->col, //
          .used = 1,
          .spec = NEWW(TypeSpec, //
              .typeType = TYObject, //
              .type = en // doesn't increase en's usage
              ) //
      );
      gvars = li_push(gvars, enumv);
    }

    break;

    case tkType: {
      Type* type = parseType(parser, gscope, true);
      types = li_push(types, type);

      // create default constructor
      Func* ctor = type_makeDefaultCtor(type);
      funcs = li_push(funcs, ctor);

      // create some extra function declares
      char* defFuncs[] = { "json", "print", "describe" };

      for (int i = 0; i < countof(defFuncs); i++) {
        Func* func = func_createDeclWithArg(defFuncs[i], NULL, type->name);
        func->line = type->line;
        func->intrinsic = true;
        funcs = li_push(funcs, func);
      }
    } break;

    case tkImport:
      imp = parseImport(parser, root);
      if (imp) {

        imports = li_push(imports, imp);

        imp->mod = par_lookupModule(*existingModulesPtr, imp->name);
        if (!imp->mod) {
          size_t len = strlen(imp->name) + 5;
          char* filename = malloc(len);
          filename[len - 1] = 0;
          strcpy(filename, imp->name);
          cstr_tr_ip_len(filename, '.', '/', len - 5);
          strcpy(filename + len - 5, ".jet");
          eprintf("%s needs %s, parsing\n", root->filename, filename);
          // later you can have a fancier routine that figures out
          // the file name from a module name
          Parser* subParser = par_fromFile(filename, true, parser->mode);
          // parseModule returns the parsed module, and adds dependent
          // modules to the second argument. dependencies are only
          // parsed once on first use.
          // FIXME: these parsers will LEAK!
          if (subParser) {
            if ((imp->mod
                    = parseModule(subParser, existingModulesPtr, NULL)))
              li_shift(&imp->mod->importedBy, root);
          }
        }
      }
      break;

    case tkTest: tests = li_push(tests, parseTest(parser, gscope)); break;

    case tkVar:
    case tkLet:
      // TODO: add these to exprs
      {
        int col = parser->token.col + parser->token.matchlen;
        Var *var = parseVar(parser), *orig;
        if (!var) {
          tok_advance(&parser->token);
          continue;
        }
        if ((orig = scope_getVar(gscope, var->name)))
          err_duplicateVar(parser, var, orig->line, orig->col);
        Import* imp = mod_getImportByAlias(root, var->name);
        Func* fnc = mod_getFuncByName(root, var->name);
        if (imp) err_duplicateVar(parser, var, imp->line, imp->col);
        if (fnc) err_duplicateVar(parser, var, fnc->line, 5);

        if (var->init) resolveVars(parser, var->init, gscope, false);
        var->isLet = true;
        var->isVar = false;
        gvars = li_push(gvars, var);

        // TODO: validation should raise issue if var->init is
        // missing
        Expr* expr = NEW(Expr);
        expr->kind = tkVarDefn;
        expr->line = var->init ? var->init->line : parser->token.line;
        expr->col = col;
        expr->prec = TokenKind_getPrecedence(tkAssign);
        expr->var = var;

        // and (var->init->prec or var->init->kind == tkIdent))
        // TODO: you actually need to send the PtrList item which is
        // generated in the next line as the topExpr, not the expr
        // itself
        if (var->init) resolveVars(parser, var->init, gscope, false);

        gexprs = li_push(gexprs, expr);
      }

      break;
    case tkEOL: *(parser->token.pos) = 0;
    // fallthrough
    case tkComment:
    // TODO: add line comment to module exprs
    case tkOneSpace: tok_advance(&parser->token); break;
    case tkIdent: // stmt funcs: f(x) := f(y, w = 4) etc.
      if (tok_peekCharAfter(&parser->token) == '(') {
        funcs = li_push(funcs, parseStmtFunc(parser, gscope));
        break;
      }
    default:
      err_unexpectedToken(parser, "expected a keyword here");
      while (!ISIN(3, parser->token.kind, tkEOL, tkComment, tkEOF))
        tok_advance(&parser->token);
    }
  }

  // Add some default functions "built-ins"
  // TODO: move this into a function later
  char* defTypes[] = { "String", "Number", "Boolean" };
  char* defFuncs[] = { "json", "print", "describe" };
  char* retTypes[countof(defFuncs)] = {}; // fill these for non-void funcs

  for (int j = 0; j < countof(defTypes); j++)
    for (int i = 0; i < countof(defFuncs); i++) {
      Func* func
          = func_createDeclWithArg(defFuncs[i], retTypes[i], defTypes[j]);
      func->intrinsic = true;
      funcs = li_push(funcs, func);
    }
  root->nlines = parser->token.line;

  // do some analysis that happens after the entire module is loaded
  mod_analyse(parser, root);
  // if (importer) li_shift(&importer->importedBy, root);
  return root;
}
