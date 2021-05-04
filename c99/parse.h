
#pragma mark - PARSE EXPR
static Expr* parseExpr(Parser* parser) {
    // there are 2 steps to this madness.
    // 1. parse a sequence of tokens into RPN using shunting-yard.
    // 2. walk the rpn stack as a sequence and copy it into a result
    // stack, collapsing the stack when you find nonterminals (ops, func
    // calls, array index, ...)

    // I guess if you want to parse something like if x == 3 x = 4 -- NO WAY
    // NEVER then you have to fold the rpn as soon as you have two consecutive
    // non-ops on the stack and are pushing a third. (x 3) opstack (==) pushing
    // x -> fold. but dont allow this monstrosity! this is why `if x == 3 then x
    // = 4` is needed

    static PtrArray rpn, ops, result;
    int prec_top = 0;
    Expr* p = NULL;
    TokenKind revBrkt = tkUnknown;

    // ******* STEP 1 CONVERT TOKENS INTO RPN

    while (parser->token.kind != tkNullChar //
        && parser->token.kind != tkNewline
        && parser->token.kind != tkLineComment) { // build RPN

        // you have to ensure that ops have a space around them, etc.
        // so don't just skip the one spaces like you do now.
        if (parser->token.kind == tkOneSpace) Token_advance(&parser->token);
        if (parser->token.kind == tkIdentifier
            && memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser); // but continue parsing

        Expr* expr;
        if (Parser_matches(parser, tkParenOpen))
            expr = lparen;
        else if (Parser_matches(parser, tkParenClose))
            expr = rparen;
        else
            expr = Expr_fromToken(&parser->token); // dont advance yet

        int prec = expr->prec;
        bool rassoc = prec ? expr->rassoc : false;
        char lookAheadChar = Token_peekCharAfter(&parser->token);

        switch (expr->kind) {
        case tkIdentifier:
            if (memchr(parser->token.pos, '_', parser->token.matchlen))
                Parser_errorInvalidIdent(parser); // but continue parsing
            expr->slen = parser->token.matchlen;
            switch (lookAheadChar) {
            // TODO: need a general lookahead that skips whitespace.
            // case '!':
            //     if (parser->token.pos[2] != '(') goto defaultCase;
            case '(':
                expr->kind = tkFunctionCall;
                expr->prec = 60;
                PtrArray_push(&ops, expr);
                break;
            case '[':
                expr->kind = tkSubscript;
                expr->prec = 60;
                PtrArray_push(&ops, expr);
                break;
            case ' ':
                if (parser->token.pos[2] != '{') goto defaultCase;
                // otherwise fall through
            case '{':
                expr->kind = tkObjectInit;
                expr->prec = 60;
                PtrArray_push(&ops, expr);
                break;

            default:
            defaultCase:
                PtrArray_push(&rpn, expr);
                break;
            }
            break;

        case tkParenOpen:
            PtrArray_push(&ops, expr);
            if (!PtrArray_empty(&ops)
                && PtrArray_topAs(Expr*, &ops)->kind == tkFunctionCall)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == ')') PtrArray_push(&rpn, NULL);
            // for empty func() push null for no args
            if (lookAheadChar == '&') Token_advance(&parser->token);
            // ^ for mutating funcs, & is applied to the first arg on a call
            break;

        case tkArrayOpen:
            PtrArray_push(&ops, expr);
            if (!PtrArray_empty(&ops)
                && PtrArray_topAs(Expr*, &ops)->kind == tkSubscript)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == ']') PtrArray_push(&rpn, NULL);
            // for empty arr[] push null for no args
            break;

        case tkBraceOpen:
            PtrArray_push(&ops, expr);
            if (!PtrArray_empty(&ops)
                && PtrArray_topAs(Expr*, &ops)->kind == tkObjectInit)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == '}') PtrArray_push(&rpn, NULL);
            // for empty Obj {} push null for no args
            break;

        case tkParenClose:
        case tkArrayClose:
        case tkBraceClose:

            revBrkt = TokenKind_reverseBracket(expr->kind);
            if (PtrArray_empty(&ops)) {
                // need atleast the opening bracket of the current kind
                Parser_errorParsingExpr(parser, expr, "mismatched bracket");
                goto error;
            }

            else
                while (!PtrArray_empty(&ops)) {
                    p = PtrArray_pop(&ops);
                    if (p->kind == revBrkt) break;
                    PtrArray_push(&rpn, p);
                }

            // tkArrayOpen is a unary op.
            if ((p && p->kind == tkArrayOpen))
                if ((PtrArray_empty(&ops)
                        || (PtrArray_top(&rpn)
                            && PtrArray_topAs(Expr*, &ops)->kind
                                != tkSubscript))
                    // don't do this if its part of a subscript
                    || (PtrArray_empty(&rpn)
                        || (PtrArray_top(&rpn)
                            && PtrArray_topAs(Expr*, &rpn)->kind != tkOpColon)))
                    // or aa range. range exprs are handled separately. by
                    // themselves they don't need a surrounding [], but for
                    // grouping like 2+[8:66] they do.
                    PtrArray_push(&rpn, p);

            // a dict literal (another unary op).
            if ((p && p->kind == tkBraceOpen)
                && (PtrArray_empty(&ops)
                    || (PtrArray_top(&rpn)
                        && PtrArray_topAs(Expr*, &ops)->kind != tkObjectInit)))
                // again, not if it is an object init
                PtrArray_push(&rpn, p);
            // Object { member1 = 3, member3 = "train" }
            // ^^ this is an object init, not a dict
            // { "bing" = 34, "whang" = 33 }
            // ^^ this is a dict

            break;

        case tkKeyword_check: PtrArray_push(&ops, expr); break;
        case tkExclamation:
            if (PtrArray_empty(&rpn)
                || PtrArray_topAs(Expr*, &rpn)->kind != tkIdentifier) {
                Parser_errorParsingExpr(parser, expr, "invalid use of '!'");
                // TODO: change error to "invalid use of ! operator or
                // something"
            }
            break;

        case tkKeyword_return:
            // for empty return, push a NULL if there is no expr coming.
            PtrArray_push(&ops, expr);
            if (lookAheadChar == '!' || lookAheadChar == '\n')
                PtrArray_push(&rpn, NULL);
            break;

        case tkUnaryDot:
            expr->kind = tkPeriod;
            expr->unary = false;
            // if (PtrArray_empty(&rpn))
            PtrArray_push(&rpn, NULL);
            // push a NULL which will later be substituted with a dummy var of
            // the inferred enum type.

            // if (!PtrArray_empty(&ops)) {
            //     TokenKind kt = PtrArray_topAs(Expr*, &ops)->kind;
            //     if (!ISIN(2, kt, tkPeriod, tkFunctionCall))
            //         PtrArray_push(&rpn, NULL);
            //     // && rpn.used > 1
            //     //                    && PtrArray_topAs(Expr*,
            //     //                    &rpn)->kind != tkIdentifier))
            //     // fallthru
            // }
            fallthrough;
        default:
            if (prec) {
                if (expr->kind == tkOpColon) {
                    if (PtrArray_empty(&rpn)
                        || (!PtrArray_top(&rpn) && !PtrArray_empty(&ops)
                            && PtrArray_topAs(Expr*, &ops)->kind != tkOpColon)
                        || (PtrArray_topAs(Expr*, &rpn)->kind == tkOpColon
                            && !PtrArray_empty(&ops)
                            && (PtrArray_topAs(Expr*, &ops)->kind == tkOpComma
                                || PtrArray_topAs(Expr*, &ops)->kind
                                    == tkArrayOpen)))
                        // TODO: better way to parse :, 1:, :-1, etc.
                        // while passing tokens to RPN, if you see a :
                        // with nothing on the RPN or comma or [, push a
                        // NULL. while unwinding the op stack, if you
                        // pop a : and see a NULL or comma on the rpn,
                        // push another NULL.
                        PtrArray_push(&rpn, &expr_const_0);
                    // indicates empty operand
                }
                while (!PtrArray_empty(&ops)) {
                    prec_top = PtrArray_topAs(Expr*, &ops)->prec;
                    if (!prec_top) break; // left parenthesis
                    if (prec > prec_top) break;
                    if (prec == prec_top && rassoc) break;
                    p = PtrArray_pop(&ops);

                    if (p->kind != tkOpComma && p->kind != tkOpSemiColon
                        && p->kind != tkFunctionCall && p->kind != tkSubscript
                        && PtrArray_topAs(Expr*, &rpn)
                        && PtrArray_topAs(Expr*, &rpn)->kind == tkOpComma) {
                        Parser_errorUnexpectedToken(
                            parser, "unsupported use of comma");
                        // TODO: make this an error of unexpected expr instead
                        goto error;
                    }

                    if (!(p->prec || p->unary) && p->kind != tkFunctionCall
                        && p->kind != tkOpColon && p->kind != tkSubscript
                        && rpn.used < 2) {
                        Parser_errorUnexpectedToken(
                            parser, "need 2 operands to binary op");
                        // TODO: make this errorUnexpectedExpr
                        goto error;
                    }

                    PtrArray_push(&rpn, p);
                }

                if (PtrArray_empty(&rpn) && !expr->unary) {
                    Parser_errorUnexpectedToken(
                        parser, "binary op with no left operand");
                    // TODO: again unexpected Expr
                    goto error;
                }
                if (expr->kind == tkOpColon
                    && (lookAheadChar == ',' || lookAheadChar == ':'
                        || lookAheadChar == ']' || lookAheadChar == ')'))
                    PtrArray_push(&rpn, &expr_const_0);

                PtrArray_push(&ops, expr);
            } else {
                PtrArray_push(&rpn, expr);
            }
        }
        Token_advance(&parser->token);
        if (parser->token.kind == tkOneSpace) Token_advance(&parser->token);
    }
exitloop:

    while (!PtrArray_empty(&ops)) {
        p = PtrArray_pop(&ops);

        if (p->kind != tkOpComma && p->kind != tkFunctionCall
            && p->kind != tkSubscript && p->kind != tkArrayOpen
            && PtrArray_topAs(Expr*, &rpn)
            && PtrArray_topAs(Expr*, &rpn)->kind == tkOpComma) {
            Parser_errorUnexpectedExpr(parser, PtrArray_topAs(Expr*, &rpn));
            goto error;
        }

        if (!(p->prec || p->unary)
            && (p->kind != tkFunctionCall && p->kind != tkSubscript)
            && rpn.used < 2) {
            Parser_errorParsingExpr(parser, p, "invalid use of comma");
            goto error;
            // TODO: even if you have more than two, neither of the top
            // two should be a comma
        }

        PtrArray_push(&rpn, p);
    }

    // *** STEP 2 CONVERT RPN INTO EXPR TREE

    Expr* arg;
    for (int i = 0; i < rpn.used; i++) {
        if (!(p = rpn.ref[i])) goto justpush;
        switch (p->kind) {
        case tkFunctionCall:
        case tkSubscript:
            if (result.used > 0) {
                arg = PtrArray_pop(&result);
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
        case tkMultiDotNumber:
        case tkIdentifier:
        case tkKeyword_no:
        case tkKeyword_nil:
        case tkKeyword_yes:
        case tkParenOpen:
        case tkLineComment: break;

        default:
            // everything else is a nonterminal, needs left/right
            if (!p->prec) {
                Parser_errorParsingExpr(
                    parser, p, "unexpected token type with no precedence");
                goto error;
            }

            if (PtrArray_empty(&result)) {
                Parser_errorParsingExpr(parser, p, "no operands");
                goto error;
            }

            p->right = PtrArray_pop(&result);

            if (!p->unary) {
                if (PtrArray_empty(&result)) {
                    Parser_errorParsingExpr(
                        parser, p, "need 2 operands to binary op");
                    goto error;
                }
                p->left = PtrArray_pop(&result);
            }
        }
    justpush:
        PtrArray_push(&result, p);
    }
    if (!result.used) {
        Parser_errorUnexpectedToken(
            parser, "nothing parsed"); //    (parser, p);
        goto error;
    } else if (result.used != 1) {
        if (PtrArray_topAs(Expr*, &result)->kind != tkLineComment) {
            Parser_errorParsingExpr(parser, p, "more than 1 result");
            goto error;
        }
    }

    ops.used = 0;
    rpn.used = 0;
    result.used = 0;
    return result.ref[0];

error:

    while (parser->token.pos < parser->end
        && (parser->token.kind != tkNewline
            && parser->token.kind != tkLineComment
            && parser->token.kind != tkNullChar))
        Token_advance(&parser->token);

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
                eprintf(
                    "%.*s ", 32, e->prec ? TokenKind_repr[e->kind] : e->string);
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
                eprintf(
                    "%.*s ", 32, e->prec ? TokenKind_repr[e->kind] : e->string);
            }
        eputs("];;");
    }

    if (p) {
        eprintf("p: %.*s ", 32, p->prec ? TokenKind_repr[p->kind] : p->string);
        // eprintf("");
    }
    eputs("\n");

    ops.used = 0; // "reset" stacks
    rpn.used = 0;
    result.used = 0;
    return NULL;
}

#pragma mark - PARSE TYPESPEC
static TypeSpec* parseTypeSpec(Parser* parser) {
    parser->token.mergeArrayDims = true;

    TypeSpec* spec = NEW(TypeSpec);
    spec->line = parser->token.line;
    spec->col = parser->token.col;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    if (!isalpha(*parser->token.pos)) Parser_errorInvalidIdent(parser);

    spec->name = parseIdent(parser);

    if (Parser_matches(parser, tkArrayDims)) {
        if (isalpha(*parser->token.pos)) {
            // Dict
        } else {
            for (int i = 0; i < parser->token.matchlen; i++)
                if (parser->token.pos[i] == ':') spec->dims++;
            if (!spec->dims) spec->dims = 1;
            spec->collectionType = spec->dims == 1 ? CTYArray : CTYTensor;
        }
        Token_advance(&parser->token);
    }

    Parser_ignore(parser, tkUnits);

    assert(parser->token.kind != tkUnits);
    assert(parser->token.kind != tkArrayDims);

    parser->token.mergeArrayDims = false;
    return spec;
}

#pragma mark - PARSE VAR
static Var* parseVar(Parser* parser) {
    Var* var = NEW(Var);
    var->isVar = (parser->token.kind == tkKeyword_var);
    var->isLet = (parser->token.kind == tkKeyword_let);

    if (var->isVar) Parser_consume(parser, tkKeyword_var);
    if (var->isLet) Parser_consume(parser, tkKeyword_let);
    if (var->isVar || var->isLet) Parser_consume(parser, tkOneSpace);

    var->line = parser->token.line;
    var->col = parser->token.col;

    parser->token.mergeArrayDims = true;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'a' || *parser->token.pos > 'z')
        Parser_errorInvalidIdent(parser);
    var->name = parseIdent(parser);

    // if (matches(parser, tkExclamation)) {
    //     // ! is only used in function arguments. We set isVar here, but the
    //     // caller parseFunc should set isArg on each of its parsed arguments.
    //     // Then the linter/emitter know how to generate the var.
    //     var->isVar = true;
    //     Token_advance(&parser->token);
    // }

    int dims = 0;
    // if (matches(parser, tkArrayDims)) {
    //     for (int i = 0; i < parser->token.matchlen; i++)
    //         if (parser->token.pos[i] == ':') dims++;
    //     if (! dims) dims = 1;
    //     Token_advance(&parser->token);
    // }
    parser->token.mergeArrayDims = false;

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_matches(parser, tkIdentifier))
    //        && Parser_ignore(parser, tkKeyword_as)) {
    //      Parser_consume(parser, tkOneSpace);
    {
        var->spec = parseTypeSpec(parser);
    } else {
        var->spec = NEW(TypeSpec);
        var->spec->line = parser->token.line;
        var->spec->col = parser->token.col;
        var->spec->name = "";
    }

    Parser_ignore(parser, tkOneSpace);
    if (Parser_ignore(parser, tkOpAssign)) var->init = parseExpr(parser);

    return var;
}

static List(Var) * parseArgs(Parser* parser) {
    List(Var)* args = NULL;
    Parser_consume(parser, tkParenOpen);
    if (Parser_ignore(parser, tkParenClose)) return args;

    do {
        Var* arg = parseVar(parser);
        arg->isArg = true;
        PtrList_append(&args, arg);
    } while (Parser_ignore(parser, tkOpComma));

    Parser_consume(parser, tkParenClose);
    return args;
}
static Scope* parseScope(Parser* parser, Scope* parent, bool isTypeBody);

static Scope* parseScopeCases(Parser* parser, Scope* parent) {

    Scope* scope = NEW(Scope);
    scope->parent = parent;
    List(Var)** stmts = &scope->stmts;
    Expr* expr;

    while (parser->token.pos < parser->end) {
        switch (parser->token.kind) {

        case tkKeyword_case:
            expr = Parser_match(parser, tkKeyword_case);
            expr->left = parseExpr(parser);
            Token_advance(&parser->token); // trample null
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScope(parser, scope, false);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it for
            // 'match' or 'if' resp. see later for 'else' as well

            stmts = PtrList_append(stmts, expr);

            break;

        case tkNewline:
        case tkOneSpace:
        case tkLineComment: Token_advance(&parser->token); break;

        case tkKeyword_end: goto exitloop;

        default:
            Parser_errorUnexpectedToken(parser, "expected 'case' or 'end'");
            Token_advance(&parser->token);
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE SCOPE
static Scope* parseScope(Parser* parser, Scope* parent, bool isTypeBody) {
    Scope* scope = NEW(Scope);

    Var *var = NULL, *orig = NULL;
    Expr* expr = NULL;
    TokenKind tt = tkUnknown;
    Scope* forScope = NULL;

    scope->parent = parent;
    bool startedElse = false;
    bool startedCase = false;

    List(Var)** locals = &scope->locals;
    List(Var)** stmts = &scope->stmts;

    while (parser->token.kind != tkKeyword_end) {

        switch (parser->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkKeyword_var:
        case tkKeyword_let: {
            int col = parser->token.col + parser->token.matchlen;
            var = parseVar(parser);
            if (!var)
                continue;
            else
                Token_advance(&parser->token);
            if ((orig = Scope_getVar(scope, var->name)))
                Parser_errorDuplicateVar(parser, var, orig->line, orig->col);

            // resolveType(var->spec, scope);
            // resolve BEFORE it is added to the list! in
            // `var x = x + 1` x should not resolve
            // if var->spec is NULL then set the type
            // if it isn't NULL then check the types match
            locals = PtrList_append(locals, var);
            expr = NEW(Expr);
            expr->kind = tkVarAssign;
            expr->line = var->init ? var->init->line : var->line;
            expr->col = col;
            expr->prec = TokenKind_getPrecedence(tkOpAssign);
            expr->var = var;

            // and (var->init->prec or var->init->kind == tkIdentifier))
            // TODO: you actually need to send the PtrList item which is
            // generated in the next line as the topExpr, not the expr
            // itself
            if (var->init) resolveVars(parser, var->init, scope, false);

            stmts = PtrList_append(stmts, expr);
        } break;

        case tkKeyword_case: goto exitloop;

        case tkKeyword_match:
            if (isTypeBody) Parser_errorInvalidTypeMember(parser);
            expr = Parser_match(parser, tkKeyword_match);
            expr->left = parseExpr(parser);
            Token_advance(&parser->token);
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScopeCases(parser, scope);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it
            // for 'match' or 'if' resp. see later for 'else' as well
            // if (tt == tkKeyword_match) {
            Parser_consume(parser, tkKeyword_end);
            Parser_ignore(parser, tkOneSpace);
            Parser_ignore(parser, tkKeyword_match);
            // }
            stmts = PtrList_append(stmts, expr);

            break;

        case tkKeyword_else:
        case tkKeyword_elif:
            if (!startedElse) goto exitloop;
        case tkKeyword_if:
        case tkKeyword_for:
        case tkKeyword_while:
            if (isTypeBody) Parser_errorInvalidTypeMember(parser);
            tt = parser->token.kind;
            expr = Parser_match(parser, tt);
            expr->left = tt != tkKeyword_else ? parseExpr(parser) : NULL;

            // because we are going to be calling resolveVars right now, we
            // need to trample the newline
            Token_advance(&parser->token);

            if (tt == tkKeyword_for) {
                // TODO: new Parser_error
                Var* fvar = NULL;
                if (!expr->left)
                    unreachable("Missing for-loop condition at %d:%d\n",
                        expr->line, expr->col);
                else {
                    if (expr->left->kind != tkOpAssign)
                        unreachable("Invalid for-loop condition: %s\n",
                            TokenKind_repr[expr->left->kind]);

                    resolveVars(parser, expr->left->right, scope, false);

                    fvar = NEW(Var);
                    fvar->name = expr->left->left->string;
                    fvar->line = expr->left->line;
                    fvar->col = expr->left->left->col;
                    fvar->isVar = true;
                    fvar->init = expr->left->right;
                    fvar->spec = NEW(TypeSpec);
                    fvar->spec->typeType = TYReal64;

                    if ((orig = Scope_getVar(scope, fvar->name)))
                        Parser_errorDuplicateVar(
                            parser, fvar, orig->line, orig->col);
                }
                forScope = NEW(Scope);
                if (fvar) PtrList_shift(&forScope->locals, fvar);
                forScope->parent = scope;

            } else if (expr->left) {
                resolveVars(parser, expr->left, scope, false);
            }
            if (tt == tkKeyword_for) {
                expr->body = parseScope(parser, forScope, isTypeBody);
            } else {
                expr->body = parseScope(parser, scope, isTypeBody);
            }
            // Mark the scope as a loop scope if it is a 'for' or 'while'.
            expr->body->isLoop = tt == tkKeyword_for || tkKeyword_while;

            if (Parser_matches(parser, tkKeyword_else) || //
                Parser_matches(parser, tkKeyword_elif)) {
                startedElse = true;
            } else {
                Parser_consume(parser, tkKeyword_end);
                Parser_ignore(parser, tkOneSpace);
                Parser_ignore(parser,
                    tt == tkKeyword_else || tt == tkKeyword_elif ? tkKeyword_if
                                                                 : tt);
            }
            stmts = PtrList_append(stmts, expr);
            break;

        case tkNewline:
        case tkOneSpace: Token_advance(&parser->token); break;

        case tkKeyword_function:
        case tkKeyword_type:
        case tkKeyword_enum:
        case tkKeyword_test:
            // in this case, it is probably an error propagating all through the
            // file because an early func or type missed an end. How about we
            // stop parsing the scope here.
            goto exitloop;

        case tkLineComment:
            if (parser->generateCommentExprs) {
                expr = Expr_fromToken(&parser->token);
                stmts = PtrList_append(stmts, expr);
            }
            Token_advance(&parser->token);
            break;

        default:
            expr = parseExpr(parser);
            if (expr && isTypeBody) {
                Parser_errorInvalidTypeMember(parser);
                expr = NULL;
            }
            if (!expr) break;
            stmts = PtrList_append(stmts, expr);
            Token_advance(&parser->token); // eat the newline
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

    while (parser->token.kind != tkKeyword_end) {
        switch (parser->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkNewline:
        case tkOneSpace: Token_advance(&parser->token); break;

        case tkLineComment:
            if (parser->generateCommentExprs) {
                expr = Expr_fromToken(&parser->token);
                PtrList_append(&scope->stmts, expr);
            }
            Token_advance(&parser->token);
            break;

        case tkIdentifier:

            expr = parseExpr(parser);

            if (!expr) break;
            if (expr->kind != tkIdentifier && expr->kind != tkOpAssign) {
                Parser_errorInvalidTypeMember(parser);
                unreachable("%s\n", TokenKind_names[expr->kind]);
                expr = NULL;
            }
            stmts = PtrList_append(stmts, expr);
            Token_advance(&parser->token); // eat the newline

            var = NEW(Var);
            var->spec = NEW(TypeSpec);
            var->line = expr->line;
            var->col = (expr->kind == tkOpAssign) ? expr->left->col : expr->col;
            var->name = (expr->kind == tkOpAssign) ? expr->left->string
                                                   : expr->string;
            var->init = expr->right;
            vars = PtrList_append(vars, var);

            if (expr->kind == tkOpAssign)
                resolveVars(parser, expr->right, scope, false);

            break;

        default:
            Parser_errorExpectedToken(parser, parser->token.kind);
            goto exitloop;
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE PARAM
static List(Var) * parseParams(Parser* parser) {
    Parser_consume(parser, tkOpLT);
    List(Var) * params;
    Var* param;
    do {
        param = NEW(Var);
        param->name = parseIdent(parser);
        if (Parser_ignore(parser, tkKeyword_as))
            param->spec = parseTypeSpec(parser);
        if (Parser_ignore(parser, tkOpAssign)) param->init = parseExpr(parser);
        PtrList_append(&params, param);
    } while (Parser_ignore(parser, tkOpComma));
    Parser_consume(parser, tkOpGT);
    return params;
}

#pragma mark - PARSE FUNC / STMT-FUNC
static Func* parseFunc(Parser* parser, Scope* globScope, bool shouldParseBody) {
    Parser_consume(parser, tkKeyword_function);
    Parser_consume(parser, tkOneSpace);
    Func* func = NEW(Func);

    func->line = parser->token.line;
    func->col = parser->token.col;

    func->nameLen = parser->token.matchlen;
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parseIdent(parser);
    func->isDeclare = !shouldParseBody;

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // Parser_ignore(parser, tkExclamation);

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);

    if (mutator && func->argCount) {
        Var* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_ignore(parser, tkKeyword_as)) {
        Parser_consume(parser, tkOneSpace);
        func->spec = parseTypeSpec(parser);
    }

    if (shouldParseBody) {
        Parser_ignore(parser, tkLineComment);
        Parser_consume(parser, tkNewline);

        Scope* funcScope = NEW(Scope);
        funcScope->parent = globScope;
        funcScope->locals = func->args;
        func->body = parseScope(parser, funcScope, false);

        Parser_consume(parser, tkKeyword_end);
        Parser_ignore(parser, tkOneSpace);
        Parser_ignore(parser, tkKeyword_function);
    }

    return func;
}

static Func* parseStmtFunc(Parser* parser, Scope* globScope) {
    Func* func = NEW(Func);

    func->line = parser->token.line;
    func->isStmt = true;

    func->nameLen = parser->token.matchlen;
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parseIdent(parser);

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // Parser_ignore(parser, tkExclamation);

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);

    if (mutator && func->argCount) {
        Var* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    Parser_ignore(parser, tkOneSpace);

    func->spec = NEW(TypeSpec);
    func->spec->line = parser->token.line;
    func->spec->col = parser->token.col;
    func->spec->name = "";

    Expr* ret = exprFromCurrentToken(parser);

    // if you have toplevel code (eg func call) it tends to reach here
    if (ret->kind != tkOpColEq) return NULL;

    ret->kind = tkKeyword_return;
    ret->unary = true;

    ret->right = parseExpr(parser);
    Scope* scope = NEW(Scope);
    PtrList_append(&scope->stmts, ret);

    Scope* funcScope = NEW(Scope);
    funcScope->locals = func->args;
    scope->parent = funcScope;
    func->body = scope;

    Parser_consume(parser, tkNewline);
    resolveVars(parser, ret->right, funcScope, false);

    return func;
}

#pragma mark - PARSE TEST
static JetTest* parseTest(Parser* parser, Scope* globScope) {
    Parser_consume(parser, tkKeyword_test);
    Parser_consume(parser, tkOneSpace);
    JetTest* test = NEW(JetTest);

    test->line = parser->token.line;

    if (parser->token.kind != tkString && parser->token.kind != tkRawString)
        Parser_errorInvalidTestName(parser);
    test->name = parser->token.pos + 1;
    Token_advance(&parser->token);

    Parser_consume(parser, tkNewline);

    test->body = parseScope(parser, NULL, false);

    Parser_consume(parser, tkKeyword_end);
    Parser_ignore(parser, tkOneSpace);
    Parser_ignore(parser, tkKeyword_test);

    return test;
}

#pragma mark - PARSE UNITS
static JetUnits* parseUnits(Parser* parser) { return NULL; }

#pragma mark - PARSE TYPE
static Type* parseType(Parser* parser, Scope* globScope, bool shouldParseBody) {
    Type* type = NEW(Type);

    Parser_consume(parser, tkKeyword_type);
    Parser_consume(parser, tkOneSpace);

    type->line = parser->token.line;
    type->col = parser->token.col;

    // if (memchr(parser->token.pos, '_', parser->token.matchlen))
    //     Parser_errorInvalidIdent(parser);
    // if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
    //     Parser_errorInvalidIdent(parser);
    type->name = parseIdent(parser);

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_ignore(parser, tkKeyword_extends)) {
        Parser_consume(parser, tkOneSpace);
        type->super = parseTypeSpec(parser);
    }
    Parser_ignore(parser, tkNewline);

    type->body = NULL;
    // if (TypeType_byName(type->name) != TYUnresolved) {
    //     Parser_errorDuplicateType(parser, type, NULL);
    //     return type;
    // }

    type->body = parseScope(parser, globScope, true);

    Parser_consume(parser, tkKeyword_end);
    Parser_ignore(parser, tkOneSpace);
    Parser_ignore(parser, tkKeyword_type);

    return type;
}

static Type* parseEnum(Parser* parser, Scope* globScope) {
    Type* en = NEW(Type);

    Parser_consume(parser, tkKeyword_enum);
    Parser_consume(parser, tkOneSpace);

    en->line = parser->token.line;
    en->col = parser->token.col;
    en->isEnum = true;

    // if (memchr(parser->token.pos, '_', parser->token.matchlen))
    //     Parser_errorInvalidIdent(parser);
    // if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
    //     Parser_errorInvalidIdent(parser);
    en->name = parseIdent(parser);

    Parser_consume(parser, tkNewline);

    // if (TypeType_byName(en->name) != TYUnresolved) {
    //     // conflicts with a primitive type name
    //     Parser_errorDuplicateEnum(parser, en, NULL);
    //     return en;
    // }

    en->body = parseEnumBody(parser, globScope);

    Parser_consume(parser, tkKeyword_end);
    Parser_ignore(parser, tkOneSpace);
    Parser_ignore(parser, tkKeyword_enum);

    return en;
}

static Import* parseImport(Parser* parser, Module* ownerMod) {
    Import* import = NEW(Import);
    char* tmp;
    Parser_consume(parser, tkKeyword_import);
    Parser_consume(parser, tkOneSpace);

    // import->isPackage = Parser_ignore(parser, tkAt);

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    if (!Parser_matches(parser, tkIdentifier)) {
        Parser_errorExpectedToken(parser, tkIdentifier);
        return NULL;
    }
    import->name = parser->token.pos; // parseIdent(parser);

    char* cend = import->name;
    while (*cend && (isalnum(*cend) || *cend == '.')) cend++;

    parser->token.pos = cend;
    Token_detect(&parser->token);

    //    size_t len = parser->token.pos - import->name;
    Parser_ignore(parser, tkOneSpace);
    if (Parser_ignore(parser, tkKeyword_as)) {

        Parser_ignore(parser, tkOneSpace);
        //      import->hasAlias = true;

        assert(Parser_matches(parser, tkIdentifier));
        if (memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser);

        import->aliasOffset = parser->token.pos - import->name;
        parseIdent(parser);

    } else {
        import->aliasOffset
            = CString_base(import->name, '.', parser->token.pos - import->name)
            - import->name;
    }

    char endchar = *parser->token.pos;
    *parser->token.pos = 0;
    if (Module_getImportByAlias(ownerMod, import->name + import->aliasOffset)
        || Module_getFuncByName(ownerMod, import->name + import->aliasOffset)
        || Module_getVar(ownerMod, import->name + import->aliasOffset)) {
        unreachable(
            "import name already used: %s", import->name + import->aliasOffset);
        import = NULL;
    }
    *parser->token.pos = endchar;
    // ^ need to restore the nl since it is used for line counting

    // Parser_ignore(parser, tkOneSpace);

    // if (parser->token.kind != tkLineComment && parser->token.kind !=
    // tkNewline)
    //     Parser_errorUnexpectedToken(parser);
    // while (
    //     parser->token.kind != tkLineComment //
    //     && parser->token.kind != tkNewline//
    //     && parser->token.kind!=tkNullChar
    //     )
    //     Token_advance(&parser->token);
    return import;
}

void analyseModule(Parser* parser, Module* mod);

static Func* Type_makeDefaultCtor(Type* type) {
    Func* ctor = NEW(Func);
    ctor->line = type->line;
    ctor->isDefCtor = true;
    // Ctors must AlWAYS return a new object.
    // even Ctors with args.
    ctor->returnsNewObjectAlways = true;
    ctor->name = type->name;
    char buf[128];
    int l = snprintf(buf, 128, "%s_new_", type->name);
    ctor->selector = CString_pndup(buf, l);
    TypeSpec* tspec = TypeSpec_new(TYObject, CTYNone);
    tspec->type = type;
    ctor->spec = tspec;
    return ctor;
}

// this func should just be replaced by parseScope handling function type etc

//,
//        .init = (Expr[]) { { .kind = tkNumber, .string = "1" } } }
//}
//;

// vno->vno->;
// Var* vyes = NEW(Var);
// vyes->name = "yes";
// vyes->typeType = TYBool;

// static Module* Parser_lookupModuleByAlias(PtrList* existingModules, char*
// name) {
//     foreach (Module*, mod, existingModules) {
//         // eprintf("lookup %s %s\n", name, mod->name);
//         if (!strcasecmp(name, mod->name)) return mod;
//     }
//     return NULL;
// }
static Module* Parser_lookupModule(PtrList* existingModules, char* name) {
    foreach (Module*, mod, existingModules) {
        // eprintf("lookup %s %s\n", name, mod->name);
        if (!strcasecmp(name, mod->name)) return mod;
    }
    return NULL;
}

static Module* parseModule(
    Parser* parser, PtrList** existingModulesPtr, Module* importer) {
    Module* root = NEW(Module);

    // ret->noext = CString_noext(CString_clone(filename));
    root->name = CString_tr_ip(
        CString_noext_ip(CString_clone(parser->filename)), '/', '.');
    // root->name = parser->moduleName;
    // To break recursion, add the root to the existingModules list right away.
    // The name is all that is required for this module to be found later.
    PtrList_shift(existingModulesPtr, root);
    // eprintf("Parsing %s\n", root->name);
    // root->scope = NEW(Scope);

    // Token parser->token = { .pos = data.ref, .end = data.ref + data.len };
    Token_advance(&parser->token); // maybe put this in parser ctor

    Import* import = NULL;
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
    List(JetTest)** tests = &root->tests;
    List(JetEnum)** enums = &root->enums;
    Scope* gscope = root->scope;
    List(Var)** gvars = &gscope->locals; // globals
    List(Expr)** gexprs = &gscope->stmts; // globals

    // for_to(i, countof(vnoyes))
    // gvars = PtrList_append(gvars, expr_const_empty);
    // gvars = PtrList_append(gvars, expr_const_yes);
    // gvars = PtrList_append(gvars, expr_const_no);
    // gvars = PtrList_append(gvars, expr_const_nil);

    while (parser->token.kind != tkNullChar) {

        if (parser->mode == PMTokenize) {
            printf("%s %2d %3d %3d %-20s\t%.*s\n", parser->filename,
                parser->token.line, parser->token.col, parser->token.matchlen,
                TokenKind_names[parser->token.kind],
                parser->token.kind == tkNewline ? 0 : parser->token.matchlen,
                parser->token.pos);
            Token_advance(&parser->token);
            continue;
        }

        switch (parser->token.kind) {

        case tkKeyword_declare:
            Token_advance(&parser->token);
            Parser_consume(parser, tkOneSpace);
            if (parser->token.kind == tkKeyword_function) {
                Func* func = parseFunc(parser, gscope, false);
                func->isDeclare = true;
                funcs = PtrList_append(funcs, func);
            }
            if (parser->token.kind == tkKeyword_type) {
                Type* type = parseType(parser, gscope, false);
                type->isDeclare = true;
                types = PtrList_append(types, type);
            }
            break;

        case tkKeyword_function:
            funcs = PtrList_append(funcs, parseFunc(parser, gscope, true));
            break;

        case tkKeyword_enum: {
            Type* en = parseEnum(parser, gscope);
            enums = PtrList_append(enums, en);
            // add a global of the corresponding type so that it can be used
            // to access members.
            assert(en);
            Var* enumv = NEW(Var);
            enumv->name = en->name;
            enumv->line = en->line;
            enumv->col = en->col;
            enumv->spec = NEW(TypeSpec);
            enumv->spec->typeType = TYObject;
            enumv->spec->type = en;
            gvars = PtrList_append(gvars, enumv);
        }

        break;

        case tkKeyword_type: {
            Type* type = parseType(parser, gscope, true);
            types = PtrList_append(types, type);

            // create default constructor
            Func* ctor = Type_makeDefaultCtor(type);
            funcs = PtrList_append(funcs, ctor);

            // create some extra function declares
            char* defFuncs[] = { "json", "print", "describe" };

            for (int i = 0; i < countof(defFuncs); i++) {
                Func* func
                    = Func_createDeclWithArg(defFuncs[i], NULL, type->name);
                func->line = type->line;
                func->intrinsic = true;
                funcs = PtrList_append(funcs, func);
            }
        } break;

        case tkKeyword_import:
            import = parseImport(parser, root);
            if (import) {

                imports = PtrList_append(imports, import);

                import->module
                    = Parser_lookupModule(parser->modules, import->name);
                if (!import->module) {
                    eprintf("%s needs %s, parsing\n", root->name, import->name);
                    size_t len = strlen(import->name) + 5;
                    char* filename = malloc(len);
                    filename[len - 1] = 0;
                    strcpy(filename, import->name);
                    CString_tr_ip_len(filename, '.', '/', len - 5);
                    strcpy(filename + len - 5, ".jet");
                    // later you can have a fancier routine that figures out
                    // the file name from a module name
                    Parser* subParser
                        = Parser_fromFile(filename, true, parser->mode);
                    // parseModule returns the parsed module, and adds dependent
                    // modules to the second argument. dependencies are only
                    // parsed once on first use.
                    // FIXME: these parsers will LEAK!
                    if (subParser) {
                        if ((import->module = parseModule(
                                 parser, existingModulesPtr, NULL)))
                            PtrList_shift(&import->module->importedBy, root);
                    }
                }
            }
            break;

        case tkKeyword_test:
            tests = PtrList_append(tests, parseTest(parser, gscope));
            break;

        case tkKeyword_var:
        case tkKeyword_let:
            // TODO: add these to exprs
            {
                int col = parser->token.col + parser->token.matchlen;
                Var *var = parseVar(parser), *orig;
                if (!var) {
                    Token_advance(&parser->token);
                    continue;
                }
                if ((orig = Scope_getVar(gscope, var->name)))
                    Parser_errorDuplicateVar(
                        parser, var, orig->line, orig->col);
                Import* imp = Module_getImportByAlias(root, var->name);
                Func* fnc = Module_getFuncByName(root, var->name);
                if (imp)
                    Parser_errorDuplicateVar(parser, var, imp->line, imp->col);
                if (fnc) Parser_errorDuplicateVar(parser, var, fnc->line, 5);

                if (var->init) resolveVars(parser, var->init, gscope, false);
                var->isLet = true;
                var->isVar = false;
                gvars = PtrList_append(gvars, var);

                // TODO: validation should raise issue if var->init is
                // missing
                Expr* expr = NEW(Expr);
                expr->kind = tkVarAssign;
                expr->line = var->init ? var->init->line : parser->token.line;
                expr->col = col;
                // printf("%d\n", col);
                expr->prec = TokenKind_getPrecedence(tkOpAssign);
                expr->var = var;

                // and (var->init->prec or var->init->kind == tkIdentifier))
                // TODO: you actually need to send the PtrList item which is
                // generated in the next line as the topExpr, not the expr
                // itself
                if (var->init) resolveVars(parser, var->init, gscope, false);

                gexprs = PtrList_append(gexprs, expr);
            }

            break;
        case tkNewline: *(parser->token.pos) = 0;
        // fallthrough
        case tkLineComment:
        // TODO: add line comment to module exprs
        case tkOneSpace: Token_advance(&parser->token); break;
        case tkIdentifier: // stmt funcs: f(x) := f(y, w = 4) etc.
            if (Token_peekCharAfter(&parser->token) == '(') {
                funcs = PtrList_append(funcs, parseStmtFunc(parser, gscope));
                break;
            }
        default:
            Parser_errorUnexpectedToken(parser, "expected a keyword here");
            while (!ISIN(
                3, parser->token.kind, tkNewline, tkLineComment, tkNullChar))
                Token_advance(&parser->token);
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
                = Func_createDeclWithArg(defFuncs[i], retTypes[i], defTypes[j]);
            func->intrinsic = true;
            funcs = PtrList_append(funcs, func);
        }

    // do some analysis that happens after the entire module is loaded
    Module_analyse(parser, root);
    // if (importer) PtrList_shift(&importer->importedBy, root);
    return root;
}

// TODO: move this to separate file or to analysis.c [sempass.c]
