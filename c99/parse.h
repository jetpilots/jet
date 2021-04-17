
#pragma mark - PARSE EXPR
static ast_expr_t* parse_expr(parser_t* parser) {
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

    static array_t(ast_expr_t) rpn, ops, result;
    int prec_top = 0;
    ast_expr_t* p = NULL;
    tokenkind_e revBrkt = tk_unknown;

    // ******* STEP 1 CONVERT TOKENS INTO RPN

    while (parser->token.kind != tk_nullChar //
        && parser->token.kind != tk_newline
        && parser->token.kind != tk_lineComment) { // build RPN

        // you have to ensure that ops have a space around them, etc.
        // so don't just skip the one spaces like you do now.
        if (parser->token.kind == tk_oneSpace) Token_advance(&parser->token);
        if (parser->token.kind == tk_identifier
            && memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser); // but continue parsing

        ast_expr_t* expr;
        if (Parser_matches(parser, tk_parenOpen))
            expr = lparen;
        else if (Parser_matches(parser, tk_parenClose))
            expr = rparen;
        else
            expr = fromToken_expr(&parser->token); // dont advance yet

        int prec = expr->prec;
        bool rassoc = prec ? expr->rassoc : false;
        char lookAheadChar = Token_peekCharAfter(&parser->token);

        switch (expr->kind) {
        case tk_identifier:
            if (memchr(parser->token.pos, '_', parser->token.matchlen))
                Parser_errorInvalidIdent(parser); // but continue parsing
            expr->slen = parser->token.matchlen;
            switch (lookAheadChar) {
            // TODO: need a general lookahead that skips whitespace.
            // case '!':
            //     if (parser->token.pos[2] != '(') goto defaultCase;
            case '(':
                expr->kind = tk_functionCall;
                expr->prec = 60;
                array_push(&ops, expr);
                break;
            case '[':
                expr->kind = tk_subscript;
                expr->prec = 60;
                array_push(&ops, expr);
                break;
            case ' ':
                if (parser->token.pos[2] != '{') goto defaultCase;
                // otherwise fall through
            case '{':
                expr->kind = tk_objectInit;
                expr->prec = 60;
                array_push(&ops, expr);
                break;

            default:
            defaultCase:
                array_push(&rpn, expr);
                break;
            }
            break;

        case tk_parenOpen:
            array_push(&ops, expr);
            if (!array_empty(&ops)
                && array_topas(ast_expr_t*, &ops)->kind == tk_functionCall)
                array_push(&rpn, expr);
            if (lookAheadChar == ')') array_push(&rpn, NULL);
            // for empty func() push null for no args
            if (lookAheadChar == '&') Token_advance(&parser->token);
            // ^ for mutating funcs, & is applied to the first arg on a call
            break;

        case tk_arrayOpen:
            array_push(&ops, expr);
            if (!array_empty(&ops)
                && array_topAs(ast_expr_t*, &ops)->kind == tk_subscript)
                array_push(&rpn, expr);
            if (lookAheadChar == ']') array_push(&rpn, NULL);
            // for empty arr[] push null for no args
            break;

        case tk_braceOpen:
            array_push(&ops, expr);
            if (!array_empty(&ops)
                && array_topAs(ast_expr_t*, &ops)->kind == tk_objectInit)
                array_push(&rpn, expr);
            if (lookAheadChar == '}') array_push(&rpn, NULL);
            // for empty Obj {} push null for no args
            break;

        case tk_parenClose:
        case tk_arrayClose:
        case tk_braceClose:

            revBrkt = tokenkind_e_reverseBracket(expr->kind);
            if (array_empty(&ops)) {
                // need atleast the opening bracket of the current kind
                Parser_errorParsingExpr(parser, expr, "mismatched bracket");
                goto error;
            }

            else
                while (!array_empty(&ops)) {
                    p = array_pop(&ops);
                    if (p->kind == revBrkt) break;
                    array_push(&rpn, p);
                }

            // tk_arrayOpen is a unary op.
            if ((p && p->kind == tk_arrayOpen))
                if ((array_empty(&ops)
                        || (array_top(&rpn)
                            && array_topAs(ast_expr_t*, &ops)->kind
                                != tk_subscript))
                    // don't do this if its part of a subscript
                    || (array_empty(&rpn)
                        || (array_top(&rpn)
                            && array_topAs(ast_expr_t*, &rpn)->kind
                                != tk_opColon)))
                    // or aa range. range exprs are handled separately. by
                    // themselves they don't need a surrounding [], but for
                    // grouping like 2+[8:66] they do.
                    array_push(&rpn, p);

            // a dict literal (another unary op).
            if ((p && p->kind == tk_braceOpen)
                && (array_empty(&ops)
                    || (array_top(&rpn)
                        && array_topAs(ast_expr_t*, &ops)->kind
                            != tk_objectInit)))
                // again, not if it is an object init
                array_push(&rpn, p);
            // Object { member1 = 3, member3 = "train" }
            // ^^ this is an object init, not a dict
            // { "bing" = 34, "whang" = 33 }
            // ^^ this is a dict

            break;

        case tk_keyword_check: array_push(&ops, expr); break;
        case tk_exclamation:
            if (array_empty(&rpn)
                || array_topAs(ast_expr_t*, &rpn)->kind != tk_identifier) {
                Parser_errorParsingExpr(parser, expr, "invalid use of '!'");
                // TODO: change error to "invalid use of ! operator or
                // something"
            }
            break;

        case tk_keyword_return:
            // for empty return, push a NULL if there is no expr coming.
            array_push(&ops, expr);
            if (lookAheadChar == '!' || lookAheadChar == '\n')
                array_push(&rpn, NULL);
            break;

        case tk_unaryDot:
            expr->kind = tk_period;
            expr->unary = false;
            // if (array_empty(&rpn))
            array_push(&rpn, NULL);
            // push a NULL which will later be substituted with a dummy var of
            // the inferred enum type.

            // if (!array_empty(&ops)) {
            //     tokenkind_e kt = array_topAs(ast_expr_t*, &ops)->kind;
            //     if (!ISIN(2, kt, tk_period, tk_functionCall))
            //         array_push(&rpn, NULL);
            //     // && rpn.used > 1
            //     //                    && array_topAs(ast_expr_t*,
            //     //                    &rpn)->kind != tk_identifier))
            //     // fallthru
            // }
            fallthrough;
        default:
            if (prec) {
                if (expr->kind == tk_opColon) {
                    if (array_empty(&rpn)
                        || (!array_top(&rpn) && !array_empty(&ops)
                            && array_topAs(ast_expr_t*, &ops)->kind
                                != tk_opColon)
                        || (array_topAs(ast_expr_t*, &rpn)->kind == tk_opColon
                            && !array_empty(&ops)
                            && (array_topAs(ast_expr_t*, &ops)->kind
                                    == tk_opComma
                                || array_topAs(ast_expr_t*, &ops)->kind
                                    == tk_arrayOpen)))
                        // TODO: better way to parse :, 1:, :-1, etc.
                        // while passing tokens to RPN, if you see a :
                        // with nothing on the RPN or comma or [, push a
                        // NULL. while unwinding the op stack, if you
                        // pop a : and see a NULL or comma on the rpn,
                        // push another NULL.
                        array_push(&rpn, &expr_const_0);
                    // indicates empty operand
                }
                while (!array_empty(&ops)) {
                    prec_top = array_topAs(ast_expr_t*, &ops)->prec;
                    if (!prec_top) break; // left parenthesis
                    if (prec > prec_top) break;
                    if (prec == prec_top && rassoc) break;
                    p = array_pop(&ops);

                    if (p->kind != tk_opComma && p->kind != tk_opSemiColon
                        && p->kind != tk_functionCall && p->kind != tk_subscript
                        && array_topAs(ast_expr_t*, &rpn)
                        && array_topAs(ast_expr_t*, &rpn)->kind == tk_opComma) {
                        Parser_errorUnexpectedToken(
                            parser, "unsupported use of comma");
                        // TODO: make this an error of unexpected expr instead
                        goto error;
                    }

                    if (!(p->prec || p->unary) && p->kind != tk_functionCall
                        && p->kind != tk_opColon && p->kind != tk_subscript
                        && rpn.used < 2) {
                        Parser_errorUnexpectedToken(
                            parser, "need 2 operands to binary op");
                        // TODO: make this errorUnexpectedExpr
                        goto error;
                    }

                    array_push(&rpn, p);
                }

                if (array_empty(&rpn) && !expr->unary) {
                    Parser_errorUnexpectedToken(
                        parser, "binary op with no left operand");
                    // TODO: again unexpected Expr
                    goto error;
                }
                if (expr->kind == tk_opColon
                    && (lookAheadChar == ',' || lookAheadChar == ':'
                        || lookAheadChar == ']' || lookAheadChar == ')'))
                    array_push(&rpn, &expr_const_0);

                array_push(&ops, expr);
            } else {
                array_push(&rpn, expr);
            }
        }
        Token_advance(&parser->token);
        if (parser->token.kind == tk_oneSpace) Token_advance(&parser->token);
    }
exitloop:

    while (!array_empty(&ops)) {
        p = array_pop(&ops);

        if (p->kind != tk_opComma && p->kind != tk_functionCall
            && p->kind != tk_subscript && p->kind != tk_arrayOpen
            && array_topAs(ast_expr_t*, &rpn)
            && array_topAs(ast_expr_t*, &rpn)->kind == tk_opComma) {
            Parser_errorUnexpectedExpr(parser, array_topAs(ast_expr_t*, &rpn));
            goto error;
        }

        if (!(p->prec || p->unary)
            && (p->kind != tk_functionCall && p->kind != tk_subscript)
            && rpn.used < 2) {
            Parser_errorParsingExpr(parser, p, "invalid use of comma");
            goto error;
            // TODO: even if you have more than two, neither of the top
            // two should be a comma
        }

        array_push(&rpn, p);
    }

    // *** STEP 2 CONVERT RPN INTO EXPR TREE

    ast_expr_t* arg;
    for (int i = 0; i < rpn.used; i++) {
        if (!(p = rpn.ref[i])) goto justpush;
        switch (p->kind) {
        case tk_functionCall:
        case tk_subscript:
            if (result.used > 0) {
                arg = array_pop(&result);
                if (arg && p->kind == tk_subscript) {
                    // assert(arg->kind == tk_arrayOpen);
                    if (arg->kind == tk_arrayOpen) arg = arg->right;
                }
                p->left = arg;
            }
            break;

        case tk_number:
        case tk_string:
        case tk_rawString:
        case tk_regexp:
        case tk_units:
        case tk_multiDotNumber:
        case tk_identifier:
        case tk_keyword_no:
        case tk_keyword_nil:
        case tk_keyword_yes:
        case tk_parenOpen:
        case tk_lineComment: break;

        default:
            // everything else is a nonterminal, needs left/right
            if (!p->prec) {
                Parser_errorParsingExpr(
                    parser, p, "unexpected token type with no precedence");
                goto error;
            }

            if (array_empty(&result)) {
                Parser_errorParsingExpr(parser, p, "no operands");
                goto error;
            }

            p->right = array_pop(&result);

            if (!p->unary) {
                if (array_empty(&result)) {
                    Parser_errorParsingExpr(
                        parser, p, "need 2 operands to binary op");
                    goto error;
                }
                p->left = array_pop(&result);
            }
        }
    justpush:
        array_push(&result, p);
    }
    if (!result.used) {
        Parser_errorUnexpectedToken(
            parser, "nothing parsed"); //    (parser, p);
        goto error;
    } else if (result.used != 1) {
        if (array_topAs(ast_expr_t*, &result)->kind != tk_lineComment) {
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
        && (parser->token.kind != tk_newline
            && parser->token.kind != tk_lineComment
            && parser->token.kind != tk_nullChar))
        Token_advance(&parser->token);

    if (ops.used) {
        eputs("ops: [ ");
        for (int i = 0; i < ops.used; i++)
            eprintf("%s ", tokenkind_e_repr[((ast_expr_t*)ops.ref[i])->kind]);
        eputs("];;");
    }

    if (rpn.used) {
        eputs("rpn: [ ");
        for (int i = 0; i < rpn.used; i++)
            if (!rpn.ref[i])
                eputs("NUL ");
            else {
                ast_expr_t* e = rpn.ref[i];
                eprintf("%.*s ", 32,
                    e->prec ? tokenkind_e_repr[e->kind] : e->string);
            }
        eputs("];;");
    }

    if (result.used) {
        eputs("result: [ ");
        for (int i = 0; i < result.used; i++)
            if (!result.ref[i])
                eputs("NUL ");
            else {
                ast_expr_t* e = result.ref[i];
                eprintf("%.*s ", 32,
                    e->prec ? tokenkind_e_repr[e->kind] : e->string);
            }
        eputs("];;");
    }

    if (p) {
        eprintf(
            "p: %.*s ", 32, p->prec ? tokenkind_e_repr[p->kind] : p->string);
        // eprintf("");
    }
    eputs("\n");

    ops.used = 0; // "reset" stacks
    rpn.used = 0;
    result.used = 0;
    return NULL;
}

#pragma mark - PARSE ty_pESPEC
static ast_typespec_t* parse_typespec(parser_t* parser) {
    parser->token.mergedims = true;

    ast_typespec_t* typespec = NEW(ast_typespec_t);
    typespec->line = parser->token.line;
    typespec->col = parser->token.col;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    if (!isalpha(*parser->token.pos)) Parser_errorInvalidIdent(parser);

    typespec->name = parse_ident(parser);
    // parser->token.pos; //
    // Token_advance(&parser->token);
    // while (isalnum(parser->token.pos) || *parser->token.pos == '.')
    //     parser->token.pos++;
    // int len = parser->token.pos - typespec->name;
    // Token_detect(&parser->token);

    if (Parser_matches(parser, tk_arrayDims)) {
        if (isalpha(*parser->token.pos)) {
            // Dict
        } else {
            for (int i = 0; i < parser->token.matchlen; i++)
                if (parser->token.pos[i] == ':') typespec->dims++;
            if (!typespec->dims) typespec->dims = 1;
            typespec->collectionType
                = typespec->dims == 1 ? cty_array : cty_tensor;
        }
        Token_advance(&parser->token);
    }

    Parser_ignore(parser, tk_units);

    assert(parser->token.kind != tk_units);
    assert(parser->token.kind != tk_arrayDims);

    parser->token.mergedims = false;
    return typespec;
}

#pragma mark - PARSE VAR
static ast_var_t* parse_var(parser_t* parser) {
    ast_var_t* var = NEW(ast_var_t);
    var->isVar = (parser->token.kind == tk_keyword_var);
    var->isLet = (parser->token.kind == tk_keyword_let);

    if (var->isVar) Parser_consume(parser, tk_keyword_var);
    if (var->isLet) Parser_consume(parser, tk_keyword_let);
    if (var->isVar || var->isLet) Parser_consume(parser, tk_oneSpace);

    var->line = parser->token.line;
    var->col = parser->token.col;

    parser->token.mergedims = true;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'a' || *parser->token.pos > 'z')
        Parser_errorInvalidIdent(parser);
    var->name = parse_ident(parser);

    // if (matches(parser, tk_exclamation)) {
    //     // ! is only used in function arguments. We set isVar here, but the
    //     // caller parseFunc should set isArg on each of its parsed arguments.
    //     // Then the formater/emitter know how to generate the var.
    //     var->isVar = true;
    //     Token_advance(&parser->token);
    // }

    int dims = 0;
    // if (matches(parser, tk_arrayDims)) {
    //     for (int i = 0; i < parser->token.matchlen; i++)
    //         if (parser->token.pos[i] == ':') dims++;
    //     if (! dims) dims = 1;
    //     Token_advance(&parser->token);
    // }
    parser->token.mergedims = false;

    if (Parser_ignore(parser, tk_oneSpace)
        && Parser_matches(parser, tk_identifier))
    //        && Parser_ignore(parser, tk_keyword_as)) {
    //      Parser_consume(parser, tk_oneSpace);
    {
        var->typespec = parse_typespec(parser);
    } else {
        var->typespec = NEW(ast_typespec_t);
        var->typespec->line = parser->token.line;
        var->typespec->col = parser->token.col;
        var->typespec->name = "";
    }
    // var->typespec->dims = dims;

    Parser_ignore(parser, tk_oneSpace);
    if (Parser_ignore(parser, tk_opAssign)) //
        var->init = parse_expr(parser);

    return var;
}

static list_t(ast_var_t) * parse_args(parser_t* parser) {
    list_t(ast_var_t)* args = NULL;
    Parser_consume(parser, tk_parenOpen);
    if (Parser_ignore(parser, tk_parenClose)) return args;

    ast_var_t* arg;
    do {
        arg = parse_var(parser);
        arg->isArg = true;
        list_append(&args, arg);
    } while (Parser_ignore(parser, tk_opComma));

    Parser_consume(parser, tk_parenClose);
    return args;
}
static ast_scope_t* parse_scope(
    parser_t* parser, ast_scope_t* parent, bool isTypeBody);

static ast_scope_t* parse_scopeCases(parser_t* parser, ast_scope_t* parent) {

    ast_scope_t* scope = NEW(ast_scope_t);
    scope->parent = parent;
    list_t(ast_var_t)** stmts = &scope->stmts;
    ast_expr_t* expr;

    while (parser->token.pos < parser->end) {
        switch (parser->token.kind) {

        case tk_keyword_case:
            // if (startedCase) {
            //     startedCase = false;
            //     break;
            // } // goto exitloop;
            // startedCase = true;
            // startedCase = !startedCase;
            // if (!startedCase) {
            //     stmts = list_append(stmts, expr);
            //     break;
            // }
            // case tk_keyword_match:
            // if (isTypeBody) Parser_errorInvalidTypeMember(parser);
            // tt = parser->token.kind; // either match or case
            expr = Parser_match(parser, tk_keyword_case);
            expr->left = parse_expr(parser);
            Token_advance(&parser->token); // trample null
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parse_scope(parser, scope, false);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it for
            // 'match' or 'if' resp. see later for 'else' as well
            // if (tt == tk_keyword_match) {
            //     Parser_consume(parser, tk_keyword_end);
            //     Parser_ignore(parser, tk_oneSpace);
            //     Parser_ignore(parser, tk_keyword_match);
            // }

            stmts = list_append(stmts, expr);

            break;

        case tk_newline:
        case tk_oneSpace:
        case tk_lineComment: Token_advance(&parser->token); break;

        case tk_keyword_end: goto exitloop;

        default:
            Parser_errorUnexpectedToken(parser, "expected 'case' or 'end'");
            Token_advance(&parser->token);
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE SCOPE
static ast_scope_t* parse_scope(
    parser_t* parser, ast_scope_t* parent, bool isTypeBody) {
    ast_scope_t* scope = NEW(ast_scope_t);

    ast_var_t *var = NULL, *orig = NULL;
    ast_expr_t* expr = NULL;
    tokenkind_e tt = tk_unknown;
    ast_scope_t* forScope = NULL;

    scope->parent = parent;
    bool startedElse = false;
    bool startedCase = false;

    list_t(ast_var_t)** locals = &scope->locals;
    list_t(ast_var_t)** stmts = &scope->stmts;

    while (parser->token.kind != tk_keyword_end) {

        switch (parser->token.kind) {

        case tk_nullChar:
            Parser_errorExpectedToken(parser, tk_unknown);
            goto exitloop;

        case tk_keyword_var:
        case tk_keyword_let: {
            int col = parser->token.col + parser->token.matchlen;
            var = parse_var(parser);
            if (!var)
                continue;
            else
                Token_advance(&parser->token);
            if ((orig = getVar_scope(scope, var->name)))
                Parser_errorDuplicateVar(parser, var, orig->line, orig->col);
            // TODO: why only idents and binops for resolveVars??

            // resolveType(var->typespec, scope);
            // resolve BEFORE it is added to the list! in
            // `var x = x + 1` x should not resolve
            // if var->typespec is NULL then set the type
            // if it isn't NULL then check the types match
            locals = list_append(locals, var);
            // TODO: validation should raise issue if var->init is
            // missing
            expr = NEW(ast_expr_t);
            expr->kind = tk_varAssign;
            expr->line = var->init ? var->init->line : var->line;
            // parser->token.line;
            expr->col = col; // var->init ? var->init->col : 1;
            expr->prec = tokenkind_e_getPrecedence(tk_opAssign);
            expr->var = var;

            // and (var->init->prec or var->init->kind == tk_identifier))
            // TODO: you actually need to send the PtrList item which is
            // generated in the next line as the topExpr, not the expr
            // itself
            if (var->init) resolveVars(parser, var->init, scope, false);

            // TODO: KEEP THE LAST LISTITEM AND APPEND TO THAT!!
            stmts = list_append(stmts, expr);
        } break;

            // case tk_keyword_match:

        case tk_keyword_case:
            //     // if (startedCase) {
            //     //     startedCase = false;
            //     //     break;
            goto exitloop;
        //     // startedCase = true;
        //     startedCase = !startedCase;
        //     if (!startedCase) {
        //         stmts = list_append(stmts, expr);
        //         break;
        //     }
        case tk_keyword_match:
            // expr = parse_scopeMatch(parser);

            if (isTypeBody) Parser_errorInvalidTypeMember(parser);
            // tt = parser->token.kind; // either match or case
            expr = Parser_match(parser, tk_keyword_match);
            expr->left = parse_expr(parser);
            Token_advance(&parser->token);
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parse_scopeCases(parser, scope);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it
            // for 'match' or 'if' resp. see later for 'else' as well
            // if (tt == tk_keyword_match) {
            Parser_consume(parser, tk_keyword_end);
            Parser_ignore(parser, tk_oneSpace);
            Parser_ignore(parser, tk_keyword_match);
            // }
            stmts = list_append(stmts, expr);

            break;

        case tk_keyword_else:
        case tk_keyword_elif:
            if (!startedElse) goto exitloop;
        case tk_keyword_if:
        case tk_keyword_for:
        case tk_keyword_while:
            if (isTypeBody) Parser_errorInvalidTypeMember(parser);
            tt = parser->token.kind;
            expr = Parser_match(parser, tt);
            expr->left = tt != tk_keyword_else ? parse_expr(parser) : NULL;

            // because we are going to be calling resolveVars right now, we
            // need to trample the newline
            Token_advance(&parser->token);

            // if(parser->token.pos)
            // TODO: for must <parse its expr as a VarDecl, because it can
            // have 'as Type' etc. Now you parse an assignment Expr and hack
            // an ast_var_t out of it.
            if (tt == tk_keyword_for) {
                // TODO: new Parser_error
                ast_var_t* fvar = NULL;
                if (!expr->left)
                    unreachable("Missing for-loop condition at %d:%d\n",
                        expr->line, expr->col);
                else {
                    if (expr->left->kind != tk_keyword_in)
                        unreachable("Invalid for-loop condition: %s\n",
                            tokenkind_e_repr[expr->left->kind]);

                    resolveVars(parser, expr->left->right, scope, false);

                    fvar = NEW(ast_var_t);
                    fvar->name = expr->left->left->string;
                    fvar->line = expr->left->line;
                    fvar->col = expr->left->left->col;
                    fvar->isVar = true;
                    fvar->init = expr->left->right;
                    fvar->typespec = NEW(ast_typespec_t);
                    fvar->typespec->typeType = ty_real64;

                    if ((orig = getVar_scope(scope, fvar->name)))
                        Parser_errorDuplicateVar(
                            parser, fvar, orig->line, orig->col);
                }
                forScope = NEW(ast_scope_t);
                if (fvar) list_shift(&forScope->locals, fvar);
                forScope->parent = scope;

                // scope = forScope; // so that when parseScope is called
                // for the child scope, it receives the for variable's scope
                // as parent

            } else if (expr->left) {
                resolveVars(parser, expr->left, scope, false);
            } // TODO: `for` necessarily introduces a counter variable, so
            // check if that var name doesn't already exist in scope.
            // Also assert that the cond of a for expr has kind
            // tk_opAssign.
            // insert a temp scope holding the var that for declares, then
            // later move that var to the parsed scope
            if (tt == tk_keyword_for) {
                // TODO: here it is too late to add the variable,
                // because parseScope will call resolveVars.
                // var = NEW(ast_var_t);
                // var->name = expr->left->left->string;
                // var->init = expr->left->right;
                // var->typespec = NEW(ast_typespec_t);
                // var->typespec->typeType = ty_uInt32;
                // list_append(&expr->body->locals, var);
                expr->body = parse_scope(parser, forScope, isTypeBody);
            } else {
                expr->body = parse_scope(parser, scope, isTypeBody);
            }
            // Mark the scope as a loop scope if it is a 'for' or 'while'.
            expr->body->isLoop = tt == tk_keyword_for || tk_keyword_while;

            if (Parser_matches(parser, tk_keyword_else) || //
                Parser_matches(parser, tk_keyword_elif)) {
                startedElse = true;
            } else {
                Parser_consume(parser, tk_keyword_end);
                Parser_ignore(parser, tk_oneSpace);
                Parser_ignore(parser,
                    tt == tk_keyword_else || tt == tk_keyword_elif
                        ? tk_keyword_if
                        : tt);
            }
            stmts = list_append(stmts, expr);
            break;

        case tk_newline:
        case tk_oneSpace: Token_advance(&parser->token); break;

        case tk_keyword_function:
        case tk_keyword_type:
        case tk_keyword_enum:
        case tk_keyword_test:
            // in this case, it is probably an error propagating all through the
            // file because an early func or type missed an end. How about we
            // stop parsing the scope here.
            goto exitloop;

        case tk_lineComment:
            if (parser->generateCommentExprs) {
                expr = fromToken_expr(&parser->token);
                stmts = list_append(stmts, expr);
            }
            Token_advance(&parser->token);
            break;

        default:
            expr = parse_expr(parser);
            if (expr && isTypeBody) {
                Parser_errorInvalidTypeMember(parser);
                expr = NULL;
            }
            if (!expr) break;
            stmts = list_append(stmts, expr);
            Token_advance(&parser->token); // eat the newline
            resolveVars(parser, expr, scope, false);
            break;
        }
    }
exitloop:
    return scope;
}

static ast_scope_t* parse_enumBody(parser_t* parser, ast_scope_t* globScope) {
    ast_scope_t* scope = NEW(ast_scope_t);
    scope->parent = globScope;
    ast_expr_t* expr = NULL;
    ast_var_t* var = NULL;
    list_t(ast_var_t)** vars = &scope->locals;
    list_t(ast_var_t)** stmts = &scope->stmts;

    while (parser->token.kind != tk_keyword_end) {
        switch (parser->token.kind) {

        case tk_nullChar:
            Parser_errorExpectedToken(parser, tk_unknown);
            goto exitloop;

        case tk_newline:
        case tk_oneSpace: Token_advance(&parser->token); break;

        case tk_lineComment:
            if (parser->generateCommentExprs) {
                expr = fromToken_expr(&parser->token);
                list_append(&scope->stmts, expr);
            }
            Token_advance(&parser->token);
            break;

        case tk_identifier:

            // if (parser->token.kind != tk_identifier) {
            //     Parser_errorInvalidTypeMember(parser);
            //     while (parser->token.pos != tk_newline)
            //         Token_advance(&parser->token);
            //     Token_advance(&parser->token); // eat the newline
            //     break;
            // }

            expr = parse_expr(parser);

            if (!expr) break;
            if (expr->kind != tk_identifier && expr->kind != tk_opAssign) {
                Parser_errorInvalidTypeMember(parser);
                unreachable("%s\n", tokenkind_e_names[expr->kind]);
                expr = NULL;
            }
            stmts = list_append(stmts, expr);
            Token_advance(&parser->token); // eat the newline

            var = NEW(ast_var_t);
            var->typespec = NEW(ast_typespec_t);
            var->line = expr->line;
            var->col
                = (expr->kind == tk_opAssign) ? expr->left->col : expr->col;
            var->name = (expr->kind == tk_opAssign) ? expr->left->string
                                                    : expr->string;
            var->init = expr->right;
            vars = list_append(vars, var);

            // var->typespec->typeType = ty_object;
            // var->typespec->type = ;

            if (expr->kind == tk_opAssign)
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
static list_t(ast_var_t) * parse_params(parser_t* parser) {
    Parser_consume(parser, tk_opLT);
    list_t(ast_var_t) * params;
    ast_var_t* param;
    do {
        param = NEW(ast_var_t);
        param->name = parse_ident(parser);
        if (Parser_ignore(parser, tk_keyword_as))
            param->typespec = parse_typespec(parser);
        if (Parser_ignore(parser, tk_opAssign))
            param->init = parse_expr(parser);
        list_append(&params, param);
    } while (Parser_ignore(parser, tk_opComma));
    Parser_consume(parser, tk_opGT);
    return params;
}

#pragma mark - PARSE FUNC / STMT-FUNC
static ast_func_t* parse_func(
    parser_t* parser, ast_scope_t* globScope, bool shouldParseBody) {
    Parser_consume(parser, tk_keyword_function);
    Parser_consume(parser, tk_oneSpace);
    ast_func_t* func = NEW(ast_func_t);

    func->line = parser->token.line;
    func->col = parser->token.col;

    func->nameLen = parser->token.matchlen;
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parse_ident(parser);
    func->isDeclare = !shouldParseBody;

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // Parser_ignore(parser, tk_exclamation);

    func->args = parse_args(parser);
    func->argCount = list_count(func->args);

    if (mutator && func->argCount) {
        ast_var_t* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    if (Parser_ignore(parser, tk_oneSpace)
        && Parser_ignore(parser, tk_keyword_as)) {
        Parser_consume(parser, tk_oneSpace);
        func->returnSpec = parse_typespec(parser);
    }

    if (shouldParseBody) {
        Parser_consume(parser, tk_newline);

        ast_scope_t* funcScope = NEW(ast_scope_t);
        funcScope->parent = globScope;
        funcScope->locals = func->args;
        func->body = parse_scope(parser, funcScope, false);

        Parser_consume(parser, tk_keyword_end);
        Parser_ignore(parser, tk_oneSpace);
        Parser_ignore(parser, tk_keyword_function);
    }

    return func;
}

static ast_func_t* parse_stmtFunc(parser_t* parser, ast_scope_t* globScope) {
    ast_func_t* func = NEW(ast_func_t);

    func->line = parser->token.line;
    func->isStmt = true;

    func->nameLen = parser->token.matchlen;
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parse_ident(parser);

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // Parser_ignore(parser, tk_exclamation);

    func->args = parse_args(parser);
    func->argCount = list_count(func->args);

    if (mutator && func->argCount) {
        ast_var_t* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    Parser_ignore(parser, tk_oneSpace);

    func->returnSpec = NEW(ast_typespec_t);
    func->returnSpec->line = parser->token.line;
    func->returnSpec->col = parser->token.col;
    func->returnSpec->name = "";

    ast_expr_t* ret = exprFromCurrentToken(parser);

    // if you have toplevel code (eg func call) it tends to reach here
    if (ret->kind != tk_opColEq) return NULL;

    ret->kind = tk_keyword_return;
    ret->unary = true;

    ret->right = parse_expr(parser);
    ast_scope_t* scope = NEW(ast_scope_t);
    list_append(&scope->stmts, ret);

    ast_scope_t* funcScope = NEW(ast_scope_t);
    funcScope->locals = func->args;
    scope->parent = funcScope;
    func->body = scope;

    Parser_consume(parser, tk_newline);
    resolveVars(parser, ret->right, funcScope, false);

    return func;
}

#pragma mark - PARSE TEST
static ast_test_t* parse_test(parser_t* parser, ast_scope_t* globScope) {
    Parser_consume(parser, tk_keyword_test);
    Parser_consume(parser, tk_oneSpace);
    ast_test_t* test = NEW(ast_test_t);

    test->line = parser->token.line;

    if (parser->token.kind != tk_string && parser->token.kind != tk_rawString)
        Parser_errorInvalidTestName(parser);
    test->name = parser->token.pos + 1;
    Token_advance(&parser->token);

    Parser_consume(parser, tk_newline);

    test->body = parse_scope(parser, NULL, false);

    Parser_consume(parser, tk_keyword_end);
    Parser_ignore(parser, tk_oneSpace);
    Parser_ignore(parser, tk_keyword_test);

    return test;
}

#pragma mark - PARSE UNITS
static ast_units_t* parse_units(parser_t* parser) { return NULL; }

#pragma mark - PARSE ty_pE
static ast_type_t* parse_type(
    parser_t* parser, ast_scope_t* globScope, bool shouldParseBody) {
    ast_type_t* type = NEW(ast_type_t);

    Parser_consume(parser, tk_keyword_type);
    Parser_consume(parser, tk_oneSpace);

    type->line = parser->token.line;
    type->col = parser->token.col;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
        Parser_errorInvalidIdent(parser);
    type->name = parse_ident(parser);

    if (Parser_ignore(parser, tk_oneSpace)
        && Parser_ignore(parser, tk_keyword_extends)) {
        Parser_consume(parser, tk_oneSpace);
        type->super = parse_typespec(parser);
    }
    Parser_ignore(parser, tk_newline);

    type->body = NULL; // this means type is declare
    if (typetype_e_byName(type->name) != ty_unresolved) {
        Parser_errorDuplicateType(parser, type, NULL);
        return type;
    }

    // if (!shouldParseBody) return type;

    type->body = parse_scope(parser, globScope, true);

    Parser_consume(parser, tk_keyword_end);
    Parser_ignore(parser, tk_oneSpace);
    Parser_ignore(parser, tk_keyword_type);

    return type;
}

static ast_type_t* parse_enum(parser_t* parser, ast_scope_t* globScope) {
    ast_type_t* en = NEW(ast_type_t);

    Parser_consume(parser, tk_keyword_enum);
    Parser_consume(parser, tk_oneSpace);

    en->line = parser->token.line;
    en->col = parser->token.col;
    en->isEnum = true;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
        Parser_errorInvalidIdent(parser);
    en->name = parse_ident(parser);

    Parser_consume(parser, tk_newline);

    if (typetype_e_byName(en->name) != ty_unresolved) {
        // conflicts with a primitive type name
        Parser_errorDuplicateEnum(parser, en, NULL);
        return en;
    }

    en->body = parse_enumBody(parser, globScope);

    Parser_consume(parser, tk_keyword_end);
    Parser_ignore(parser, tk_oneSpace);
    Parser_ignore(parser, tk_keyword_enum);

    return en;
}

static ast_import_t* parse_import(parser_t* parser, ast_module_t* ownerMod) {
    ast_import_t* import = NEW(ast_import_t);
    char* tmp;
    Parser_consume(parser, tk_keyword_import);
    Parser_consume(parser, tk_oneSpace);

    // import->isPackage = Parser_ignore(parser, tk_at);

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    if (!Parser_matches(parser, tk_identifier)) {
        Parser_errorExpectedToken(parser, tk_identifier);
        return NULL;
    }
    import->name = parser->token.pos; // parse_ident(parser);

    char* cend = import->name;
    while (*cend && (isalnum(*cend) || *cend == '.')) cend++;

    parser->token.pos = cend;
    Token_detect(&parser->token);

    //    size_t len = parser->token.pos - import->name;
    Parser_ignore(parser, tk_oneSpace);
    if (Parser_ignore(parser, tk_keyword_as)) {

        Parser_ignore(parser, tk_oneSpace);
        //      import->hasAlias = true;

        assert(Parser_matches(parser, tk_identifier));
        if (memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser);

        import->aliasOffset = parser->token.pos - import->name;
        parse_ident(parser);

    } else {
        import->aliasOffset
            = CString_base(import->name, '.', parser->token.pos - import->name)
            - import->name;
    }

    char endchar = *parser->token.pos;
    *parser->token.pos = 0;
    if (getImportByAlias_module(ownerMod, import->name + import->aliasOffset)
        || getFuncByName_module(ownerMod, import->name + import->aliasOffset)
        || getVar_module(ownerMod, import->name + import->aliasOffset)) {
        unreachable(
            "import name already used: %s", import->name + import->aliasOffset);
        import = NULL;
    }
    *parser->token.pos = endchar;
    // ^ need to restore the nl since it is used for line counting

    // Parser_ignore(parser, tk_oneSpace);

    // if (parser->token.kind != tk_lineComment && parser->token.kind !=
    // tk_newline)
    //     Parser_errorUnexpectedToken(parser);
    // while (
    //     parser->token.kind != tk_lineComment //
    //     && parser->token.kind != tk_newline//
    //     && parser->token.kind!=tk_nullChar
    //     )
    //     Token_advance(&parser->token);
    return import;
}

void analyseModule(parser_t* parser, ast_module_t* mod);

static ast_func_t* makeDefaultCtor_type(ast_type_t* type) {
    ast_func_t* ctor = NEW(ast_func_t);
    ctor->line = type->line;
    ctor->isDefCtor = true;
    // Ctors must AlWAYS return a new object.
    // even Ctors with args.
    ctor->returnsNewObjectAlways = true;
    ctor->name = type->name;
    char buf[128];
    int l = snprintf(buf, 128, "%s_new_", type->name);
    ctor->selector = CString_pndup(buf, l);
    ast_typespec_t* tspec = new_typespec(ty_object, cty_none);
    tspec->type = type;
    ctor->returnSpec = tspec;
    return ctor;
}

// this func should just be replaced by parseScope handling function type etc

//,
//        .init = (ast_expr_t[]) { { .kind = tk_number, .string = "1" } } }
//}
//;

// vno->vno->;
// ast_var_t* vyes = NEW(ast_var_t);
// vyes->name = "yes";
// vyes->typeType = ty_bool;

// static ast_module_t* Parser_lookupModuleByAlias(PtrList* existingModules,
// char* name) {
//     foreach (ast_module_t*, mod, existingModules) {
//         // eprintf("lookup %s %s\n", name, mod->name);
//         if (!strcasecmp(name, mod->name)) return mod;
//     }
//     return NULL;
// }
static ast_module_t* Parser_lookupModule(PtrList* existingModules, char* name) {
    foreach (ast_module_t*, mod, existingModules) {
        // eprintf("lookup %s %s\n", name, mod->name);
        if (!strcasecmp(name, mod->name)) return mod;
    }
    return NULL;
}

static ast_module_t* parse_module(
    parser_t* parser, PtrList** existingModulesPtr, ast_module_t* importer) {
    ast_module_t* root = NEW(ast_module_t);
    root->name = parser->moduleName;
    // To break recursion, add the root to the existingModules list right away.
    // The name is all that is required for this module to be found later.
    list_shift(existingModulesPtr, root);
    // eprintf("Parsing %s\n", root->name);
    // root->scope = NEW(ast_scope_t);

    // Token parser->token = { .pos = data.ref, .end = data.ref + data.len };
    Token_advance(&parser->token); // maybe put this in parser ctor

    ast_import_t* import = NULL;
    // The take away is (for C gen):
    // Every caller who calls append(List) should keep a local List*
    // to follow the list top as items are appended. Each actual append
    // call must be followed by an update of this pointer to its own
    // ->next. Append should be called on the last item of the list, not
    // the first. (it will work but seek through the whole list every
    // time).

    list_t(ast_func_t)** funcs = &root->funcs;
    list_t(ast_import_t)** imports = &root->imports;
    list_t(ast_type_t)** types = &root->types;
    list_t(ast_test_t)** tests = &root->tests;
    list_t(ast_enum_t)** enums = &root->enums;
    ast_scope_t* gscope = root->scope;
    list_t(ast_var_t)** gvars = &gscope->locals; // globals
    list_t(ast_expr_t)** gexprs = &gscope->stmts; // globals

    // for_to(i, countof(vnoyes))
    // gvars = list_append(gvars, expr_const_empty);
    // gvars = list_append(gvars, expr_const_yes);
    // gvars = list_append(gvars, expr_const_no);
    // gvars = list_append(gvars, expr_const_nil);

    while (parser->token.kind != tk_nullChar) {

        if (parser->mode == PMTokenize) {
            printf("%s %2d %3d %3d %-20s\t%.*s\n", parser->moduleName,
                parser->token.line, parser->token.col, parser->token.matchlen,
                tokenkind_e_names[parser->token.kind],
                parser->token.kind == tk_newline ? 0 : parser->token.matchlen,
                parser->token.pos);
            Token_advance(&parser->token);
            continue;
        }

        switch (parser->token.kind) {

        case tk_keyword_declare:
            Token_advance(&parser->token);
            Parser_consume(parser, tk_oneSpace);
            if (parser->token.kind == tk_keyword_function) {
                ast_func_t* func = parse_func(parser, gscope, false);
                func->isDeclare = true;
                funcs = list_append(funcs, func);
            }
            if (parser->token.kind == tk_keyword_type) {
                ast_type_t* type = parse_type(parser, gscope, false);
                type->isDeclare = true;
                types = list_append(types, type);
            }
            break;

        case tk_keyword_function:
            funcs = list_append(funcs, parse_func(parser, gscope, true));
            break;

        case tk_keyword_enum: {
            ast_type_t* en = parse_enum(parser, gscope);
            enums = list_append(enums, en);
            // add a global of the corresponding type so that it can be used
            // to access members.
            assert(en);
            ast_var_t* enumv = NEW(ast_var_t);
            enumv->name = en->name;
            enumv->line = en->line;
            enumv->col = en->col;
            enumv->typespec = NEW(ast_typespec_t);
            enumv->typespec->typeType = ty_object;
            enumv->typespec->type = en;
            gvars = list_append(gvars, enumv);
        }

        break;

        case tk_keyword_type: {
            ast_type_t* type = parse_type(parser, gscope, true);
            types = list_append(types, type);

            // create default constructor
            ast_func_t* ctor = makeDefaultCtor_type(type);
            funcs = list_append(funcs, ctor);

            // create some extra function declares
            char* defFuncs[] = { "json", "print", "describe" };

            for (int i = 0; i < countof(defFuncs); i++) {
                ast_func_t* func
                    = createDeclWithArg_func(defFuncs[i], NULL, type->name);
                func->line = type->line;
                func->intrinsic = true;
                funcs = list_append(funcs, func);
            }
        } break;

        case tk_keyword_import:
            import = parse_import(parser, root);
            if (import) {

                imports = list_append(imports, import);

                import->module
                    = Parser_lookupModule(parser->modules, import->name);
                if (!import->module) {
                    eprintf("%s needs %s, parsing\n", root->name, import->name);
                    size_t len = strlen(import->name) + 5;
                    char* filename = malloc(len);
                    filename[len - 1] = 0;
                    strcpy(filename, import->name);
                    CString_tr_ip(filename, '.', '/', len - 5);
                    strcpy(filename + len - 5, ".jet");
                    // later you can have a fancier routine that figures out
                    // the file name from a module name
                    parser_t* subParser
                        = Parser_fromFile(filename, true, parser->mode);
                    // parseModule returns the parsed module, and adds dependent
                    // modules to the second argument. dependencies are only
                    // parsed once on first use.
                    // FIXME: these parsers will LEAK!
                    if (subParser) {
                        if ((import->module = parse_module(
                                 parser, existingModulesPtr, NULL)))
                            list_shift(&import->module->importedBy, root);
                    }
                }
            }
            break;

        case tk_keyword_test:
            tests = list_append(tests, parse_test(parser, gscope));
            break;

        case tk_keyword_var:
        case tk_keyword_let:
            // TODO: add these to exprs
            {
                int col = parser->token.col + parser->token.matchlen;
                ast_var_t *var = parse_var(parser), *orig;
                if (!var) {
                    Token_advance(&parser->token);
                    continue;
                }
                if ((orig = getVar_scope(gscope, var->name)))
                    Parser_errorDuplicateVar(
                        parser, var, orig->line, orig->col);
                ast_import_t* imp = getImportByAlias_module(root, var->name);
                ast_func_t* fnc = getFuncByName_module(root, var->name);
                if (imp)
                    Parser_errorDuplicateVar(parser, var, imp->line, imp->col);
                if (fnc) Parser_errorDuplicateVar(parser, var, fnc->line, 5);

                if (var->init) resolveVars(parser, var->init, gscope, false);
                var->isLet = true;
                var->isVar = false;
                gvars = list_append(gvars, var);

                // TODO: validation should raise issue if var->init is
                // missing
                ast_expr_t* expr = NEW(ast_expr_t);
                expr->kind = tk_varAssign;
                expr->line = var->init ? var->init->line : parser->token.line;
                expr->col = col;
                // printf("%d\n", col);
                expr->prec = tokenkind_e_getPrecedence(tk_opAssign);
                expr->var = var;

                // and (var->init->prec or var->init->kind == tk_identifier))
                // TODO: you actually need to send the PtrList item which is
                // generated in the next line as the topExpr, not the expr
                // itself
                if (var->init) resolveVars(parser, var->init, gscope, false);

                gexprs = list_append(gexprs, expr);
            }

            break;
        case tk_newline: *(parser->token.pos) = 0;
        // fallthrough
        case tk_lineComment:
        // TODO: add line comment to module exprs
        case tk_oneSpace: Token_advance(&parser->token); break;
        case tk_identifier: // stmt funcs: f(x) := f(y, w = 4) etc.
            if (Token_peekCharAfter(&parser->token) == '(') {
                funcs = list_append(funcs, parse_stmtFunc(parser, gscope));
                break;
            }
        default:
            Parser_errorUnexpectedToken(parser, "expected a keyword here");
            while (!ISIN(
                3, parser->token.kind, tk_newline, tk_lineComment, tk_nullChar))
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
            ast_func_t* func
                = createDeclWithArg_func(defFuncs[i], retTypes[i], defTypes[j]);
            func->intrinsic = true;
            funcs = list_append(funcs, func);
        }

    // do some analysis that happens after the entire module is loaded
    analyse_module(parser, root);
    // if (importer) list_shift(&importer->importedBy, root);
    return root;
}

// TODO: move this to separate file or to analysis.c [sempass.c]
