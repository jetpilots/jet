
#pragma mark - PARSE EXPR
static ASTExpr* parseExpr(Compiler* com, Lexer& lex) {
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

    static Array<ASTExpr*> rpn, ops, result;
    int prec_top = 0;
    ASTExpr* p = NULL;
    TokenKind revBrkt = tkUnknown;

    // ******* STEP 1 CONVERT TOKENS INTO RPN

    while (!(lex.is(tkNullChar) //
        or lex.is(tkNewline) //
        or lex.is(tkLineComment))) { // build RPN

        // you have to ensure that ops have a space around them, etc.
        // so don't just skip the one spaces like you do now.
        // if (lex.is(tkOneSpace)) advance(lex);
        lex.ignore(tkOneSpace);

        if (lex.is(tkIdentifier) and memchr(lex.pos, '_', lex.matchlen))
            errorInvalidIdent(parser); // but continue parsing

        ASTExpr* expr;
        if (matches(parser, tkParenOpen))
            expr = lparen;
        else if (matches(parser, tkParenClose))
            expr = rparen;
        else
            expr = fromToken(lex); // dont advance yet

        int prec = expr->prec;
        bool rassoc = prec ? expr->rassoc : false;
        char lookAheadChar = Lexer_peekCharAfter(lex);

        switch (expr->kind) {
        case tkIdentifier:
            if (memchr(lex.pos, '_', lex.matchlen))
                errorInvalidIdent(parser); // but continue parsing
            expr->slen = lex.matchlen;
            switch (lookAheadChar) {
            // TODO: need a general lookahead that skips whitespace.
            // case '!':
            //     if (lex.pos[2] != '(') goto defaultCase;
            case '(':
                expr->kind = tkFunctionCall;
                expr->prec = 60;
                ops.push(expr);
                break;
            case '[':
                expr->kind = tkSubscript;
                expr->prec = 60;
                ops.push(expr);
                break;
            case ' ':
                if (lex.pos[2] != '{') goto defaultCase;
                // otherwise fall through
            case '{':
                expr->kind = tkObjectInit;
                expr->prec = 60;
                ops.push(expr);
                break;

            default:
            defaultCase:
                rpn.push(expr);
                break;
            }
            break;

        case tkParenOpen:
            ops.push(expr);
            if (!ops.empty() and ops.top()->is(tkFunctionCall)) rpn.push(expr);
            if (lookAheadChar == ')') rpn.push(NULL);
            // for empty func() push null for no args
            if (lookAheadChar == '&') Lexer_advance(lex);
            // ^ for mutating funcs, & is applied to the first arg on a call
            break;

        case tkArrayOpen:
            ops.push(expr);
            if (!ops.empty() and ops.top()->is(tkSubscript)) rpn.push(expr);
            if (lookAheadChar == ']') rpn.push(NULL);
            // for empty arr[] push null for no args
            break;

        case tkBraceOpen:
            ops.push(expr);
            if (!ops.empty() and ops.top()->is(tkObjectInit)) rpn.push(expr);
            if (lookAheadChar == '}') rpn.push(NULL);
            // for empty Obj {} push null for no args
            break;

        case tkParenClose:
        case tkArrayClose:
        case tkBraceClose:

            revBrkt = TokenKind_reverseBracket(expr->kind);
            if (ops.empty()) {
                // need atleast the opening bracket of the current kind
                errorParsingExpr(parser, expr, "mismatched bracket");
                goto error;
            }

            else
                while (!ops.empty()) {
                    p = ops.pop();
                    if (p->is(revBrkt)) break;
                    rpn.push(p);
                }

            // tkArrayOpen is a unary op.
            if ((p and p->is(tkArrayOpen)))
                if ((ops.empty()
                        or (rpn.top() and ops.top()->kind != tkSubscript))
                    // don't do this if its part of a subscript
                    or (rpn.empty()
                        or (rpn.top() and rpn.top()->kind != tkOpColon)))
                    // or aa range. range exprs are handled separately. by
                    // themselves they don't need a surrounding [], but for
                    // grouping like 2+[8:66] they do.
                    rpn.push(p);

            // a dict literal (another unary op).
            if ((p and p->is(tkBraceOpen))
                and (ops.empty()
                    or (rpn.top() and ops.top()->kind != tkObjectInit)))
                // again, not if it is an object init
                rpn.push(p);
            // Object { member1 = 3, member3 = "train" }
            // ^^ this is an object init, not a dict
            // { "bing" = 34, "whang" = 33 }
            // ^^ this is a dict

            break;

        case tkKeyword_check:
            ops.push(expr);
            break;
        case tkExclamation:
            if (rpn.empty() or rpn.top()->kind != tkIdentifier) {
                errorParsingExpr(parser, expr, "invalid use of '!'");
                // TODO: change error to "invalid use of ! operator or
                // something"
            }
            break;

        case tkKeyword_return:
            // for empty return, push a NULL if there is no expr coming.
            ops.push(expr);
            if (lookAheadChar == '!' or lookAheadChar == '\n') rpn.push(NULL);
            break;

        case tkUnaryDot:
            expr->kind = tkPeriod;
            expr->unary = false;
            // if (rpn.empty())
            rpn.push(NULL);
            // push a NULL which will later be substituted with a dummy var of
            // the inferred enum type.

            // if (!empty( ops)) {
            //     TokenKind kt = top(   ops)->kind;
            //     if (!ISIN(2, kt, tkPeriod, tkFunctionCall))
            //         rpn.push(NULL);
            //     // and rpn.used > 1
            //     //                    and top(
            //     //                    &rpn)->kind != tkIdentifier))
            //     // fallthru
            // }
            fallthrough;
        default:
            if (prec) {
                if (expr->is(tkOpColon)) {
                    if (rpn.empty()
                        or (!rpn.top() and !ops.empty()
                            and ops.top()->kind != tkOpColon)
                        or (rpn.top()->is(tkOpColon) and !ops.empty()
                            and (ops.top()->is(tkOpComma)
                                or ops.top()->is(tkArrayOpen))))
                        // TODO: better way to parse :, 1:, :-1, etc.
                        // while passing tokens to RPN, if you see a :
                        // with nothing on the RPN or comma or [, push a
                        // NULL. while unwinding the op stack, if you
                        // pop a : and see a NULL or comma on the rpn,
                        // push another NULL.
                        rpn.push(&expr_const_0);
                    // indicates empty operand
                }
                while (!ops.empty()) {
                    prec_top = ops.top()->prec;
                    if (!prec_top) break; // left parenthesis
                    if (prec > prec_top) break;
                    if (prec == prec_top and rassoc) break;
                    p = ops.pop();

                    if (!(p->is(tkOpComma) or p->is(tkOpSemiColon)
                            or p->is(tkFunctionCall) or p->is(tkSubscript))
                        and rpn.top() and rpn.top()->is(tkOpComma)) {
                        errorUnexpectedToken(
                            parser, "unsupported use of comma");
                        // TODO: make this an error of unexpected expr instead
                        goto error;
                    }

                    if (!(p->prec or p->unary) and p->kind != tkFunctionCall
                        and p->kind != tkOpColon and p->kind != tkSubscript
                        and rpn.used < 2) {
                        errorUnexpectedToken(
                            parser, "need 2 operands to binary op");
                        // TODO: make this errorUnexpectedExpr
                        goto error;
                    }

                    rpn.push(p);
                }

                if (rpn.empty() and !expr->unary) {
                    errorUnexpectedToken(
                        parser, "binary op with no left operand");
                    // TODO: again unexpected Expr
                    goto error;
                }
                if (expr->is(tkOpColon)
                    and (lookAheadChar == ',' or lookAheadChar == ':'
                        or lookAheadChar == ']' or lookAheadChar == ')'))
                    rpn.push(&expr_const_0);

                ops.push(expr);
            } else {
                rpn.push(expr);
            }
        }
        lex.advance();
        lex.ignore(tkOneSpace);
    }
exitloop:

    while (!ops.empty()) {
        p = ops.pop();

        if (p->kind != tkOpComma and p->kind != tkFunctionCall
            and p->kind != tkSubscript and p->kind != tkArrayOpen and rpn.top()
            and rpn.top()->is(tkOpComma)) {
            errorUnexpectedExpr(parser, rpn.top());
            goto error;
        }

        if (!(p->prec or p->unary)
            and (p->kind != tkFunctionCall and p->kind != tkSubscript)
            and rpn.used < 2) {
            errorParsingExpr(parser, p, "invalid use of comma");
            goto error;
            // TODO: even if you have more than two, neither of the top
            // two should be a comma
        }

        rpn.push(p);
    }

    // *** STEP 2 CONVERT RPN INTO EXPR TREE

    ASTExpr* arg;
    for (int i = 0; i < rpn.used; i++) {
        if (!(p = rpn.ref[i])) goto justpush;
        switch (p->kind) {
        case tkFunctionCall:
        case tkSubscript:
            if (result.used > 0) {
                arg = result.pop();
                if (arg and p->is(tkSubscript)) {
                    // assert(arg->is(tkArrayOpen));
                    if (arg->is(tkArrayOpen)) arg = arg->right;
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
        case tkLineComment:
            break;

        default:
            // everything else is a nonterminal, needs left/right
            if (!p->prec) {
                errorParsingExpr(
                    parser, p, "unexpected token type with no precedence");
                goto error;
            }

            if (result.empty()) {
                errorParsingExpr(parser, p, "no operands");
                goto error;
            }

            p->right = result.pop();

            if (!p->unary) {
                if (result.empty()) {
                    errorParsingExpr(parser, p, "need 2 operands to binary op");
                    goto error;
                }
                p->left = result.pop();
            }
        }
    justpush:
        result.push(p);
    }
    if (!result.used) {
        errorUnexpectedToken(parser, "nothing parsed"); //    (parser, p);
        goto error;
    } else if (result.used != 1) {
        if (result.top()->kind != tkLineComment) {
            errorParsingExpr(parser, p, "more than 1 result");
            goto error;
        }
    }

    ops.used = 0;
    rpn.used = 0;
    result.used = 0;
    return result.ref[0];

error:

    while (lex.pos < com->end
        and (lex.kind != tkNewline and lex.kind != tkLineComment
            and lex.kind != tkNullChar))
        Lexer_advance(lex);

    if (ops.used) {
        eputs("ops: [ ");
        for (ASTExpr* e : ops) {            eprintf("%s ", TokenKind_repr(e->kind, false);
        }
        eputs("];;");
    }

    if (rpn.used) {
        eputs("rpn: [ ");
        for (ASTExpr* e : rpn) {
            if (!e)
                eputs("NUL ");
            else {
                eprintf("%.*s ", 32,
                    e->prec ? TokenKind_repr(e->kind, false) : e->string);
            }
        }
        eputs("];;");
    }

    if (result.used) {
        eputs("result: [ ");
        for (ASTExpr* e : result) {
            if (!e)
                eputs("NUL ");
            else {
                eprintf("%.*s ", 32,
                    e->prec ? TokenKind_repr(e->kind, false) : e->string);
            }
        }
        eputs("];;");
    }

    if (p) {
        eprintf("p: %.*s ", 32,
            p->prec ? TokenKind_repr(p->kind, false) : p->string);
    }
    eputs("\n");

    ops.used = 0; // "reset" stacks
    rpn.used = 0;
    result.used = 0;
    return NULL;
}

#pragma mark - PARSE TYPESPEC
static ASTTypeSpec* parseTypeSpec(Compiler* com) {
    lex.mergeArrayDims = true;

    ASTTypeSpec* typeSpec = NEW(ASTTypeSpec);
    typeSpec->line = lex.line;
    typeSpec->col = lex.col;

    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);

    if (!isalpha(*lex.pos)) errorInvalidIdent(parser);

    typeSpec->name = parseIdent(parser);
    // lex.pos; //
    // Lexer_advance(lex);
    // while (isalnum(lex.pos) or *lex.pos == '.')
    //     lex.pos++;
    // int len = lex.pos - typeSpec->name;
    // Lexer_detect(lex);

    if (matches(parser, tkArrayDims)) {
        if (isalpha(*lex.pos)) {
            // Dict
        } else {
            for (int i = 0; i < lex.matchlen; i++)
                if (lex.pos[i] == ':') typeSpec->dims++;
            if (!typeSpec->dims) typeSpec->dims = 1;
            typeSpec->collectionType
                = typeSpec->dims == 1 ? CTYArray : CTYTensor;
        }
        Lexer_advance(lex);
    }

    ignore(parser, tkUnits);

    assert(lex.kind != tkUnits);
    assert(lex.kind != tkArrayDims);

    lex.mergeArrayDims = false;
    return typeSpec;
}

#pragma mark - PARSE VAR
static ASTVar* parseVar(Compiler* com) {
    ASTVar* var = NEW(ASTVar);
    var->isVar = (lex.is(tkKeyword_var));
    var->isLet = (lex.is(tkKeyword_let));

    if (var->isVar) Lexer_consume(parser, tkKeyword_var);
    if (var->isLet) Lexer_consume(parser, tkKeyword_let);
    if (var->isVar or var->isLet) Lexer_consume(parser, tkOneSpace);

    var->line = lex.line;
    var->col = lex.col;

    lex.mergeArrayDims = true;

    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);
    if (*lex.pos < 'a' or *lex.pos > 'z') errorInvalidIdent(parser);
    var->name = parseIdent(parser);

    // if (matches(parser, tkExclamation)) {
    //     // ! is only used in function arguments. We set isVar here, but the
    //     // caller parseFunc should set isArg on each of its parsed arguments.
    //     // Then the linter/emitter know how to generate the var.
    //     var->isVar = true;
    //     Lexer_advance(lex);
    // }

    int dims = 0;
    // if (matches(parser, tkArrayDims)) {
    //     for (int i = 0; i < lex.matchlen; i++)
    //         if (lex.pos[i] == ':') dims++;
    //     if (! dims) dims = 1;
    //     Lexer_advance(lex);
    // }
    lex.mergeArrayDims = false;

    if (ignore(parser, tkOneSpace) and matches(parser, tkIdentifier))
    //        and ignore(parser, tkKeyword_as)) {
    //      Lexer_consume(parser, tkOneSpace);
    {
        var->typeSpec = parseTypeSpec(parser);
    } else {
        var->typeSpec = NEW(ASTTypeSpec);
        var->typeSpec->line = lex.line;
        var->typeSpec->col = lex.col;
        var->typeSpec->name = "";
    }
    // var->typeSpec->dims = dims;

    ignore(parser, tkOneSpace);
    if (ignore(parser, tkOpAssign)) //
        var->init = parseExpr(parser);

    return var;
}

static List<ASTVar>* parseArgs(Compiler* com) {
    List<ASTVar>* args = NULL;
    Lexer_consume(parser, tkParenOpen);
    if (ignore(parser, tkParenClose)) return args;

    ASTVar* arg;
    do {
        arg = parseVar(parser);
        arg->isArg = true;
        PtrList_append(&args, arg);
    } while (ignore(parser, tkOpComma));

    Lexer_consume(parser, tkParenClose);
    return args;
}
static ASTScope* parseScope(Compiler* com, ASTScope* parent, bool isTypeBody);

static ASTScope* parseScopeCases(Compiler* com, ASTScope* parent) {

    ASTScope* scope = NEW(ASTScope);
    scope->parent = parent;
    List<ASTVar>** stmts = &scope->stmts;
    ASTExpr* expr;

    while (lex.pos < com->end) {
        switch (lex.kind) {

        case tkKeyword_case:
            // if (startedCase) {
            //     startedCase = false;
            //     break;
            // } // goto exitloop;
            // startedCase = true;
            // startedCase = !startedCase;
            // if (!startedCase) {
            //     stmts = PtrList_append(stmts, expr);
            //     break;
            // }
            // case tkKeyword_match:
            // if (isTypeBody) errorInvalidTypeMember(parser);
            // tt = lex.kind; // either match or case
            expr = match(parser, tkKeyword_case);
            expr->left = parseExpr(parser);
            Lexer_advance(lex); // trample null
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScope(parser, scope, false);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it for
            // 'match' or 'if' resp. see later for 'else' as well
            // if (tt == tkKeyword_match) {
            //     Lexer_consume(parser, tkKeyword_end);
            //     ignore(parser, tkOneSpace);
            //     ignore(parser, tkKeyword_match);
            // }

            stmts = PtrList_append(stmts, expr);

            break;

        case tkNewline:
        case tkOneSpace:
        case tkLineComment:
            Lexer_advance(lex);

            break;

        case tkKeyword_end:
            goto exitloop;

        default:
            errorUnexpectedToken(parser, "expected 'case' or 'end'");
            Lexer_advance(lex);
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE SCOPE
static ASTScope* parseScope(Compiler* com, ASTScope* parent, bool isTypeBody) {
    ASTScope* scope = NEW(ASTScope);

    ASTVar *var = NULL, *orig = NULL;
    ASTExpr* expr = NULL;
    TokenKind tt = tkUnknown;
    ASTScope* forScope = NULL;

    scope->parent = parent;
    bool startedElse = false;
    bool startedCase = false;

    List<ASTVar>** locals = &scope->locals;
    List<ASTVar>** stmts = &scope->stmts;

    while (lex.kind != tkKeyword_end) {

        switch (lex.kind) {

        case tkNullChar:
            errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkKeyword_var:
        case tkKeyword_let:
            var = parseVar(parser);
            if (!var)
                continue;
            else
                Lexer_advance(lex);
            if ((orig = getVar(scope, var->name)))
                errorDuplicateVar(parser, var, orig->line, orig->col);
            // TODO: why only idents and binops for resolveVars??

            // resolveType(var->typeSpec, scope);
            // resolve BEFORE it is added to the list! in
            // `var x = x + 1` x should not resolve
            // if var->typeSpec is NULL then set the type
            // if it isn't NULL then check the types match
            locals = PtrList_append(locals, var);
            // TODO: validation should raise issue if var->init is
            // missing
            expr = NEW(ASTExpr);
            expr->kind = tkVarAssign;
            expr->line = var->init ? var->init->line : var->line;
            // lex.line;
            expr->col = var->init ? var->init->col : 1;
            expr->prec = TokenKind_getPrecedence(tkOpAssign);
            expr->var = var;

            // and (var->init->prec or var->init->is(tkIdentifier)))
            // TODO: you actually need to send the PtrList item which is
            // generated in the next line as the topExpr, not the expr
            // itself
            if (var->init) resolveVars(parser, var->init, scope, false);

            // TODO: KEEP THE LAST LISTITEM AND APPEND TO THAT!!
            stmts = PtrList_append(stmts, expr);
            break;

            // case tkKeyword_match:

        case tkKeyword_case:
            //     // if (startedCase) {
            //     //     startedCase = false;
            //     //     break;
            goto exitloop;
        //     // startedCase = true;
        //     startedCase = !startedCase;
        //     if (!startedCase) {
        //         stmts = PtrList_append(stmts, expr);
        //         break;
        //     }
        case tkKeyword_match:
            // expr = parseScopeMatch(parser);

            if (isTypeBody) errorInvalidTypeMember(parser);
            // tt = lex.kind; // either match or case
            expr = match(parser, tkKeyword_match);
            expr->left = parseExpr(parser);
            Lexer_advance(lex);
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScopeCases(parser, scope);
            // match's body is a scope full of cases
            // case's body is a scope

            // 'case' and 'else' should never consume the 'end', leave it
            // for 'match' or 'if' resp. see later for 'else' as well
            // if (tt == tkKeyword_match) {
            Lexer_consume(parser, tkKeyword_end);
            ignore(parser, tkOneSpace);
            ignore(parser, tkKeyword_match);
            // }
            stmts = PtrList_append(stmts, expr);

            break;

        case tkKeyword_else:
        case tkKeyword_elif:
            if (!startedElse) goto exitloop;
        case tkKeyword_if:
        case tkKeyword_for:
        case tkKeyword_while:
            if (isTypeBody) errorInvalidTypeMember(parser);
            tt = lex.kind;
            expr = match(parser, tt);
            expr->left = tt != tkKeyword_else ? parseExpr(parser) : NULL;

            // because we are going to be calling resolveVars right now, we
            // need to trample the newline
            Lexer_advance(lex);

            // if(lex.pos)
            // TODO: for must <parse its expr as a VarDecl, because it can
            // have 'as Type' etc. Now you parse an assignment Expr and hack
            // an ASTVar out of it.
            if (tt == tkKeyword_for) {
                // TODO: new error
                ASTVar* fvar = NULL;
                if (!expr->left)
                    unreachable("Missing for-loop condition at %d:%d\n",
                        expr->line, expr->col);
                else {
                    if (expr->left->kind != tkKeyword_in)
                        unreachable("Invalid for-loop condition: %s\n",
                            TokenKind_repr(expr->left->kind, false));

                    resolveVars(parser, expr->left->right, scope, false);

                    fvar = NEW(ASTVar);
                    fvar->name = expr->left->left->string;
                    fvar->line = expr->left->line;
                    fvar->col = expr->left->left->col;
                    fvar->isVar = true;
                    fvar->init = expr->left->right;
                    fvar->typeSpec = NEW(ASTTypeSpec);
                    fvar->typeSpec->typeType = TYReal64;

                    if ((orig = getVar(scope, fvar->name)))
                        errorDuplicateVar(parser, fvar, orig->line, orig->col);
                }
                forScope = NEW(ASTScope);
                if (fvar) PtrList_shift(&forScope->locals, fvar);
                forScope->parent = scope;

                // scope = forScope; // so that when parseScope is called
                // for the child scope, it receives the for variable's scope
                // as parent

            } else if (expr->left) {
                resolveVars(parser, expr->left, scope, false);
            } // TODO: `for` necessarily introduces a counter variable, so
            // check if that var name doesn't already exist in scope.
            // Also assert that the cond of a for expr has kind
            // tkOpAssign.
            // insert a temp scope holding the var that for declares, then
            // later move that var to the parsed scope
            if (tt == tkKeyword_for) {
                // TODO: here it is too late to add the variable,
                // because parseScope will call resolveVars.
                // var = NEW(ASTVar);
                // var->name = expr->left->left->string;
                // var->init = expr->left->right;
                // var->typeSpec = NEW(ASTTypeSpec);
                // var->typeSpec->typeType = TYUInt32;
                // PtrList_append(&expr->body->locals, var);
                expr->body = parseScope(parser, forScope, isTypeBody);

            } else {
                expr->body = parseScope(parser, scope, isTypeBody);
            }

            if (matches(parser, tkKeyword_else) or //
                matches(parser, tkKeyword_elif)) {
                startedElse = true;
            } else {
                Lexer_consume(parser, tkKeyword_end);
                ignore(parser, tkOneSpace);
                ignore(parser,
                    tt == tkKeyword_else or tt == tkKeyword_elif ? tkKeyword_if
                                                                 : tt);
            }
            stmts = PtrList_append(stmts, expr);
            break;

        case tkNewline:
        case tkOneSpace:
            Lexer_advance(lex);
            break;

        case tkKeyword_function:
        case tkKeyword_type:
        case tkKeyword_enum:
        case tkKeyword_test:
            // in this case, it is probably an error propagating all through the
            // file because an early func or type missed an end. How about we
            // stop parsing the scope here.
            goto exitloop;

        case tkLineComment:
            if (com->generateCommentExprs) {
                expr = fromToken(lex);
                stmts = PtrList_append(stmts, expr);
            }
            Lexer_advance(lex);
            break;

        default:
            expr = parseExpr(parser);
            if (expr and isTypeBody) {
                errorInvalidTypeMember(parser);
                expr = NULL;
            }
            if (!expr) break;
            stmts = PtrList_append(stmts, expr);
            Lexer_advance(lex); // eat the newline
            resolveVars(parser, expr, scope, false);
            break;
        }
    }
exitloop:
    return scope;
}

static ASTScope* parseEnumBody(Compiler* com, ASTScope* globScope) {
    ASTScope* scope = NEW(ASTScope);
    scope->parent = globScope;
    ASTExpr* expr = NULL;
    ASTVar* var = NULL;
    List<ASTVar>** vars = &scope->locals;
    List<ASTVar>** stmts = &scope->stmts;

    while (lex.kind != tkKeyword_end) {
        switch (lex.kind) {

        case tkNullChar:
            errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkNewline:
        case tkOneSpace:
            Lexer_advance(lex);
            break;

        case tkLineComment:
            if (com->generateCommentExprs) {
                expr = fromToken(lex);
                PtrList_append(&scope->stmts, expr);
            }
            Lexer_advance(lex);
            break;

        case tkIdentifier:

            // if (lex.kind != tkIdentifier) {
            //     errorInvalidTypeMember(parser);
            //     while (lex.pos != tkNewline)
            //         Lexer_advance(lex);
            //     Lexer_advance(lex); // eat the newline
            //     break;
            // }

            expr = parseExpr(parser);

            if (!expr) break;
            if (expr->kind != tkIdentifier and expr->kind != tkOpAssign) {
                errorInvalidTypeMember(parser);
                unreachable("%s\n", TokenKind_str[expr->kind]);
                expr = NULL;
            }
            stmts = PtrList_append(stmts, expr);
            Lexer_advance(lex); // eat the newline

            var = NEW(ASTVar);
            var->typeSpec = NEW(ASTTypeSpec);
            var->line = expr->line;
            var->col = (expr->is(tkOpAssign)) ? expr->left->col : expr->col;
            var->name
                = (expr->is(tkOpAssign)) ? expr->left->string : expr->string;
            var->init = expr->right;
            vars = PtrList_append(vars, var);

            // var->typeSpec->typeType = TYObject;
            // var->typeSpec->type = ;

            if (expr->is(tkOpAssign))
                resolveVars(parser, expr->right, scope, false);

            break;

        default:
            errorExpectedToken(parser, lex.kind);
            goto exitloop;
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE PARAM
static List<ASTVar>* parseParams(Compiler* com) {
    Lexer_consume(parser, tkOpLT);
    List<ASTVar>* params;
    ASTVar* param;
    do {
        param = NEW(ASTVar);
        param->name = parseIdent(parser);
        if (ignore(parser, tkKeyword_as))
            param->typeSpec = parseTypeSpec(parser);
        if (ignore(parser, tkOpAssign)) param->init = parseExpr(parser);
        PtrList_append(&params, param);
    } while (ignore(parser, tkOpComma));
    Lexer_consume(parser, tkOpGT);
    return params;
}

#pragma mark - PARSE FUNC / STMT-FUNC
static ASTFunc* parseFunc(
    Compiler* com, ASTScope* globScope, bool shouldParseBody) {
    Lexer_consume(parser, tkKeyword_function);
    Lexer_consume(parser, tkOneSpace);
    ASTFunc* func = NEW(ASTFunc);

    func->line = lex.line;

    func->nameLen = lex.matchlen;
    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);
    func->name = parseIdent(parser);
    func->isDeclare = !shouldParseBody;

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // ignore(parser, tkExclamation);

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);

    if (mutator and func->argCount) {
        ASTVar* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    if (ignore(parser, tkOneSpace) and ignore(parser, tkKeyword_as)) {
        Lexer_consume(parser, tkOneSpace);
        func->returnSpec = parseTypeSpec(parser);
    }

    if (shouldParseBody) {
        Lexer_consume(parser, tkNewline);

        ASTScope* funcScope = NEW(ASTScope);
        funcScope->parent = globScope;
        funcScope->locals = func->args;
        func->body = parseScope(parser, funcScope, false);

        Lexer_consume(parser, tkKeyword_end);
        ignore(parser, tkOneSpace);
        ignore(parser, tkKeyword_function);
    }

    return func;
}

static ASTFunc* parseStmtFunc(Compiler* com, ASTScope* globScope) {
    ASTFunc* func = NEW(ASTFunc);

    func->line = lex.line;
    func->isStmt = true;

    func->nameLen = lex.matchlen;
    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);
    func->name = parseIdent(parser);

    bool mutator = func->name[func->nameLen - 1] == '!';
    if (mutator) func->nameLen--;
    // ignore(parser, tkExclamation);

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);

    if (mutator and func->argCount) {
        ASTVar* arg1 = func->args->item;
        arg1->isVar = true;
        func->mutator = true;
    }

    ignore(parser, tkOneSpace);

    func->returnSpec = NEW(ASTTypeSpec);
    func->returnSpec->line = lex.line;
    func->returnSpec->col = lex.col;
    func->returnSpec->name = "";

    ASTExpr* ret = exprFromCurrentToken(parser);

    // if you have toplevel code (eg func call) it tends to reach here
    if (ret->kind != tkColEq) return NULL;

    ret->kind = tkKeyword_return;
    ret->unary = true;

    ret->right = parseExpr(parser);
    ASTScope* scope = NEW(ASTScope);
    PtrList_append(&scope->stmts, ret);

    ASTScope* funcScope = NEW(ASTScope);
    funcScope->locals = func->args;
    scope->parent = funcScope;
    func->body = scope;

    Lexer_consume(parser, tkNewline);
    resolveVars(parser, ret->right, funcScope, false);

    return func;
}

#pragma mark - PARSE TEST
static ASTTest* parseTest(Compiler* com, ASTScope* globScope) {
    Lexer_consume(parser, tkKeyword_test);
    Lexer_consume(parser, tkOneSpace);
    ASTTest* test = NEW(ASTTest);

    test->line = lex.line;

    if (lex.kind != tkString and lex.kind != tkRawString)
        errorInvalidTestName(parser);
    test->name = lex.pos + 1;
    Lexer_advance(lex);

    Lexer_consume(parser, tkNewline);

    test->body = parseScope(parser, NULL, false);

    Lexer_consume(parser, tkKeyword_end);
    ignore(parser, tkOneSpace);
    ignore(parser, tkKeyword_test);

    return test;
}

#pragma mark - PARSE UNITS
static ASTUnits* parseUnits(Compiler* com) { return NULL; }

#pragma mark - PARSE TYPE
static ASTType* parseType(
    Compiler* com, ASTScope* globScope, bool shouldParseBody) {
    ASTType* type = NEW(ASTType);

    Lexer_consume(parser, tkKeyword_type);
    Lexer_consume(parser, tkOneSpace);

    type->line = lex.line;
    type->col = lex.col;

    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);
    if (*lex.pos < 'A' or *lex.pos > 'Z') errorInvalidIdent(parser);
    type->name = parseIdent(parser);

    if (ignore(parser, tkOneSpace) and ignore(parser, tkKeyword_extends)) {
        Lexer_consume(parser, tkOneSpace);
        type->super = parseTypeSpec(parser);
    }
    ignore(parser, tkNewline);

    type->body = NULL; // this means type is declare
    if (TypeType_byName(type->name) != TYUnresolved) {
        errorDuplicateType(parser, type, NULL);
        return type;
    }

    // if (!shouldParseBody) return type;

    type->body = parseScope(parser, globScope, true);

    Lexer_consume(parser, tkKeyword_end);
    ignore(parser, tkOneSpace);
    ignore(parser, tkKeyword_type);

    return type;
}

static ASTType* parseEnum(Compiler* com, ASTScope* globScope) {
    ASTType* en = NEW(ASTType);

    Lexer_consume(parser, tkKeyword_enum);
    Lexer_consume(parser, tkOneSpace);

    en->line = lex.line;
    en->col = lex.col;
    en->isEnum = true;

    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);
    if (*lex.pos < 'A' or *lex.pos > 'Z') errorInvalidIdent(parser);
    en->name = parseIdent(parser);

    Lexer_consume(parser, tkNewline);

    if (TypeType_byName(en->name) != TYUnresolved) {
        // conflicts with a primitive type name
        errorDuplicateEnum(parser, en, NULL);
        return en;
    }

    en->body = parseEnumBody(parser, globScope);

    Lexer_consume(parser, tkKeyword_end);
    ignore(parser, tkOneSpace);
    ignore(parser, tkKeyword_enum);

    return en;
}

static ASTImport* parseImport(Compiler* com, ASTModule* ownerMod) {
    ASTImport* import = NEW(ASTImport);
    char* tmp;
    Lexer_consume(parser, tkKeyword_import);
    Lexer_consume(parser, tkOneSpace);

    // import->isPackage = ignore(parser, tkAt);

    if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);

    if (!matches(parser, tkIdentifier)) {
        errorExpectedToken(parser, tkIdentifier);
        return NULL;
    }
    import->name = lex.pos; // parseIdent(parser);

    char* cend = import->name;
    while (*cend and (isalnum(*cend) or *cend == '.')) cend++;

    lex.pos = cend;
    Lexer_detect(lex);

    //    size_t len = lex.pos - import->name;
    ignore(parser, tkOneSpace);
    if (ignore(parser, tkKeyword_as)) {

        ignore(parser, tkOneSpace);
        //      import->hasAlias = true;

        assert(matches(parser, tkIdentifier));
        if (memchr(lex.pos, '_', lex.matchlen)) errorInvalidIdent(parser);

        import->aliasOffset = lex.pos - import->name;
        parseIdent(parser);

    } else {
        import->aliasOffset
            = CString_base(import->name, '.', lex.pos - import->name)
            - import->name;
    }

    char endchar = *lex.pos;
    *lex.pos = 0;
    if (getImportByAlias(ownerMod, import->name + import->aliasOffset)
        or getFuncByName(ownerMod, import->name + import->aliasOffset)
        or getVar(ownerMod, import->name + import->aliasOffset)) {
        unreachable(
            "import name already used: %s", import->name + import->aliasOffset);
        import = NULL;
    }
    *lex.pos = endchar;
    // ^ need to restore the nl since it is used for line counting

    // ignore(parser, tkOneSpace);

    // if (lex.kind != tkLineComment and lex.kind !=
    // tkNewline)
    //     errorUnexpectedToken(parser);
    // while (
    //     lex.kind != tkLineComment //
    //     and lex.kind != tkNewline//
    //     and lex.kind!=tkNullChar
    //     )
    //     Lexer_advance(lex);
    return import;
}

void analyseModule(Compiler* com, ASTModule* mod);

static makeDefaultCtor(ASTType* type) {
    ASTFunc* ctor = NEW(ASTFunc);
    ctor->line = type->line;
    ctor->isDefCtor = true;
    // Ctors must AlWAYS return a new object.
    // even Ctors with args.
    ctor->returnsNewObjectAlways = true;
    ctor->name = type->name;
    char buf[128];
    int l = snprintf(buf, 128, "%s_new_", type->name);
    ctor->selector = CString_pndup(buf, l);
    new (TYObject, CTYNone);
    tspec->type = type;
    ctor->returnSpec = tspec;
    return ctor;
}

// this func should just be replaced by parseScope handling function type etc

//,
//        .init = (ASTExpr[]) { { .kind = tkNumber, .string = "1" } } }
//}
//;

// vno->vno->;
// ASTVar* vyes = NEW(ASTVar);
// vyes->name = "yes";
// vyes->typeType = TYBool;

// static lookupModuleByAlias(PtrList* existingModules, char*
// name) {
//     for (ASTModule& mod : existingModules) {
//         // eprintf("lookup %s %s\n", name, mod->name);
//         if (!strcasecmp(name, mod->name)) return mod;
//     }
//     return NULL;
// }
static lookupModule(PtrList* existingModules, char* name) {
    for (ASTModule& mod : existingModules) {
        // eprintf("lookup %s %s\n", name, mod->name);
        if (!strcasecmp(name, mod->name)) return mod;
    }
    return NULL;
}

static ASTModule* parseModule(Compiler* com, const char* filename) {
    ASTModule* root = NEW(ASTModule);
    root->fqname = CString_tr_ip(CString_noext(filename), '/', '.');

    String data = slurp(filename);
    root->reporter.lines = String_splitlines(String_clone(data));
    root->filename = strdup(filename);
    root->reporter.filename = filename

        // To break recursion, add the root to the existing modules list right
        // away. The name is all that is required for this module to be found
        // later. PtrList_shift(existingModulesPtr, root);
        PtrList_shift(&com->modules, root);

    Lexer lex = { .pos = data.ref, .end = data.ref + data.len };
    Lexer_advance(lex); // maybe put this in parser ctor

    ASTImport* import = NULL;
    // The take away is (for C gen):
    // Every caller who calls append(List) should keep a local List*
    // to follow the list top as items are appended. Each actual append
    // call must be followed by an update of this pointer to its own
    // ->next. Append should be called on the last item of the list, not
    // the first. (it will work but seek through the whole list every
    // time).

    List<ASTFunc>** funcs = &root->funcs;
    List<ASTImport>** imports = &root->imports;
    List<ASTType>** types = &root->types;
    List<ASTTest>** tests = &root->tests;
    List<ASTEnum>** enums = &root->enums;
    ASTScope* gscope = root->scope;
    List<ASTVar>** gvars = &gscope->locals; // globals
    List<ASTExpr>** gexprs = &gscope->stmts; // globals

    // for_to(i, countof(vnoyes))
    // gvars = PtrList_append(gvars, expr_const_empty);
    // gvars = PtrList_append(gvars, expr_const_yes);
    // gvars = PtrList_append(gvars, expr_const_no);
    // gvars = PtrList_append(gvars, expr_const_nil);

    while (lex.kind != tkNullChar) {

        if (com->opts.mode == PMTokenize) {
            printf("%s %2d %3d %3d %-20s\t%.*s\n", com->moduleName, lex.line,
                lex.col, lex.matchlen, TokenKind_str[lex.kind],
                lex.is(tkNewline) ? 0 : lex.matchlen, lex.pos);
            Lexer_advance(lex);
            continue;
        }

        switch (lex.kind) {

        case tkKeyword_declare:
            Lexer_advance(lex);
            Lexer_consume(lex, tkOneSpace);
            if (lex.is(tkKeyword_function)) {
                ASTFunc* func = parseFunc(parser, gscope, false);
                func->isDeclare = true;
                funcs = PtrList_append(funcs, func);
            }
            if (lex.is(tkKeyword_type)) {
                ASTType* type = parseType(parser, gscope, false);
                type->isDeclare = true;
                types = PtrList_append(types, type);
            }
            break;

        case tkKeyword_function:
            funcs = PtrList_append(funcs, parseFunc(parser, gscope, true));
            break;

        case tkKeyword_enum: {
            ASTType* en = parseEnum(parser, gscope);
            enums = PtrList_append(enums, en);
            // add a global of the corresponding type so that it can be used
            // to access members.
            assert(en);
            ASTVar* enumv = NEW(ASTVar);
            enumv->name = en->name;
            enumv->line = en->line;
            enumv->col = en->col;
            enumv->typeSpec = NEW(ASTTypeSpec);
            enumv->typeSpec->typeType = TYObject;
            enumv->typeSpec->type = en;
            gvars = PtrList_append(gvars, enumv);
        }

        break;

        case tkKeyword_type: {
            ASTType* type = parseType(parser, gscope, true);
            types = PtrList_append(types, type);

            // create default constructor
            makeDefaultCtor(type);
            funcs = PtrList_append(funcs, ctor);

            // create some extra function declares
            char* defFuncs[] = { "json", "print", "describe" };

            for (int i = 0; i < countof(defFuncs); i++) {
                ASTFunc* func
                    = createDeclWithArg(defFuncs[i], NULL, type->name);
                func->line = type->line;
                func->intrinsic = true;
                funcs = PtrList_append(funcs, func);
            }
        } break;

        case tkKeyword_import:
            import = parseImport(parser, root);
            if (import) {

                imports = PtrList_append(imports, import);

                import->module = lookupModule(com->modules, import->name);
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
                    // Parser* subParser
                    //     = fromFile(filename, true, com->mode);
                    // parseModule returns the parsed module, and adds dependent
                    // modules to the second argument. dependencies are only
                    // parsed once on first use.
                    // FIXME: these parsers will LEAK!
                    // if (subParser) {
                    if ((import->module = parseModule(com, filename)))
                        PtrList_shift(&import->module->importedBy, root);
                    // }
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
                ASTVar *var = parseVar(parser), *orig;
                if (!var) {
                    Lexer_advance(lex);
                    continue;
                }
                if ((orig = getVar(gscope, var->name)))
                    errorDuplicateVar(parser, var, orig->line, orig->col);
                getImportByAlias(root, var->name);
                getFuncByName(root, var->name);
                if (imp) errorDuplicateVar(parser, var, imp->line, imp->col);
                if (fnc) errorDuplicateVar(parser, var, fnc->line, 5);

                if (var->init) resolveVars(parser, var->init, gscope, false);
                var->isLet = true;
                var->isVar = false;
                gvars = PtrList_append(gvars, var);

                // TODO: validation should raise issue if var->init is
                // missing
                ASTExpr* expr = NEW(ASTExpr);
                expr->kind = tkVarAssign;
                expr->line = var->init ? var->init->line : lex.line;
                expr->col = var->init ? var->init->col : 1;
                expr->prec = TokenKind_getPrecedence(tkOpAssign);
                expr->var = var;

                // and (var->init->prec or var->init->is(tkIdentifier)))
                // TODO: you actually need to send the PtrList item which is
                // generated in the next line as the topExpr, not the expr
                // itself
                if (var->init) resolveVars(parser, var->init, gscope, false);

                gexprs = PtrList_append(gexprs, expr);
            }

            break;
        case tkNewline:
            *(lex.pos) = 0;
        // fallthrough
        case tkLineComment:
        // TODO: add line comment to module exprs
        case tkOneSpace:
            Lexer_advance(lex);
            break;
        case tkIdentifier: // stmt funcs: f(x) := f(y, w = 4) etc.
            if (Lexer_peekCharAfter(lex) == '(') {
                funcs = PtrList_append(funcs, parseStmtFunc(parser, gscope));
                break;
            }
        default:
            errorUnexpectedToken(parser, "expected a keyword here");
            while (!ISIN(3, lex.kind, tkNewline, tkLineComment, tkNullChar))
                Lexer_advance(lex);
        }
    }

    // Add some default functions "built-ins"
    // TODO: move this into a function later
    char* defTypes[] = { "String", "Number", "Boolean" };
    char* defFuncs[] = { "json", "print", "describe" };
    char* retTypes[countof(defFuncs)] = {}; // fill these for non-void funcs

    for (int j = 0; j < countof(defTypes); j++)
        for (int i = 0; i < countof(defFuncs); i++) {
            createDeclWithArg(defFuncs[i], retTypes[i], defTypes[j]);
            func->intrinsic = true;
            funcs = PtrList_append(funcs, func);
        }

    // do some analysis that happens after the entire module is loaded
    analyseModule(parser, root);
    // if (importer) PtrList_shift(&importer->importedBy, root);
    return root;
}

// TODO: move this to separate file or to analysis.c [sempass.c]

static void unmarkTypesVisited(ASTModule* mod);
static int markTypesVisited(Compiler* com, ASTExpr* expr);
static int checkCycles(Compiler* com, ASTType* type);

void analyseModule(Compiler* com, ASTModule* mod) {
    // If function calls are going to be resolved based on the type of
    // first arg, then ALL functions must be visited in order to
    // generate their selectors and resolve their typespecs. (this does
    // not set the resolved flag on the func -- that is done by the
    // semantic pass)
    for (ASTFunc& func : mod->funcs) {
        for (ASTVar& arg : func->args)
            resolveTypeSpec(parser, arg->typeSpec, mod);
        if (func->returnSpec) resolveTypeSpec(parser, func->returnSpec, mod);
        getSelector(func);
    }

    ASTFunc* fstart = NULL;
    // don't break on the first match, keep looking so that duplicate starts
    // can be found
    for (ASTFunc& func : mod->funcs)
        if (!strcmp(func->name, "start")) fstart = func;

    // If we are linting, the whole file must be analysed. this happens
    // regardless of whether start was found or not
    if (com->mode == PMTest or com->mode == PMLint) {
        for (ASTExpr& stmt : mod->scope->stmts)
            analyseExpr(parser, stmt, mod->scope, mod, NULL, false);
        // for (ASTVar& var : mod->scope->locals)
        //     if (var->init)
        //         analyseExpr(parser, var->init, mod->scope, mod, false);
        for (ASTTest& test : mod->tests) analyseTest(parser, test, mod);
        for (ASTFunc& func : mod->funcs) analyse(parser, func, mod);
        for (ASTType& type : mod->types) analyseType(parser, type, mod);
        for (ASTType& en : mod->enums) analyseType(parser, en, mod);

    } else if (fstart) {
        /* TODO: what if you have tests and a start()? Now you will have to
         analyse the tests anyway */
        for (ASTExpr& stmt : mod->scope->stmts)
            analyseExpr(parser, stmt, mod->scope, mod, NULL, false);
        // for (ASTVar& var : mod->scope->locals)
        //     if (var->init)
        //         analyseExpr(parser, var->init, mod->scope, mod, false);

        analyse(parser, fstart, mod);
        // Check dead code -- unused funcs and types, and report warnings.
        for (ASTFunc& func : mod->funcs)
            if (!func->analysed and !func->isDefCtor)
                warnUnusedFunc(parser, func);
        for (ASTType& type : mod->types)
            if (!type->analysed) warnUnusedType(parser, type);

    } else { // TODO: new error, unless you want to get rid of start
        eputs("\n\e[31m*** error:\e[0m cannot find function "
              "\e[33mstart\e[0m.\n");
        com->issues.errCount++;
    }

    // Check each type for cycles in inheritance graph.
    // Actually if there is no inheritance and composition is favoured, you
    // have to check each statement in the type body instead of just walking
    // up the super chain. If any statements are initialized by
    // constructors, mark the type of that statement as visited and recur
    // into that type to check its statements to see if you ever revisit
    // anything. Unfortunately it does not seem that this would be easy to
    // do iteratively (not recursively), as it can be done for just checking
    // supers. for (ASTType& type : mod->types) {
    //     if (! type->analysed or not type->super) continue;
    //     assert(type->super->typeType == TYObject);

    //     // traverse the type hierarchy for this type and see if you
    //     revisit any ASTType* superType = type->super->type; while
    //     (superType) {
    //         if (superType->visited) {
    //             errorInheritanceCycle(self, type);
    //             break;
    //         }
    //         superType->visited = true;
    //         if (! superType->super) break;
    //         assert(superType->super->typeType == TYObject);
    //         superType = superType->super->type;
    //     }

    //     // reset the cycle check flag on all types
    //     for (ASTType& etype : mod->types)
    //         if (type->analysed) etype->visited = false;
    // }

    // check each stmt in each type to find cycles.
    for (ASTType& type : mod->types)
        if (type->analysed and type->body and !type->visited) {
            if (checkCycles(parser, type)) {
                // cycle was detected. err has been reported along with a
                // backtrace. now just unset the dim control codes.
                eprintf(" ...%s\n", "\e[0m");
                // just report the first cycle found. typically there will
                // be only one cycle and you will end up reporting the same
                // cycle for all types that are in it, which is useless.
                // break;
                // the last type (for which the error was reported) won't
                // have its cycle check flags cleared, but who cares. OTHER
                // IDEA: clear the flag only if there was no error. that way
                // the next iteration will skip over those whose flags are
                // already set.
            } else
                unmarkTypesVisited(mod);
        }
}

// return 0 on no cycle found, -1 on cycle found
static int checkCycles(Compiler* com, ASTType* type) {
    for (ASTExpr& stmt : type->body->stmts)
        if (markTypesVisited(parser, stmt)) {
            eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
                type->name, com->filename, stmt->line, stmt->col);
            return -1;
        }
    return 0;
}

static int markTypesVisited(Compiler* com, ASTExpr* expr) {
    ASTType* type = NULL;
    if (!expr) return 0;
    switch (expr->kind) {
    case tkVarAssign:
        return markTypesVisited(parser, expr->var->init);
    case tkFunctionCall:
        return markTypesVisited(parser, expr->left);
    case tkFunctionCallResolved:
        if (markTypesVisited(parser, expr->left)) return -1;
        // if (expr->func->isDefCtor) type =
        // expr->func->returnSpec->type;
        if (expr->func->returnSpec->typeType == TYObject
            and expr->func->returnsNewObjectAlways)
            type = expr->func->returnSpec->type;
        break;
    case tkSubscript:
    case tkSubscriptResolved:
        return markTypesVisited(parser, expr->left);
        // case tkKeyword_if:
        // case tkKeyword_elif:
        // case tkKeyword_case:
        // case tkKeyword_match:

    case tkIdentifierResolved:
    case tkString:
    case tkIdentifier:
    case tkKeyword_no:
    case tkKeyword_yes:
    case tkKeyword_nil:
    case tkNumber:
    case tkRawString:
    case tkLineComment:
        return 0;
    default:
        if (expr->prec) {
            int ret = 0;
            if (!expr->unary) ret += markTypesVisited(parser, expr->left);
            ret += markTypesVisited(parser, expr->right);
            if (ret) return ret;
        } else
            unreachable("unknown expr kind: %s at %d:%d\n",
                TokenKind_str[expr->kind], expr->line, expr->col);
    }
    if (!type) return 0;
    if (type->visited) {
        errorConstructorHasCycle(parser, type);
        eprintf("%s", "\e[;2m"); // Backtrace (innermost first):\n");
        return -1;
    }
    type->visited = true;
    return checkCycles(parser, type);
}

static void unmarkTypesVisited(ASTModule* mod) {
    // reset the cycle check flag on all types
    for (ASTType& type : mod->types) type->visited = false;
}
