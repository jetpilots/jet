
#pragma mark - PARSE EXPR
static ASTExpr* parseExpr() {

    static PtrArray rpn, ops, result;
    int prec_top = 0;
    ASTExpr* p = NULL;
    TokenKind revBrkt = tkUnknown;

    while (parser->token.kind != tkNullChar && parser->token.kind != tkNewline
        && parser->token.kind != tkLineComment) {

        if (parser->token.kind == tkOneSpace) Token_advance(&parser->token);
        if (parser->token.kind == tkIdentifier
            && memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser);

        ASTExpr* expr;
        if (Parser_matches(parser, tkParenOpen))
            expr = lparen;
        else if (Parser_matches(parser, tkParenClose))
            expr = rparen;
        else
            expr = ASTExpr_fromToken(&parser->token);

        int prec = expr->prec;
        bool rassoc = prec ? expr->rassoc : false;
        char lookAheadChar = Token_peekCharAfter(&parser->token);

        switch (expr->kind) {
        case tkIdentifier:
            if (memchr(parser->token.pos, '_', parser->token.matchlen))
                Parser_errorInvalidIdent(parser);
            expr->slen = parser->token.matchlen;
            switch (lookAheadChar) {

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
                && PtrArray_topAs(ASTExpr*, &ops)->kind == tkFunctionCall)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == ')') PtrArray_push(&rpn, NULL);

            if (lookAheadChar == '&') Token_advance(&parser->token);

            break;

        case tkArrayOpen:
            PtrArray_push(&ops, expr);
            if (!PtrArray_empty(&ops)
                && PtrArray_topAs(ASTExpr*, &ops)->kind == tkSubscript)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == ']') PtrArray_push(&rpn, NULL);

            break;

        case tkBraceOpen:
            PtrArray_push(&ops, expr);
            if (!PtrArray_empty(&ops)
                && PtrArray_topAs(ASTExpr*, &ops)->kind == tkObjectInit)
                PtrArray_push(&rpn, expr);
            if (lookAheadChar == '}') PtrArray_push(&rpn, NULL);

            break;

        case tkParenClose:
        case tkArrayClose:
        case tkBraceClose:

            revBrkt = TokenKind_reverseBracket(expr->kind);
            if (PtrArray_empty(&ops)) {

                Parser_errorParsingExpr(parser, expr);
                goto error;
            }

            else
                while (!PtrArray_empty(&ops)) {
                    p = PtrArray_pop(&ops);
                    if (p->kind == revBrkt) break;
                    PtrArray_push(&rpn, p);
                }

            if ((p && p->kind == tkArrayOpen))
                if ((PtrArray_empty(&ops)
                        || (PtrArray_top(&rpn)
                            && PtrArray_topAs(ASTExpr*, &ops)->kind
                                != tkSubscript))

                    || (PtrArray_empty(&rpn)
                        || (PtrArray_top(&rpn)
                            && PtrArray_topAs(ASTExpr*, &rpn)->kind
                                != tkOpColon)))

                    PtrArray_push(&rpn, p);

            if ((p && p->kind == tkBraceOpen)
                && (PtrArray_empty(&ops)
                    || (PtrArray_top(&rpn)
                        && PtrArray_topAs(ASTExpr*, &ops)->kind
                            != tkObjectInit)))

                PtrArray_push(&rpn, p);

            break;

        case tkKeyword_check:
            PtrArray_push(&ops, expr);
            break;
        case tkExclamation:
            if (PtrArray_empty(&rpn)
                || PtrArray_topAs(ASTExpr*, &rpn)->kind != tkIdentifier) {
                Parser_errorParsingExpr(parser, expr);
            }
            break;

        case tkKeyword_return:

            PtrArray_push(&ops, expr);
            if (lookAheadChar == '!' || lookAheadChar == '\n')
                PtrArray_push(&rpn, NULL);
            break;

        case tkPeriod:
            if (PtrArray_empty(&rpn) || !PtrArray_empty(&ops))

                PtrArray_push(&rpn, NULL);

        default:
            if (prec) {

                if (expr->kind == tkOpColon) {
                    if (PtrArray_empty(&rpn)
                        || (!PtrArray_top(&rpn) && !PtrArray_empty(&ops)
                            && PtrArray_topAs(ASTExpr*, &ops)->kind
                                != tkOpColon)
                        || (PtrArray_topAs(ASTExpr*, &rpn)->kind == tkOpColon
                            && !PtrArray_empty(&ops)
                            && (PtrArray_topAs(ASTExpr*, &ops)->kind
                                    == tkOpComma
                                || PtrArray_topAs(ASTExpr*, &ops)->kind
                                    == tkArrayOpen)))

                        PtrArray_push(&rpn, &expr_const_0);
                }
                while (!PtrArray_empty(&ops)) {
                    prec_top = PtrArray_topAs(ASTExpr*, &ops)->prec;
                    if (!prec_top) break;
                    if (prec > prec_top) break;
                    if (prec == prec_top && rassoc) break;
                    p = PtrArray_pop(&ops);

                    if (p->kind != tkOpComma && p->kind != tkOpSemiColon
                        && p->kind != tkFunctionCall && p->kind != tkSubscript
                        && PtrArray_topAs(ASTExpr*, &rpn)
                        && PtrArray_topAs(ASTExpr*, &rpn)->kind == tkOpComma) {
                        Parser_errorUnexpectedToken(
                            parser, "unsupported use of comma");

                        goto error;
                    }

                    if (!(p->prec || p->unary) && p->kind != tkFunctionCall
                        && p->kind != tkOpColon && p->kind != tkSubscript
                        && rpn.used < 2) {
                        Parser_errorUnexpectedToken(
                            parser, "need 2 operands to binary op");

                        goto error;
                    }

                    PtrArray_push(&rpn, p);
                }

                if (PtrArray_empty(&rpn)) {
                    Parser_errorUnexpectedToken(
                        parser, "operator with no operand(s)");

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
            && PtrArray_topAs(ASTExpr*, &rpn)
            && PtrArray_topAs(ASTExpr*, &rpn)->kind == tkOpComma) {
            Parser_errorUnexpectedExpr(parser, PtrArray_topAs(ASTExpr*, &rpn));
            goto error;
        }

        if (!(p->prec || p->unary)
            && (p->kind != tkFunctionCall && p->kind != tkSubscript)
            && rpn.used < 2) {
            Parser_errorParsingExpr(parser, p);
            goto error;
        }

        PtrArray_push(&rpn, p);
    }

    ASTExpr* arg;
    for (int i = 0; i < rpn.used; i++) {
        if (!(p = rpn.ref[i])) goto justpush;
        switch (p->kind) {
        case tkFunctionCall:
        case tkSubscript:
            if (result.used > 0) {
                arg = PtrArray_pop(&result);
                if (arg && p->kind == tkSubscript) {

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
        case tkLineComment:
            break;

        default:

            if (!p->prec) {
                Parser_errorParsingExpr(parser, p);
                goto error;
            }

            if (PtrArray_empty(&result)) {
                Parser_errorParsingExpr(parser, p);
                goto error;
            }

            p->right = PtrArray_pop(&result);

            if (!p->unary) {
                if (PtrArray_empty(&result)) {
                    Parser_errorParsingExpr(parser, p);
                    goto error;
                }
                p->left = PtrArray_pop(&result);
            }
        }
    justpush:
        PtrArray_push(&result, p);
    }
    if (!result.used) {
        Parser_errorUnexpectedToken(parser, "nothing parsed");
        goto error;
    } else if (result.used != 1) {
        if (PtrArray_topAs(ASTExpr*, &result)->kind != tkLineComment) {
            Parser_errorParsingExpr(parser, p);
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
        printf("      ops: ");
        for (int i = 0; i < ops.used; i++)
            printf("%s ", TokenKind_repr(((ASTExpr*)ops.ref[i])->kind, false));
        puts("");
    }

    if (rpn.used) {
        printf("      rpn: ");
        for (int i = 0; i < rpn.used; i++)
            if (!rpn.ref[i])
                printf("NUL ");
            else {
                ASTExpr* e = rpn.ref[i];
                printf("%.*s ", 32,
                    e->prec ? TokenKind_repr(e->kind, false) : e->string);
            }
        puts("");
    }

    if (result.used) {
        printf("      result: ");
        for (int i = 0; i < result.used; i++)
            if (!result.ref[i])
                printf("NUL ");
            else {
                ASTExpr* e = result.ref[i];
                printf("%.*s ", 32,
                    e->prec ? TokenKind_repr(e->kind, false) : e->string);
            }
        puts("");
    }

    if (p) {
        printf("      p: %.*s ", 32,
            p->prec ? TokenKind_repr(p->kind, false) : p->string);
        puts("");
    }

    ops.used = 0;
    rpn.used = 0;
    result.used = 0;
    return NULL;
}

#pragma mark - PARSE TYPESPEC
static ASTTypeSpec* parseTypeSpec() {
    parser->token.mergeArrayDims = true;

    ASTTypeSpec* typeSpec = NEW(ASTTypeSpec);
    typeSpec->line = parser->token.line;
    typeSpec->col = parser->token.col;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    typeSpec->name = parseIdent(parser);

    if (Parser_matches(parser, tkArrayDims)) {
        if (isalpha(*parser->token.pos)) {

        } else {
            for (int i = 0; i < parser->token.matchlen; i++)
                if (parser->token.pos[i] == ':') typeSpec->dims++;
            if (!typeSpec->dims) typeSpec->dims = 1;
            typeSpec->collectionType
                = typeSpec->dims == 1 ? CTYArray : CTYTensor;
        }
        Token_advance(&parser->token);
    }

    Parser_ignore(parser, tkUnits);

    assert(parser->token.kind != tkUnits);
    assert(parser->token.kind != tkArrayDims);

    parser->token.mergeArrayDims = false;
    return typeSpec;
}

#pragma mark - PARSE VAR
static ASTVar* parseVar() {
    ASTVar* var = NEW(ASTVar);
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

    int dims = 0;

    parser->token.mergeArrayDims = false;

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_matches(parser, tkIdentifier))

    {
        var->typeSpec = parseTypeSpec(parser);
    } else {
        var->typeSpec = NEW(ASTTypeSpec);
        var->typeSpec->line = parser->token.line;
        var->typeSpec->col = parser->token.col;
        var->typeSpec->name = "";
    }

    Parser_ignore(parser, tkOneSpace);
    if (Parser_ignore(parser, tkOpAssign)) var->init = parseExpr(parser);

    return var;
}

static List<ASTVar*> parseArgs() {
    List<ASTVar*>* args = NULL;
    Parser_consume(parser, tkParenOpen);
    if (Parser_ignore(parser, tkParenClose)) return args;

    ASTVar* arg;
    do {
        arg = parseVar(parser);
        arg->isArg = true;
        PtrList_append(&args, arg);
    } while (Parser_ignore(parser, tkOpComma));

    Parser_consume(parser, tkParenClose);
    return args;
}
static ASTScope* parseScope(ASTScope* parent, bool isTypeBody);

static ASTScope* parseScopeCases(ASTScope* parent) {

    ASTScope* scope = NEW(ASTScope);
    scope->parent = parent;
    List<ASTVar*>** stmts = &scope->stmts;
    ASTExpr* expr;

    while (parser->token.pos < parser->end) {
        switch (parser->token.kind) {

        case tkKeyword_case:

            expr = Parser_match(parser, tkKeyword_case);
            expr->left = parseExpr(parser);
            Token_advance(&parser->token);
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScope(parser, scope, false);

            stmts = PtrList_append(stmts, expr);

            break;

        case tkNewline:
        case tkOneSpace:
        case tkLineComment:
            Token_advance(&parser->token);

            break;

        case tkKeyword_end:
            goto exitloop;

        default:
            Parser_errorUnexpectedToken(parser, "expected 'case' or 'end'");
            Token_advance(&parser->token);
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE SCOPE
static ASTScope* parseScope(ASTScope* parent, bool isTypeBody) {
    ASTScope* scope = NEW(ASTScope);

    ASTVar *var = NULL, *orig = NULL;
    ASTExpr* expr = NULL;
    TokenKind tt = tkUnknown;
    ASTScope* forScope = NULL;

    scope->parent = parent;
    bool startedElse = false;
    bool startedCase = false;

    List<ASTVar*>** locals = &scope->locals;
    List<ASTVar*>** stmts = &scope->stmts;

    while (parser->token.kind != tkKeyword_end) {

        switch (parser->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkKeyword_var:
        case tkKeyword_let:
            var = parseVar(parser);
            if (!var)
                continue;
            else
                Token_advance(&parser->token);
            if ((orig = ASTScope_getVar(scope, var->name)))
                Parser_errorDuplicateVar(parser, var, orig);

            locals = PtrList_append(locals, var);

            expr = NEW(ASTExpr);
            expr->kind = tkVarAssign;
            expr->line = var->init ? var->init->line : parser->token.line;
            expr->col = var->init ? var->init->col : 1;
            expr->prec = TokenKind_getPrecedence(tkOpAssign);
            expr->var = var;

            if (var->init) resolveVars(parser, var->init, scope, false);

            stmts = PtrList_append(stmts, expr);
            break;

        case tkKeyword_case:

            goto exitloop;

        case tkKeyword_match:

            if (isTypeBody) Parser_errorInvalidTypeMember(parser);

            expr = Parser_match(parser, tkKeyword_match);
            expr->left = parseExpr(parser);
            Token_advance(&parser->token);
            if (expr->left) resolveVars(parser, expr->left, scope, false);

            expr->body = parseScopeCases(parser, scope);

            Parser_consume(parser, tkKeyword_end);
            Parser_ignore(parser, tkOneSpace);
            Parser_ignore(parser, tkKeyword_match);

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

            Token_advance(&parser->token);

            if (tt == tkKeyword_for) {

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

                    if ((orig = ASTScope_getVar(scope, fvar->name)))
                        Parser_errorDuplicateVar(parser, fvar, orig);
                }
                forScope = NEW(ASTScope);
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

            if (Parser_matches(parser, tkKeyword_else)
                || Parser_matches(parser, tkKeyword_elif)) {
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
        case tkOneSpace:
            Token_advance(&parser->token);
            break;

        case tkKeyword_function:
        case tkKeyword_type:
        case tkKeyword_enum:
        case tkKeyword_test:

            goto exitloop;

        case tkLineComment:
            if (parser->generateCommentExprs) {
                expr = ASTExpr_fromToken(&parser->token);
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
            Token_advance(&parser->token);
            resolveVars(parser, expr, scope, false);
            break;
        }
    }
exitloop:
    return scope;
}

static ASTScope* parseEnumBody(ASTScope* globScope) {
    ASTScope* scope = NEW(ASTScope);
    scope->parent = globScope;
    ASTExpr* expr = NULL;
    ASTVar* var = NULL;
    List<ASTVar*>** vars = &scope->locals;
    List<ASTVar*>** stmts = &scope->stmts;

    while (parser->token.kind != tkKeyword_end) {
        switch (parser->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(parser, tkUnknown);
            goto exitloop;

        case tkNewline:
        case tkOneSpace:
            Token_advance(&parser->token);
            break;

        case tkLineComment:
            if (parser->generateCommentExprs) {
                expr = ASTExpr_fromToken(&parser->token);
                PtrList_append(&scope->stmts, expr);
            }
            Token_advance(&parser->token);
            break;

        case tkIdentifier:

            expr = parseExpr(parser);

            if (!expr) break;
            if (expr->kind != tkIdentifier && expr->kind != tkOpAssign) {
                Parser_errorInvalidTypeMember(parser);
                unreachable("%s\n", TokenKind_str[expr->kind]);
                expr = NULL;
            }
            stmts = PtrList_append(stmts, expr);
            Token_advance(&parser->token);

            var = NEW(ASTVar);
            var->typeSpec = NEW(ASTTypeSpec);
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
static List<ASTVar*> parseParams() {
    Parser_consume(parser, tkOpLT);
    List<ASTVar*>* params;
    ASTVar* param;
    do {
        param = NEW(ASTVar);
        param->name = parseIdent(parser);
        if (Parser_ignore(parser, tkKeyword_as))
            param->typeSpec = parseTypeSpec(parser);
        if (Parser_ignore(parser, tkOpAssign)) param->init = parseExpr(parser);
        PtrList_append(&params, param);
    } while (Parser_ignore(parser, tkOpComma));
    Parser_consume(parser, tkOpGT);
    return params;
}

#pragma mark - PARSE FUNC / STMT-FUNC
static ASTFunc* parseFunc(ASTScope* globScope, bool shouldParseBody) {
    Parser_consume(parser, tkKeyword_function);
    Parser_consume(parser, tkOneSpace);
    ASTFunc* func = NEW(ASTFunc);

    func->line = parser->token.line;

    func->nameLen = parser->token.matchlen;
    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parseIdent(parser);
    func->isDeclare = !shouldParseBody;

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_ignore(parser, tkKeyword_as)) {
        Parser_consume(parser, tkOneSpace);
        func->returnSpec = parseTypeSpec(parser);
    }

    if (shouldParseBody) {
        Parser_consume(parser, tkNewline);

        ASTScope* funcScope = NEW(ASTScope);
        funcScope->parent = globScope;
        funcScope->locals = func->args;
        func->body = parseScope(parser, funcScope, false);

        Parser_consume(parser, tkKeyword_end);
        Parser_ignore(parser, tkOneSpace);
        Parser_ignore(parser, tkKeyword_function);
    }

    return func;
}

static ASTFunc* parseStmtFunc(ASTScope* globScope) {
    ASTFunc* func = NEW(ASTFunc);

    func->line = parser->token.line;
    func->isStmt = true;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    func->name = parseIdent(parser);

    func->args = parseArgs(parser);
    func->argCount = PtrList_count(func->args);
    Parser_ignore(parser, tkOneSpace);

    func->returnSpec = NEW(ASTTypeSpec);
    func->returnSpec->line = parser->token.line;
    func->returnSpec->col = parser->token.col;
    func->returnSpec->name = "";

    ASTExpr* ret = exprFromCurrentToken(parser);

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

    Parser_consume(parser, tkNewline);
    resolveVars(parser, ret->right, funcScope, false);

    return func;
}

#pragma mark - PARSE TEST
static ASTTest* parseTest(ASTScope* globScope) {
    Parser_consume(parser, tkKeyword_test);
    Parser_consume(parser, tkOneSpace);
    ASTTest* test = NEW(ASTTest);

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
static ASTUnits* parseUnits() { return NULL; }

#pragma mark - PARSE TYPE
static ASTType* parseType(ASTScope* globScope, bool shouldParseBody) {
    ASTType* type = NEW(ASTType);

    Parser_consume(parser, tkKeyword_type);
    Parser_consume(parser, tkOneSpace);

    type->line = parser->token.line;
    type->col = parser->token.col;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
        Parser_errorInvalidIdent(parser);
    type->name = parseIdent(parser);

    if (Parser_ignore(parser, tkOneSpace)
        && Parser_ignore(parser, tkKeyword_extends)) {
        Parser_consume(parser, tkOneSpace);
        type->super = parseTypeSpec(parser);
    }
    Parser_ignore(parser, tkNewline);

    type->body = NULL;
    if (TypeType_byName(type->name) != TYUnresolved) {
        Parser_errorDuplicateType(parser, type, NULL);
        return type;
    }

    type->body = parseScope(parser, globScope, true);

    Parser_consume(parser, tkKeyword_end);
    Parser_ignore(parser, tkOneSpace);
    Parser_ignore(parser, tkKeyword_type);

    return type;
}

static ASTType* parseEnum(ASTScope* globScope) {
    ASTType* en = NEW(ASTType);

    Parser_consume(parser, tkKeyword_enum);
    Parser_consume(parser, tkOneSpace);

    en->line = parser->token.line;
    en->col = parser->token.col;
    en->isEnum = true;

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);
    if (*parser->token.pos < 'A' || *parser->token.pos > 'Z')
        Parser_errorInvalidIdent(parser);
    en->name = parseIdent(parser);

    Parser_consume(parser, tkNewline);

    if (TypeType_byName(en->name) != TYUnresolved) {

        Parser_errorDuplicateEnum(parser, en, NULL);
        return en;
    }

    en->body = parseEnumBody(parser, globScope);

    Parser_consume(parser, tkKeyword_end);
    Parser_ignore(parser, tkOneSpace);
    Parser_ignore(parser, tkKeyword_enum);

    return en;
}

static ASTImport* parseImport(ASTModule* ownerMod) {
    ASTImport* import = NEW(ASTImport);
    char* tmp;
    Parser_consume(parser, tkKeyword_import);
    Parser_consume(parser, tkOneSpace);

    if (memchr(parser->token.pos, '_', parser->token.matchlen))
        Parser_errorInvalidIdent(parser);

    assert(Parser_matches(parser, tkIdentifier));
    import->name = parser->token.pos;

    char* cend = import->name;
    while (*cend && (isalnum(*cend) || *cend == '.')) cend++;

    parser->token.pos = cend;
    Token_detect(&parser->token);

    Parser_ignore(parser, tkOneSpace);
    if (Parser_ignore(parser, tkKeyword_as)) {

        Parser_ignore(parser, tkOneSpace);

        assert(Parser_matches(parser, tkIdentifier));
        if (memchr(parser->token.pos, '_', parser->token.matchlen))
            Parser_errorInvalidIdent(parser);

        import->alias = parseIdent(parser);

    } else {
        import->alias
            = CString_base(import->name, '.', parser->token.pos - import->name);
    }

    char endchar = *parser->token.pos;
    *parser->token.pos = 0;
    if (ASTModule_getImportByAlias(ownerMod, import->alias)
        || ASTModule_getFunc(ownerMod, import->alias)
        || ASTModule_getVar(ownerMod, import->alias)) {
        unreachable("import name already used: %s", import->alias);
        import = NULL;
    }
    *parser->token.pos = endchar;

    return import;
}

void analyseModule(Parser* parser, ASTModule* mod);

static ASTFunc* ASTType_makeDefaultCtor(ASTType* type) {
    ASTFunc* ctor = NEW(ASTFunc);
    ctor->line = type->line;
    ctor->isDefCtor = true;

    ctor->returnsNewObjectAlways = true;
    ctor->name = type->name;
    char buf[128];
    int l = snprintf(buf, 128, "%s_new_", type->name);
    ctor->selector = pstrndup(buf, l);
    ASTTypeSpec* tspec = ASTTypeSpec_new(TYObject, CTYNone);
    tspec->type = type;
    ctor->returnSpec = tspec;
    return ctor;
}

static ASTModule* Parser_lookupModule(
    List<ASTModule&>& existingModules, char* name) {
    foreach (ASTModule*, mod, existingModules) {

        if (name ^= mod->name) return mod;
    }
    return NULL;
}

static ASTModule* parseModule(
    List<ASTModule&>& existingModules, ASTModule* importer) {
    ASTModule* root = NEW(ASTModule);
    root->name = parser->moduleName;

    PtrList_append(existingModulesPtr, root);
    eprintf("Parsing %s\n", root->name);

    const bool onlyPrintTokens = false;
    Token_advance(&parser->token);

    ASTImport* import = NULL;

    List<ASTFunc*>** funcs = &root->funcs;
    List<ASTImport*>** imports = &root->imports;
    List<ASTType*>** types = &root->types;
    List<ASTTest*>** tests = &root->tests;
    List<ASTEnum*>** enums = &root->enums;
    ASTScope* gscope = root->scope;
    List<ASTVar*>** gvars = &gscope->locals;
    List<ASTExpr*>** gexprs = &gscope->stmts;

    while (parser->token.kind != tkNullChar) {
        if (onlyPrintTokens) {
            printf("%s %2d %3d %3d %-20s\t%.*s\n", parser->moduleName,
                parser->token.line, parser->token.col, parser->token.matchlen,
                TokenKind_str[parser->token.kind],
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
                ASTFunc* func = parseFunc(parser, gscope, false);
                func->isDeclare = true;
                funcs = PtrList_append(funcs, func);
            }
            if (parser->token.kind == tkKeyword_type) {
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

            ASTFunc* ctor = ASTType_makeDefaultCtor(type);
            funcs = PtrList_append(funcs, ctor);

            char* defFuncs[] = { "json", "print", "describe" };

            for (int i = 0; i < countof(defFuncs); i++) {
                ASTFunc* func
                    = ASTFunc_createDeclWithArg(defFuncs[i], NULL, type->name);
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
                    = Parser_lookupModule(*existingModulesPtr, import->name);
                if (!import->module) {
                    eprintf("%s needs %s, parsing\n", root->name, import->name);
                    size_t len = strlen(import->name) + 5;
                    char* filename = malloc(len);
                    filename[len - 1] = 0;
                    strcpy(filename, import->name);
                    CString_tr_ip(filename, '.', '/', len - 5);
                    strcpy(filename + len - 5, ".jet");

                    Parser* subParser
                        = Parser_fromFile(filename, true, parser->mode);

                    if (subParser)
                        import->module
                            = parseModule(subParser, existingModulesPtr, root);
                }
            }
            break;

        case tkKeyword_test:
            tests = PtrList_append(tests, parseTest(parser, gscope));

            break;
        case tkKeyword_var:
        case tkKeyword_let:

        {
            ASTVar *var = parseVar(parser), *orig;
            if (!var) {
                Token_advance(&parser->token);
                continue;
            }
            if ((orig = ASTScope_getVar(gscope, var->name)))
                Parser_errorDuplicateVar(parser, var, orig);
            if (ASTModule_getImportByAlias(root, var->name)
                || ASTModule_getFunc(root, var->name))
                unreachable(
                    "name already used by func or import: %s", var->name);
            if (var->init) resolveVars(parser, var->init, gscope, false);
            var->isLet = true;
            var->isVar = false;
            gvars = PtrList_append(gvars, var);

            ASTExpr* expr = NEW(ASTExpr);
            expr->kind = tkVarAssign;
            expr->line = var->init ? var->init->line : parser->token.line;
            expr->col = var->init ? var->init->col : 1;
            expr->prec = TokenKind_getPrecedence(tkOpAssign);
            expr->var = var;

            if (var->init) resolveVars(parser, var->init, gscope, false);

            gexprs = PtrList_append(gexprs, expr);
        }

        break;
        case tkNewline:
            *(parser->token.pos) = 0;

        case tkLineComment:

        case tkOneSpace:
            Token_advance(&parser->token);
            break;
        case tkIdentifier:
            if (Token_peekCharAfter(&parser->token) == '(') {
                funcs = PtrList_append(funcs, parseStmtFunc(parser, gscope));

                break;
            }
        default:
            Parser_errorUnexpectedToken(parser, "expected a keyword here");
            while (parser->token.kind != tkNewline
                && parser->token.kind != tkLineComment
                && parser->token.kind != tkNullChar)
                Token_advance(&parser->token);
        }
    }

    char* defTypes[] = { "String", "Number", "Boolean" };
    char* defFuncs[] = { "json", "print", "describe" };
    char* retTypes[countof(defFuncs)] = {};

    for (int j = 0; j < countof(defTypes); j++)
        for (int i = 0; i < countof(defFuncs); i++) {
            ASTFunc* func = ASTFunc_createDeclWithArg(
                defFuncs[i], retTypes[i], defTypes[j]);
            func->intrinsic = true;

            funcs = PtrList_append(funcs, func);
        }

    analyseModule(parser, root);
    if (importer) PtrList_shift(&importer->importedBy, root);
    return root;
}
