
#pragma mark - PARSE EXPR
static ASTExpr* parseExpr(Parser* self)
{
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

    static jet_PtrArray rpn, ops, result;
    int prec_top = 0;
    ASTExpr* p = NULL;
    TokenKind revBrkt = tkUnknown;

    // ******* STEP 1 CONVERT TOKENS INTO RPN

    while (self->token.kind != tkNullChar //
        and self->token.kind != tkNewline
        and self->token.kind != tkLineComment) { // build RPN

        // you have to ensure that ops have a space around them, etc.
        // so don't just skip the one spaces like you do now.
        if (self->token.kind == tkOneSpace) Token_advance(&self->token);
        if (self->token.kind == tkIdentifier
            and memchr(self->token.pos, '_', self->token.matchlen))
            Parser_errorInvalidIdent(self); // but continue parsing

        ASTExpr* expr;
        if (matches(self, tkParenOpen))
            expr = lparen;
        else if (matches(self, tkParenClose))
            expr = rparen;
        else
            expr = ASTExpr_fromToken(&self->token); // dont advance yet

        int prec = expr->prec;
        bool rassoc = prec ? expr->rassoc : false;
        char lookAheadChar = Token_peekCharAfter(&self->token);

        switch (expr->kind) {
        case tkIdentifier:
            switch (lookAheadChar) {
                // TODO: need a general lookahead that skips whitespace.
            case '(':
                expr->kind = tkFunctionCall;
                expr->prec = 60;
                jet_PtrArray_push(&ops, expr);
                break;
            case '[':
                expr->kind = tkSubscript;
                expr->prec = 60;
                jet_PtrArray_push(&ops, expr);
                break;
            case ' ':
                if (self->token.pos[2] != '{') goto defaultCase;
                // otherwise fall through
            case '{':
                expr->kind = tkObjectInit;
                expr->prec = 60;
                jet_PtrArray_push(&ops, expr);
                break;

            default:
            defaultCase:
                jet_PtrArray_push(&rpn, expr);
                break;
            }
            break;

        case tkParenOpen:
            jet_PtrArray_push(&ops, expr);
            if (not jet_PtrArray_empty(&ops)
                and jet_PtrArray_topAs(ASTExpr*, &ops)->kind == tkFunctionCall)
                jet_PtrArray_push(&rpn, expr);
            if (lookAheadChar == ')') jet_PtrArray_push(&rpn, NULL);
            // for empty func() push null for no args
            break;

        case tkArrayOpen:
            jet_PtrArray_push(&ops, expr);
            if (not jet_PtrArray_empty(&ops)
                and jet_PtrArray_topAs(ASTExpr*, &ops)->kind == tkSubscript)
                jet_PtrArray_push(&rpn, expr);
            if (lookAheadChar == ']') jet_PtrArray_push(&rpn, NULL);
            // for empty arr[] push null for no args
            break;

        case tkBraceOpen:
            jet_PtrArray_push(&ops, expr);
            if (not jet_PtrArray_empty(&ops)
                and jet_PtrArray_topAs(ASTExpr*, &ops)->kind == tkObjectInit)
                jet_PtrArray_push(&rpn, expr);
            if (lookAheadChar == '}') jet_PtrArray_push(&rpn, NULL);
            // for empty Obj {} push null for no args
            break;

        case tkParenClose:
        case tkArrayClose:
        case tkBraceClose:

            revBrkt = TokenKind_reverseBracket(expr->kind);
            if (jet_PtrArray_empty(&ops)) {
                // need atleast the opening bracket of the current kind
                Parser_errorParsingExpr(self, expr);
                goto error;
            }

            else
                while (not jet_PtrArray_empty(&ops)) {
                    p = jet_PtrArray_pop(&ops);
                    if (p->kind == revBrkt) break;
                    jet_PtrArray_push(&rpn, p);
                }
            // we'll push the tkArrayOpen as well to indicate a list
            // literal or comprehension
            // tkArrayOpen is a unary op.
            if ((p and p->kind == tkArrayOpen))
                if ((jet_PtrArray_empty(&ops)
                        or (jet_PtrArray_top(&rpn)
                            and jet_PtrArray_topAs(ASTExpr*, &ops)->kind
                                != tkSubscript))
                    // don't do this if its part of a subscript
                    or (jet_PtrArray_empty(&rpn)
                        or (jet_PtrArray_top(&rpn)
                            and jet_PtrArray_topAs(ASTExpr*, &rpn)->kind
                                != tkOpColon)))
                    // or aa range. range exprs are handled separately. by
                    // themselves they don't need a surrounding [], but for
                    // grouping like 2+[8:66] they do.
                    jet_PtrArray_push(&rpn, p);

            // push tkBraceOpen to indicate a dict literal (another unary op).
            if ((p and p->kind == tkBraceOpen)
                and (jet_PtrArray_empty(&ops)
                    or (jet_PtrArray_top(&rpn)
                        and jet_PtrArray_topAs(ASTExpr*, &ops)->kind
                            != tkObjectInit)))
                // again, not if it is an object init
                jet_PtrArray_push(&rpn, p);
            // Object { member1 = 3, member3 = "train" }
            // ^^ this is an object init, not a dict
            // { "bing" = 34, "whang" = 33 }
            // ^^ this is a dict

            break;
        case tkKeyword_check:
            jet_PtrArray_push(&ops, expr);
            break;
        case tkKeyword_return:
            // for empty return, push a NULL if there is no expr coming.
            jet_PtrArray_push(&ops, expr);
            if (lookAheadChar == '!' or lookAheadChar == '\n')
                jet_PtrArray_push(&rpn, NULL);
            break;
        default:
            if (prec) {

                if (expr->kind == tkOpColon) {
                    if (jet_PtrArray_empty(&rpn)
                        or (!jet_PtrArray_top(&rpn)
                            and !jet_PtrArray_empty(&ops)
                            and jet_PtrArray_topAs(ASTExpr*, &ops)->kind
                                != tkOpColon)
                        or (jet_PtrArray_topAs(ASTExpr*, &rpn)->kind
                                == tkOpColon
                            and !jet_PtrArray_empty(&ops)
                            and (jet_PtrArray_topAs(ASTExpr*, &ops)->kind
                                    == tkOpComma
                                or jet_PtrArray_topAs(ASTExpr*, &ops)->kind
                                    == tkArrayOpen)))
                        // TODO: better way to parse :, 1:, :-1, etc.
                        // while passing tokens to RPN, if you see a :
                        // with nothing on the RPN or comma or [, push a
                        // NULL. while unwinding the op stack, if you
                        // pop a : and see a NULL or comma on the rpn,
                        // push another NULL.
                        jet_PtrArray_push(&rpn, &expr_const_0);
                    // indicates empty operand
                }
                while (not jet_PtrArray_empty(&ops)) {
                    prec_top = jet_PtrArray_topAs(ASTExpr*, &ops)->prec;
                    if (not prec_top) break; // left parenthesis
                    if (prec > prec_top) break;
                    if (prec == prec_top and rassoc) break;
                    p = jet_PtrArray_pop(&ops);

                    if (p->kind != tkOpComma and p->kind != tkOpSemiColon
                        and p->kind != tkFunctionCall and p->kind != tkSubscript
                        and jet_PtrArray_topAs(ASTExpr*, &rpn)
                        and jet_PtrArray_topAs(ASTExpr*, &rpn)->kind
                            == tkOpComma) {
                        Parser_errorUnexpectedToken(self);
                        goto error;
                    }

                    if (not(p->prec or p->unary) and p->kind != tkFunctionCall
                        and p->kind != tkOpColon and p->kind != tkSubscript
                        and rpn.used < 2) {
                        Parser_errorUnexpectedToken(self);
                        goto error;
                    }

                    jet_PtrArray_push(&rpn, p);
                }

                if (jet_PtrArray_empty(&rpn)) {
                    Parser_errorUnexpectedToken(self);
                    goto error;
                }
                if (expr->kind == tkOpColon
                    and (lookAheadChar == ',' or lookAheadChar == ':'
                        or lookAheadChar == ']' or lookAheadChar == ')'))
                    jet_PtrArray_push(&rpn, &expr_const_0);

                jet_PtrArray_push(&ops, expr);
            } else {
                jet_PtrArray_push(&rpn, expr);
            }
        }
        Token_advance(&self->token);
        if (self->token.kind == tkOneSpace) Token_advance(&self->token);
    }
exitloop:

    while (not jet_PtrArray_empty(&ops)) {
        p = jet_PtrArray_pop(&ops);

        if (p->kind != tkOpComma and p->kind != tkFunctionCall
            and p->kind != tkSubscript and p->kind != tkArrayOpen
            and jet_PtrArray_topAs(ASTExpr*, &rpn)
            and jet_PtrArray_topAs(ASTExpr*, &rpn)->kind == tkOpComma) {
            Parser_errorUnexpectedExpr(
                self, jet_PtrArray_topAs(ASTExpr*, &rpn));
            goto error;
        }

        if (not(p->prec or p->unary)
            and (p->kind != tkFunctionCall and p->kind != tkSubscript)
            and rpn.used < 2) {
            Parser_errorParsingExpr(self, p);
            goto error;
            // TODO: even if you have more than two, neither of the top
            // two should be a comma
        }

        jet_PtrArray_push(&rpn, p);
    }

    // *** STEP 2 CONVERT RPN INTO EXPR TREE

    ASTExpr* arg;
    for (int i = 0; i < rpn.used; i++) {
        if (not(p = rpn.ref[i])) goto justpush;
        switch (p->kind) {
        case tkFunctionCall:
        case tkSubscript:
            if (result.used > 0) {
                arg = jet_PtrArray_pop(&result);
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
        case tkParenOpen:
        case tkLineComment:
            break;

        default:
            // everything else is a nonterminal, needs left/right
            if (not p->prec) {
                Parser_errorParsingExpr(self, p);
                goto error;
            }

            if (jet_PtrArray_empty(&result)) {
                Parser_errorParsingExpr(self, p);
                goto error;
            }

            p->right = jet_PtrArray_pop(&result);

            if (not p->unary) {
                if (jet_PtrArray_empty(&result)) {
                    Parser_errorParsingExpr(self, p);
                    goto error;
                }
                p->left = jet_PtrArray_pop(&result);
            }
        }
    justpush:
        jet_PtrArray_push(&result, p);
    }
    if (!result.used) {
        Parser_errorUnexpectedToken(self); //    (self, p);
        goto error;
    } else if (result.used != 1) {
        if (jet_PtrArray_topAs(ASTExpr*, &result)->kind != tkLineComment) {
            Parser_errorParsingExpr(self, p);
            goto error;
        }
    }

    ops.used = 0;
    rpn.used = 0;
    result.used = 0;
    return result.ref[0];

error:

    while (self->token.pos < self->end
        and (self->token.kind != tkNewline and self->token.kind != tkLineComment
            and self->token.kind != tkNullChar))
        Token_advance(&self->token);

    if (ops.used) {
        printf("      ops: ");
        for (int i = 0; i < ops.used; i++)
            printf("%s ", TokenKind_repr(((ASTExpr*)ops.ref[i])->kind, false));
        puts("");
    }

    if (rpn.used) {
        printf("      rpn: ");
        for (int i = 0; i < rpn.used; i++)
            if (not rpn.ref[i])
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
            if (not result.ref[i])
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

    ops.used = 0; // "reset" stacks
    rpn.used = 0;
    result.used = 0;
    return NULL;
}

#pragma mark - PARSE TYPESPEC
static ASTTypeSpec* parseTypeSpec(Parser* self)
{
    self->token.mergeArrayDims = true;

    ASTTypeSpec* typeSpec = jet_new(ASTTypeSpec);
    typeSpec->line = self->token.line;
    typeSpec->col = self->token.col;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);

    typeSpec->name = parseIdent(self);

    if (matches(self, tkArrayDims)) {
        for (int i = 0; i < self->token.matchlen; i++)
            if (self->token.pos[i] == ':') typeSpec->dims++;
        if (not typeSpec->dims) typeSpec->dims = 1;
        Token_advance(&self->token);
    }

    Parser_ignore(self, tkUnits);

    assert(self->token.kind != tkUnits);
    assert(self->token.kind != tkArrayDims);

    self->token.mergeArrayDims = false;
    return typeSpec;
}

#pragma mark - PARSE VAR
static ASTVar* parseVar(Parser* self)
{
    ASTVar* var = jet_new(ASTVar);
    var->isVar = (self->token.kind == tkKeyword_var);
    var->isLet = (self->token.kind == tkKeyword_let);

    if (var->isVar) discard(self, tkKeyword_var);
    if (var->isLet) discard(self, tkKeyword_let);
    if (var->isVar or var->isLet) discard(self, tkOneSpace);

    var->line = self->token.line;
    var->col = self->token.col;

    self->token.mergeArrayDims = true;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);
    if (*self->token.pos < 'a' or *self->token.pos > 'z')
        Parser_errorInvalidIdent(self);
    var->name = parseIdent(self);

    int dims = 0;
    if (matches(self, tkArrayDims)) {
        for (int i = 0; i < self->token.matchlen; i++)
            if (self->token.pos[i] == ':') dims++;
        if (not dims) dims = 1;
        Token_advance(&self->token);
    }
    self->token.mergeArrayDims = false;

    if (Parser_ignore(self, tkOneSpace) and Parser_ignore(self, tkKeyword_as)) {
        discard(self, tkOneSpace);
        var->typeSpec = parseTypeSpec(self);
    } else {
        var->typeSpec = jet_new(ASTTypeSpec);
        var->typeSpec->line = self->token.line;
        var->typeSpec->col = self->token.col;
        var->typeSpec->name = "";
    }
    var->typeSpec->dims = dims;

    Parser_ignore(self, tkOneSpace);
    if (Parser_ignore(self, tkOpAssign)) //
        var->init = parseExpr(self);

    return var;
}

static List(ASTVar) * parseArgs(Parser* self)
{
    List(ASTVar)* args = NULL;
    discard(self, tkParenOpen);
    if (Parser_ignore(self, tkParenClose)) return args;

    ASTVar* arg;
    do {
        arg = parseVar(self);
        jet_PtrList_append(&args, arg);
    } while (Parser_ignore(self, tkOpComma));

    discard(self, tkParenClose);
    return args;
}

#pragma mark - PARSE SCOPE
static ASTScope* parseScope(Parser* self, ASTScope* parent, bool isTypeBody)
{
    ASTScope* scope = jet_new(ASTScope);

    ASTVar *var = NULL, *orig = NULL;
    ASTExpr* expr = NULL;
    TokenKind tt = tkUnknown;
    ASTScope* forScope = NULL;

    scope->parent = parent;
    bool startedElse = false;

    List(ASTVar)** locals = &scope->locals;
    List(ASTVar)** stmts = &scope->stmts;

    while (self->token.kind != tkKeyword_end) {

        switch (self->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(self, tkUnknown);
            goto exitloop;

        case tkKeyword_var:
        case tkKeyword_let:
            var = parseVar(self);
            if (not var)
                continue;
            else
                Token_advance(&self->token);
            if ((orig = ASTScope_getVar(scope, var->name)))
                Parser_errorDuplicateVar(self, var, orig);
            // TODO: why only idents and binops for resolveVars??

            // resolveType(var->typeSpec, scope);
            // resolve BEFORE it is added to the list! in
            // `var x = x + 1` x should not resolve
            // if var->typeSpec is NULL then set the type
            // if it isn't NULL then check the types match
            locals = jet_PtrList_append(locals, var);
            // TODO: validation should raise issue if var->init is
            // missing
            expr = jet_new(ASTExpr);
            expr->kind = tkVarAssign;
            expr->line = var->init ? var->init->line : self->token.line;
            expr->col = var->init ? var->init->col : 1;
            expr->prec = TokenKind_getPrecedence(tkOpAssign);
            expr->var = var;

            // and (var->init->prec or var->init->kind == tkIdentifier))
            // TODO: you actually need to send the jet_PtrList item which is
            // generated in the next line as the topExpr, not the expr itself
            if (var->init) resolveVars(self, var->init, scope, false);

            // TODO: KEEP THE LAST LISTITEM AND APPEND TO THAT!!
            stmts = jet_PtrList_append(stmts, expr);
            break;

        case tkKeyword_else:
        case tkKeyword_elif:
            if (not startedElse) goto exitloop;

        case tkKeyword_if:
        case tkKeyword_for:
        case tkKeyword_while:
            if (isTypeBody) Parser_errorInvalidTypeMember(self);
            tt = self->token.kind;
            expr = match(self, tt);
            expr->left = tt != tkKeyword_else ? parseExpr(self) : NULL;

            // TODO: for must parse its expr as a VarDecl, because it can have
            // 'as Type' etc. Now you parse an assignment Expr and
            // hack an ASTVar out of it.
            if (tt == tkKeyword_for) {
                // TODO: new Parser_error
                if (expr->left->kind != tkOpAssign)
                    eprintf("Invalid for-loop condition: %s\n",
                        TokenKind_repr(expr->left->kind, false));
                resolveVars(self, expr->left->right, scope, false);

                var = jet_new(ASTVar);
                var->name = expr->left->left->string;
                var->line = expr->left->line;
                var->col = expr->left->left->col;
                var->isVar = true;
                var->init = expr->left->right;
                var->typeSpec = jet_new(ASTTypeSpec);
                var->typeSpec->typeType = TYReal64;

                if ((orig = ASTScope_getVar(scope, var->name)))
                    Parser_errorDuplicateVar(self, var, orig);

                forScope = jet_new(ASTScope);
                jet_PtrList_shift(&forScope->locals, var);
                forScope->parent = scope;

                // scope = forScope; // so that when parseScope is called for
                // the child scope, it receives the for variable's scope as
                // parent

            } else if (expr->left) {
                resolveVars(self, expr->left, scope, false);
            } // TODO: `for` necessarily introduces a counter variable, so
            // check if that var name doesn't already exist in scope.
            // Also assert that the cond of a for expr has kind
            // tkOpAssign.
            // insert a temp scope holding the var that for declares, then
            // later move that var to the parsed scope
            if (tt == tkKeyword_for) {
                // TODO: here it is too late to add the variable,
                // because parseScope will call resolveVars.
                // var = jet_new(ASTVar);
                // var->name = expr->left->left->string;
                // var->init = expr->left->right;
                // var->typeSpec = jet_new(ASTTypeSpec);
                // var->typeSpec->typeType = TYUInt32;
                // jet_PtrList_append(&expr->body->locals, var);
                expr->body = parseScope(self, forScope, false);

            } else {
                expr->body = parseScope(self, scope, false);
            }

            if (matches(self, tkKeyword_else)
                or matches(self, tkKeyword_elif)) {
                startedElse = true;
            } else {
                discard(self, tkKeyword_end);
                discard(self, tkOneSpace);
                discard(self,
                    tt == tkKeyword_else or tt == tkKeyword_elif ? tkKeyword_if
                                                                 : tt);
            }
            stmts = jet_PtrList_append(stmts, expr);
            break;

        case tkNewline:
        case tkOneSpace:
            Token_advance(&self->token);
            break;

        case tkLineComment:
            if (self->generateCommentExprs) {
                expr = ASTExpr_fromToken(&self->token);
                stmts = jet_PtrList_append(stmts, expr);
            }
            Token_advance(&self->token);
            break;

        default:
            expr = parseExpr(self);
            if (expr and isTypeBody) {
                Parser_errorInvalidTypeMember(self);
                expr = NULL;
            }
            if (not expr) break;
            stmts = jet_PtrList_append(stmts, expr);
            Token_advance(&self->token); // eat the newline
            resolveVars(self, expr, scope, false);
            break;
        }
    }
exitloop:
    return scope;
}

static ASTScope* parseEnumBody(Parser* self)
{
    ASTScope* scope = jet_new(ASTScope);
    ASTExpr* expr = NULL;
    while (self->token.kind != tkKeyword_end) {
        switch (self->token.kind) {

        case tkNullChar:
            Parser_errorExpectedToken(self, tkUnknown);
            goto exitloop;

        case tkNewline:
        case tkOneSpace:
            Token_advance(&self->token);
            break;

        case tkLineComment:
            if (self->generateCommentExprs) {
                expr = ASTExpr_fromToken(&self->token);
                jet_PtrList_append(&scope->stmts, expr);
            }
            Token_advance(&self->token);
            break;

        default:
            expr = parseExpr(self);
            if (expr->kind != tkIdentifier and expr->kind != tkOpAssign) {
                Parser_errorInvalidTypeMember(self);
                unreachable("%s\n", TokenKind_str[expr->kind]);
                expr = NULL;
            }
            if (not expr) break;
            jet_PtrList_append(&scope->stmts, expr);
            Token_advance(&self->token); // eat the newline
            if (expr->kind == tkOpAssign)
                resolveVars(self, expr->right, scope, false);
            break;
        }
    }
exitloop:
    return scope;
}

#pragma mark - PARSE PARAM
static List(ASTVar) * parseParams(Parser* self)
{
    discard(self, tkOpLT);
    List(ASTVar) * params;
    ASTVar* param;
    do {
        param = jet_new(ASTVar);
        param->name = parseIdent(self);
        if (Parser_ignore(self, tkKeyword_as))
            param->typeSpec = parseTypeSpec(self);
        if (Parser_ignore(self, tkOpAssign)) param->init = parseExpr(self);
        jet_PtrList_append(&params, param);
    } while (Parser_ignore(self, tkOpComma));
    discard(self, tkOpGT);
    return params;
}

#pragma mark - PARSE FUNC / STMT-FUNC
static ASTFunc* parseFunc(Parser* self, bool shouldParseBody)
{
    discard(self, tkKeyword_function);
    discard(self, tkOneSpace);
    ASTFunc* func = jet_new(ASTFunc);

    func->line = self->token.line;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);
    func->name = parseIdent(self);

    func->isDeclare = not shouldParseBody;

    func->args = parseArgs(self);
    func->argCount = jet_PtrList_count(func->args);

    if (Parser_ignore(self, tkOneSpace) and Parser_ignore(self, tkKeyword_as)) {
        discard(self, tkOneSpace);
        func->returnSpec = parseTypeSpec(self);
    }

    if (shouldParseBody) {
        discard(self, tkNewline);

        ASTScope* funcScope = jet_new(ASTScope);
        funcScope->locals = func->args;
        func->body = parseScope(self, funcScope, false);

        discard(self, tkKeyword_end);
        discard(self, tkOneSpace);
        discard(self, tkKeyword_function);
    }

    return func;
}

static ASTFunc* parseStmtFunc(Parser* self)
{
    ASTFunc* func = jet_new(ASTFunc);

    func->line = self->token.line;
    func->isStmt = true;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);
    func->name = parseIdent(self);

    func->args = parseArgs(self);
    func->argCount = jet_PtrList_count(func->args);
    Parser_ignore(self, tkOneSpace);

    func->returnSpec = jet_new(ASTTypeSpec);
    func->returnSpec->line = self->token.line;
    func->returnSpec->col = self->token.col;
    func->returnSpec->name = "";

    ASTExpr* ret = exprFromCurrentToken(self);

    // if you have toplevel code (eg func call) it tends to reach here
    if (ret->kind != tkColEq) return NULL;

    ret->kind = tkKeyword_return;
    ret->unary = true;

    ret->right = parseExpr(self);
    ASTScope* scope = jet_new(ASTScope);
    jet_PtrList_append(&scope->stmts, ret);

    ASTScope* funcScope = jet_new(ASTScope);
    funcScope->locals = func->args;
    scope->parent = funcScope;
    func->body = scope;

    resolveVars(self, ret->right, funcScope, false);

    return func;
}

#pragma mark - PARSE TEST
static ASTTest* parseTest(Parser* self)
{
    discard(self, tkKeyword_test);
    discard(self, tkOneSpace);
    ASTTest* test = jet_new(ASTTest);

    test->line = self->token.line;

    if (self->token.kind != tkString and self->token.kind != tkRawString)
        Parser_errorInvalidTestName(self);
    test->name = self->token.pos + 1;
    Token_advance(&self->token);

    discard(self, tkNewline);

    test->body = parseScope(self, NULL, false);

    discard(self, tkKeyword_end);
    discard(self, tkOneSpace);
    discard(self, tkKeyword_test);

    return test;
}

#pragma mark - PARSE UNITS
static ASTUnits* parseUnits(Parser* self) { return NULL; }

#pragma mark - PARSE TYPE
static ASTType* parseType(Parser* self, bool shouldParseBody)
{
    ASTType* type = jet_new(ASTType);

    discard(self, tkKeyword_type);
    discard(self, tkOneSpace);

    type->line = self->token.line;
    type->col = self->token.col;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);
    if (*self->token.pos < 'A' or *self->token.pos > 'Z')
        Parser_errorInvalidIdent(self);
    type->name = parseIdent(self);

    if (Parser_ignore(self, tkOneSpace)
        and Parser_ignore(self, tkKeyword_extends)) {
        discard(self, tkOneSpace);
        type->super = parseTypeSpec(self);
    }
    Parser_ignore(self, tkNewline);

    type->body = NULL; // this means type is declare
    if (TypeType_byName(type->name) != TYUnresolved) {
        Parser_errorDuplicateType(self, type, NULL);
        return type;
    }

    if (not shouldParseBody) return type;

    type->body = parseScope(self, NULL, true);

    discard(self, tkKeyword_end);
    discard(self, tkOneSpace);
    discard(self, tkKeyword_type);

    return type;
}

static ASTEnum* parseEnum(Parser* self)
{
    ASTEnum* en = jet_new(ASTEnum);

    discard(self, tkKeyword_enum);
    discard(self, tkOneSpace);

    en->line = self->token.line;
    en->col = self->token.col;

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);
    if (*self->token.pos < 'A' or *self->token.pos > 'Z')
        Parser_errorInvalidIdent(self);
    en->name = parseIdent(self);

    Parser_ignore(self, tkNewline);

    if (TypeType_byName(en->name) != TYUnresolved) {
        // conflicts with a primitive type name
        Parser_errorDuplicateEnum(self, en, NULL);
        return en;
    }

    en->body = parseEnumBody(self);

    discard(self, tkKeyword_end);
    discard(self, tkOneSpace);
    discard(self, tkKeyword_enum);

    return en;
}

static ASTImport* parseImport(Parser* self)
{
    ASTImport* import = jet_new(ASTImport);
    char* tmp;
    discard(self, tkKeyword_import);
    discard(self, tkOneSpace);

    import->isPackage = Parser_ignore(self, tkAt);

    if (memchr(self->token.pos, '_', self->token.matchlen))
        Parser_errorInvalidIdent(self);

    import->importFile = parseIdent(self);
    size_t len = self->token.pos - import->importFile;
    Parser_ignore(self, tkOneSpace);
    if (Parser_ignore(self, tkKeyword_as)) {

        Parser_ignore(self, tkOneSpace);
        import->hasAlias = true;

        if (memchr(self->token.pos, '_', self->token.matchlen))
            Parser_errorInvalidIdent(self);

        tmp = parseIdent(self);
        if (tmp) import->aliasOffset = (uint32_t)(tmp - import->importFile);

    } else {
        import->aliasOffset = (uint32_t)(
            str_base(import->importFile, '.', len) - import->importFile);
    }

    Parser_ignore(self, tkOneSpace);

    if (self->token.kind != tkLineComment and self->token.kind != tkNewline)
        Parser_errorUnexpectedToken(self);
    while (self->token.kind != tkLineComment and self->token.kind != tkNewline)
        Token_advance(&self->token);
    return import;
}
void analyseModule(Parser* self, ASTModule* mod);

static jet_PtrList* parseModule(Parser* self)
{
    ASTModule* root = jet_new(ASTModule);
    root->name = self->moduleName;
    const bool onlyPrintTokens = false;
    Token_advance(&self->token); // maybe put this in parser ctor
    ASTImport* import = NULL;

    // The take away is (for C gen):
    // Every caller who calls append(List) should keep a local List*
    // to follow the list top as items are appended. Each actual append
    // call must be followed by an update of this pointer to its own
    // ->next. Append should be called on the last item of the list, not
    // the first. (it will work but seek through the whole list every
    // time).

    List(ASTFunc)** funcs = &root->funcs;
    List(ASTImport)** imports = &root->imports;
    List(ASTType)** types = &root->types;
    List(ASTTest)** tests = &root->tests;
    List(ASTEnum)** enums = &root->enums;
    // List(ASTVar)** globalsTop = &root->globals;

    while (self->token.kind != tkNullChar) {
        if (onlyPrintTokens) {
            printf("%s %2d %3d %3d %-20s\t%.*s\n", self->moduleName,
                self->token.line, self->token.col, self->token.matchlen,
                TokenKind_str[self->token.kind],
                self->token.kind == tkNewline ? 0 : self->token.matchlen,
                self->token.pos);
            Token_advance(&self->token);
            continue;
        }
        switch (self->token.kind) {
        case tkKeyword_declare:
            Token_advance(&self->token);
            discard(self, tkOneSpace);
            if (self->token.kind == tkKeyword_function) {
                funcs = jet_PtrList_append(funcs, parseFunc(self, false));
                // if ((*funcs)->next) funcs = &(*funcs)->next;
            }
            if (self->token.kind == tkKeyword_type) {
                types = jet_PtrList_append(types, parseType(self, false));
                // if ((*types)->next) types = &(*types)->next;
            }
            break;

        case tkKeyword_function:
            funcs = jet_PtrList_append(funcs, parseFunc(self, true));
            // if ((*funcs)->next) funcs = &(*funcs)->next;
            break;

        case tkKeyword_enum:
            enums = jet_PtrList_append(enums, parseEnum(self));
            // if ((*enums)->next) enums = &(*enums)->next;
            break;

        case tkKeyword_type: {
            ASTType* type = parseType(self, true);
            types = jet_PtrList_append(types, type);
            // if ((*types)->next) types = &(*types)->next;

            // create default constructor
            ASTFunc* ctor = jet_new(ASTFunc);
            ctor->line = type->line;
            ctor->isDefCtor = true;
            // Ctors must AlWAYS return a new object.
            // even Ctors with args.
            ctor->returnsNewObjectAlways = true;
            ctor->name = type->name;
            char buf[128];
            int l = snprintf(buf, 128, "%s_new_", type->name);
            ctor->selector = pstrndup(buf, l);
            ASTTypeSpec* tspec = ASTTypeSpec_new(TYObject, CTYNone);
            tspec->type = type;
            ctor->returnSpec = tspec;
            funcs = jet_PtrList_append(funcs, ctor);
            // if ((*funcs)->next) funcs = &(*funcs)->next;

            // create some extra function declares
            char* defFuncs[] = { "json", "print", "describe" };
            for (int i = 0; i < jet_countof(defFuncs); i++) {
                ASTFunc* func
                    = ASTFunc_createDeclWithArg(defFuncs[i], NULL, type->name);
                func->line = type->line;
                func->intrinsic = true;
                funcs = jet_PtrList_append(funcs, func);
                // if ((*funcs)->next) funcs = &(*funcs)->next;
            }

        } break;

        case tkKeyword_import:
            import = parseImport(self);
            if (import) {
                imports = jet_PtrList_append(imports, import);
                // if ((*imports)->next) imports = &(*imports)->next;
                //                    auto subParser = new
                //                    Parser(import->importFile);
                //                    List<ASTModule*> subMods =
                //                    parse(subParser);
                //                    jet_PtrList_append(&modules, subMods);
            }
            break;

        case tkKeyword_test:
            tests = jet_PtrList_append(tests, parseTest(self));
            // if ((*tests)->next) tests = &(*tests)->next;
            break;
        // case tkKeyword_var:
        // case tkKeyword_let:
        // TODO: add these to exprs
        // jet_PtrList_append(globalsTop, parseVar(self));
        // if ((*globalsTop)->next) globalsTop =
        // &(*globalsTop)->next;
        // break;
        case tkNewline:
        case tkLineComment:
        // TODO: add line comment to module exprs
        case tkOneSpace:
            Token_advance(&self->token);
            break;
        case tkIdentifier: // stmt funcs: f(x) := f(y, w = 4) etc.
            if (Token_peekCharAfter(&self->token) == '(') {
                funcs = jet_PtrList_append(funcs, parseStmtFunc(self));
                // if ((*funcs)->next) funcs = &(*funcs)->next;
                break;
            }
        default:
            Parser_errorUnexpectedToken(self);
            while (self->token.kind != tkNewline
                and self->token.kind != tkLineComment
                and self->token.kind != tkNullChar)
                Token_advance(&self->token);
        }
    }
    // also keep modulesTop

    // Add some default functions "built-ins"
    // TODO: move this into a function later

    char* defTypes[] = { "String", "Number", "Boolean" };
    char* defFuncs[] = { "json", "print", "describe" };
    char* retTypes[jet_countof(defFuncs)] = {}; // fill these for non-void funcs

    for (int j = 0; j < jet_countof(defTypes); j++)
        for (int i = 0; i < jet_countof(defFuncs); i++) {
            ASTFunc* func = ASTFunc_createDeclWithArg(
                defFuncs[i], retTypes[i], defTypes[j]);
            func->intrinsic = true;
            funcs = jet_PtrList_append(funcs, func);
            // if ((*funcs)->next) funcs = &(*funcs)->next;
        }

    // do some analysis that happens after the entire module is loaded
    analyseModule(self, root);

    jet_PtrList_append(&self->modules, root);
    return self->modules;
}

// TODO: move this to separate file or to analysis.c [sempass.c]

static void ASTModule_unmarkTypesVisited(ASTModule* mod);
static int ASTExpr_markTypesVisited(Parser* self, ASTExpr* expr);
static int ASTType_checkCycles(Parser* self, ASTType* type);

void analyseModule(Parser* self, ASTModule* mod)
{
    // If function calls are going to be resolved based on the type of
    // first arg, then ALL functions must be visited in order to
    // generate their selectors and resolve their typespecs. (this does
    // not set the resolved flag on the func -- that is done by the
    // semantic pass)
    jet_foreach(ASTFunc*, func, mod->funcs)
    {
        jet_foreach(ASTVar*, arg, func->args)
            resolveTypeSpec(self, arg->typeSpec, mod);
        if (func->returnSpec) resolveTypeSpec(self, func->returnSpec, mod);
        getSelector(func);
    }

    ASTFunc* fstart = NULL;
    // don't break on the first match, keep looking so that duplicate starts
    // can be found
    jet_foreach(ASTFunc*, func, mod->funcs) if (not strcmp(func->name, "start"))
        fstart
        = func;

    /* TODO: what if you have tests and a start()? Now you will have to analyse
     the tests anyway */

    if (fstart) {
        ASTFunc_analyse(self, fstart, mod);
        // Check dead code -- unused funcs and types, and report warnings.
        jet_foreach(ASTFunc*, func, mod->funcs) if (not func->analysed
            and not func->isDefCtor) Parser_warnUnusedFunc(self, func);
        jet_foreach(ASTType*, type, mod->types) if (not type->analysed)
            Parser_warnUnusedType(self, type);

    } else { // TODO: new error, unless you want to get rid of start
        eputs(
            "\n\e[31m*** error:\e[0m cannot find function \e[33mstart\e[0m.\n");
        self->issues.errCount++;
    }

    // this happens regardless of whether start was found or not
    if (self->mode == PMGenTests or self->mode == PMLint) {
        jet_foreach(ASTTest*, test, mod->tests) analyseTest(self, test, mod);
    }
    // Check each type for cycles in inheritance graph.
    // Actually if there is no inheritance and composition is favoured, you have
    // to check each statement in the type body instead of just walking up the
    // super chain. If any statements are initialized by constructors, mark the
    // type of that statement as visited and recur into that type to check its
    // statements to see if you ever revisit anything. Unfortunately it does not
    // seem that this would be easy to do iteratively (not recursively), as it
    // can be done for just checking supers.
    // jet_foreach (ASTType*, type, mod->types) {
    //     if (not type->analysed or not type->super) continue;
    //     assert(type->super->typeType == TYObject);

    //     // traverse the type hierarchy for this type and see if you revisit
    //     any ASTType* superType = type->super->type; while (superType) {
    //         if (superType->visited) {
    //             Parser_errorInheritanceCycle(self, type);
    //             break;
    //         }
    //         superType->visited = true;
    //         if (not superType->super) break;
    //         assert(superType->super->typeType == TYObject);
    //         superType = superType->super->type;
    //     }

    //     // reset the cycle check flag on all types
    //     jet_foreach (ASTType*, etype, mod->types)
    //         if (type->analysed) etype->visited = false;
    // }

    // check each stmt in each type to find cycles.
    jet_foreach(ASTType*, type,
        mod->types) if (type->analysed and type->body and not type->visited)
    {
        if (ASTType_checkCycles(self, type)) {
            // cycle was detected. err has been reported along with a
            // backtrace. now just unset the dim control codes.
            eprintf(" ...%s\n", "\e[0m");
            // just report the first cycle found. typically there will be
            // only one cycle and you will end up reporting the same cycle
            // for all types that are in it, which is useless.
            // break;
            // the last type (for which the error was reported) won't have
            // its cycle check flags cleared, but who cares.
            // OTHER IDEA: clear the flag only if there was no error. that
            // way the next iteration will skip over those whose flags are
            // already set.
        } else
            ASTModule_unmarkTypesVisited(mod);
    }
}

// return 0 on no cycle found, -1 on cycle found
static int ASTType_checkCycles(Parser* self, ASTType* type)
{
    jet_foreach(ASTExpr*, stmt,
        type->body->stmts) if (ASTExpr_markTypesVisited(self, stmt))
    {
        eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
            type->name, self->filename, stmt->line, stmt->col);
        return -1;
    }
    return 0;
}

static int ASTExpr_markTypesVisited(Parser* self, ASTExpr* expr)
{
    ASTType* type = NULL;
    if (!expr) return 0;
    switch (expr->kind) {
    case tkVarAssign:
        return ASTExpr_markTypesVisited(self, expr->var->init);
    case tkFunctionCall:
        return ASTExpr_markTypesVisited(self, expr->left);
    case tkFunctionCallResolved:
        if (ASTExpr_markTypesVisited(self, expr->left)) return -1;
        // if (expr->func->isDefCtor) type =
        // expr->func->returnSpec->type;
        if (expr->func->returnSpec->typeType == TYObject
            and expr->func->returnsNewObjectAlways)
            type = expr->func->returnSpec->type;
        break;
    case tkSubscript:
    case tkSubscriptResolved:
        return ASTExpr_markTypesVisited(self, expr->left);
    case tkIdentifierResolved:
    case tkString:
    case tkNumber:
    case tkRawString:
    case tkLineComment:
        return 0;
    default:
        if (expr->prec) {
            int ret = 0;
            if (not expr->unary)
                ret += ASTExpr_markTypesVisited(self, expr->left);
            ret += ASTExpr_markTypesVisited(self, expr->right);
            if (ret) return ret;
        } else
            unreachable("unknown expr kind: %s at %d:%d\n",
                TokenKind_str[expr->kind], expr->line, expr->col)
    }
    if (not type) return 0;
    if (type->visited) {
        Parser_errorConstructorHasCycle(self, type);
        eprintf("%s", "\e[;2m"); // Backtrace (innermost first):\n");
        return -1;
    }
    type->visited = true;
    return ASTType_checkCycles(self, type);
}

static void ASTModule_unmarkTypesVisited(ASTModule* mod)
{
    // reset the cycle check flag on all types
    jet_foreach(ASTType*, type, mod->types) type->visited = false;
}
