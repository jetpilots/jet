#include "Token.hpp"

struct Parser {
    const char* file;
    char* data;
    long size;
    Token token;
    Diagnostics& errs;
    // Diagnostics& errs;

    Parser(const char* filename, Diagnostics& diag)
        : file(filename)
        , errs(diag) {
        // load data, make a copy in lines
    }

    bool matches(TokenKind k) { return token.is(k); }

    Expr* expr() {
        Expr* expr = fromToken();
        token.advance();
        return expr;
    }

    Expr* next_token_node(TokenKind expected, const bool ignore_error) {
        if (token.is(expected)) {
            return expr();
        } else {
            if (not ignore_error) errs.expectedToken(expected);
            return nullptr;
        }
    }
    Expr* match(TokenKind expected) { return next_token_node(expected, false); }

    Expr* trymatch(TokenKind expected) {
        return next_token_node(expected, true);
    }

    bool ignore(TokenKind expected) {
        bool ret;
        if ((ret = token.is(expected))) token.advance();
        return ret;
    }

    // this is same as match without return
    void discard(TokenKind expected) {
        if (not ignore(expected)) errs.expectedToken(expected);
    }
    Type* parseEnum() {
        // put it in mod.types, not mod.enums
    }
    // void format() {
    //     for (CString line : lines) {
    //         int cols = line.len();
    //         while (not token.is(tkNullChar)) token.format();
    //         token.discard(tkNullChar);
    //     }
    // }
    Module* parseModule() { // read file and parse
        if (not loadFile()) {
            eprintf("%s: no such file");
            return nullptr;
        };
        errs = new Diagnostics(filename, data, size);
        Module* root = new Module();
        root->errs = errs;
    }
    Func* parseFunc() { }
    Func* parseStmtFunc() { }
    Var* parseVar(Scope& scope) {
        Var *v = new Var, &var = *v;
        var.isMutable = token.is(tkKeyword_var);
        discard(var.isMutable ? tkKeyword_var : tkKeyword_let);
        discard(tkOneSpace);
        var.loc = token.loc();
        var.name = parseIdent();
        if (contains(var.name, '_') or isCapitalized(var.name))
            errs.invalidName(var);
        if (ignore(tkOneSpace) and token.is(tkIdentifier)) {
            var.typeInfo = parseTypeSpec();
        } else {
            var.typeInfo.loc = token.loc();
            var.typeInfo.name = "";
        }
        ignore(tkOneSpace);
        if (ignore(tkOpAssign)) var.init = parseExpr(scope);
        return v;
    }
    Scope* parseScope(Scope* parent) {
        Scope* scope = new Scope;

        Var *var = nullptr, *orig = nullptr;
        Expr* expr = nullptr;
        TokenKind tt = tkUnknown;
        Scope* forScope = nullptr;

        scope->parent = parent;
        bool startedElse = false;
        bool startedCase = false;

        while (not token.is(tkKeyword_end)) {
            switch (token._kind) {
            case tkNullChar: errs.expectedToken(tkUnknown); goto exitloop;

            case tkKeyword_var:
            case tkKeyword_let:
                if (not(var = parseVar(*scope)))
                    continue;
                else
                    token.advance();

                if ((orig = scope->getVar(var->name)))
                    errs.duplicateVar(var, orig);

                scope->vars.push(*var);

                expr = new Expr(tkVarAssign,
                    { var->init ? var->init->line : var->line,
                        var->init ? var->init->col - 2 : 1, 1 });
                expr->prec = getPrecedence(tkOpAssign);
                expr->var = var;

                scope->stmts.push(*expr);
                break;

            case tkKeyword_case: goto exitloop;

            case tkKeyword_match:
                if (isTypeBody) errorInvalidTypeMember(parser);

                expr = match(tkKeyword_match);
                expr->left = parseExpr(*scope);
                token.advance();

                expr->body = parseScopeCases(scope);

                discard(tkKeyword_end);
                ignore(tkOneSpace);
                ignore(tkKeyword_match);
                scope->stmts.push(*expr);
                break;

            case tkKeyword_else:
            case tkKeyword_elif:
                if (not startedElse) goto exitloop;

            case tkKeyword_if:
            case tkKeyword_for:
            case tkKeyword_while:
                if (isTypeBody) errorInvalidTypeMember(parser);
                tt = token._kind;
                expr = match(tt);
                expr->left = tt != tkKeyword_else ? parseExpr(*scope) : nullptr;

                // // because we are going to be calling resolveVars right now,
                // we
                // // need to trample the newline
                // token.advance();

                // if(token.pos)
                // TODO: for must <parse its expr as a VarDecl, because it can
                // have 'as Type' etc. Now you parse an assignment Expr and hack
                // an  Var out of it.
                if (tt == tkKeyword_for) {
                    // TODO: new error
                    Var* fvar = nullptr;
                    if (not expr->left)
                        unreachable("Missing for-loop condition at %d:%d\n",
                            expr->line, expr->col);
                    else {
                        if (not expr->left->is(tkKeyword_in))
                            unreachable("Invalid for-loop condition: %s\n",
                                TokenKind_repr(expr->left->kind, false));

                        fvar = new Var;
                        fvar->name = expr->left->left->string;
                        fvar->loc = expr->loc;
                        fvar->isMutable = true;
                        fvar->init = expr->left->right;
                        fvar->typeInfo.typeType = TYReal64;

                        if ((orig = scope->getVar(fvar->name)))
                            errs.duplicateVar(fvar, orig -);
                    }
                    forScope = new Scope;
                    if (fvar) forScope->vars.shift(*fvar);
                    forScope->parent = scope;
                }

                expr->body = parseScope(tt == tkKeyword_for ? forScope : scope);

                if (token.in(tkKeyword_else, tkKeyword_elif)) {
                    startedElse = true;
                } else {
                    discard(tkKeyword_end);
                    ignore(tkOneSpace);
                    ignore(tt == tkKeyword_else or tt == tkKeyword_elif
                            ? tkKeyword_if
                            : tt);
                }
                scope->stmts.push(*expr);
                break;

            case tkNewline:
            case tkOneSpace: token.advance(); break;

            case tkKeyword_function:
            case tkKeyword_type:
            case tkKeyword_enum:
            case tkKeyword_test:
                // in this case, it is probably an error propagating all through
                // the file because an early func or type missed an end. How
                // about we stop parsing the scope here.
                goto exitloop;

            case tkLineComment:
                if (com->generateCommentExprs) {
                    expr = token.expr();
                    scope->stmts.push(*expr);
                }
                // token.advance();
                break;

            default:
                expr = parseExpr(*scope);
                if (expr and isTypeBody) {
                    errs.invalidMember(expr);
                    expr = nullptr;
                }
                if (not expr) break;
                scope->stmts.push(*expr);
                // token.advance(); // eat the newline
                // resolveVars(expr, scope, false);
                break;
            }
        }
    exitloop:
        return scope;
    }

    Type* parseType() { }
    Import* parseImport() { }
    TypeInfo parseTypeSpec() {
        token.mergeArrayDims = true;
        TypeInfo typeSpec;
        // typeSpec.loc = { token.line, token.col, token.len };
        typeSpec.name = parseIdent(parser);

        if (matches(tkArrayDims)) {
            if (isalpha(*token.pos)) {
                // Dict
            } else {
                int dims = max(1, token.count(':'));
                if (dims > 15) errs.tooManyDims(token);
                typeSpec.collectionType = (dims == 1) ? CTYArray : CTYTensor;
                typeSpec.dims = dims;
            }
            token.advance();
        }
        ignore(tkUnits);
        assert(not token.is(tkUnits));
        assert(not token.is(tkArrayDims));
        token.mergeArrayDims = false;
        return typeSpec;
    }

    List<Var&>* parseArgs() {
        List<Var&>* args = nullptr;
        discard(tkParenOpen);
        if (ignore(tkParenClose)) return args;
        do {
            Var* arg = parseVar();
            arg->isArgument = true;
            args.push(arg);
        } while (ignore(tkOpComma));
        discard(tkParenClose);
        return args;
    }

    Scope* parseScopeCases(Scope* parent) {
        Scope* scope = new Scope;
        scope->parent = parent;
        while (not token.in(tkNewline, tkNullChar, tkKeyword_end)) {
            switch (token._kind) {
            case tkKeyword_case:
                Expr* expr = match(tkKeyword_case);
                expr->left = parseExpr(*scope);
                expr->body = parseScope(scope);
                scope->stmts.push(*expr);
                break;
            case tkNewline:
            case tkOneSpace:
            case tkLineComment: token.advance(); break;
            // case tkKeyword_end: goto exitloop;
            default:
                errs.unexpectedToken(token, "expected 'case' or 'end'");
                token.advance();
            }
        }
        // exitloop:
        return scope;
    }
    Scope* parseEnumBody(Scope* globScope) {
        Scope* scope = new Scope;
        scope->parent = globScope;
        Expr* expr = nullptr;
        Var* var = nullptr;

        while (not token.is(tkKeyword_end)) {
            switch (token._kind) {

            case tkNullChar: errs.expectedToken(tkUnknown); goto exitloop;

            case tkNewline:
            case tkOneSpace: token.advance(); break;

            case tkLineComment:
                if (com->generateCommentExprs) scope->stmts.push(token.expr());
                token.advance();
                break;

            case tkIdentifier:

                // if (lex.kind != tkIdentifier) {
                //     errorInvalidTypeMember(parser);
                //     while (lex.pos != tkNewline)
                //         token.advance();
                //     token.advance(); // eat the newline
                //     break;
                // }

                expr = parseExpr(*scope);

                if (not expr) break;
                if (not expr->in(tkIdentifier, tkOpAssign)) {
                    errs.invalidEnumMember(expr);
                    unreachable("%s\n", TokenKind_str[expr->kind]);
                    expr = nullptr;
                }
                scope->stmts.push(*expr);

                var = new Var;
                var->loc = expr->loc;
                var->name = (expr->is(tkOpAssign)) ? expr->left->string
                                                   : expr->string;
                var->init = expr->right;
                scope->vars.push(*var);

                break;

            default: errs.unexpectedToken(token); goto exitloop;
            }
        }
    exitloop:
        return scope;
    }

    Expr* parseExpr(Scope& scope) {
        // there are 2 steps to this madness.
        // 1. parse a sequence of tokens into RPN using shunting-yard.
        // 2. walk the rpn stack as a sequence and copy it into a result
        // stack, collapsing the stack when you find nonterminals (ops, func
        // calls, array index, ...)

        // I guess if you want to parse something like if x == 3 x = 4 -- NO WAY
        // NEVER then you have to fold the rpn as soon as you have two
        // consecutive non-ops on the stack and are pushing a third. (x 3)
        // opstack (==) pushing x -> fold. but dont allow this monstrosity! this
        // is why `if x == 3 then x = 4` is needed

        static Array<Expr*> rpn, ops, result;
        int prec_top = 0;
        Expr* p = nullptr;
        TokenKind revBrkt = tkUnknown;

        // ******* STEP 1 CONVERT TOKENS INTO RPN

        while (not(token.in(tkNullChar, tkNewline, tkLineComment))) {

            ignore(tkOneSpace);

            Expr* expr;
            if (token.is(tkParenOpen))
                expr = lparen;
            else if (token.is(tkParenClose))
                expr = rparen;
            else
                expr = token.expr(); // dont advance yet

            int prec = expr->prec;
            bool rassoc = prec ? expr->rassoc : false;
            char lookAhead = token.peek();

            switch (expr->kind) {
            case tkIdentifier:
                expr->slen = token.matchlen;
                switch (lookAhead) {
                // TODO: need a general lookahead that skips whitespace.
                // case '!':
                //     if (token.pos[2] != '(') goto defaultCase;
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
                    if (token.pos[2] != '{') goto defaultCase;
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
                if (not ops.empty() and ops.top()->is(tkFunctionCall))
                    rpn.push(expr);
                if (lookAhead == ')') rpn.push(nullptr); // empty func()
                if (lookAhead == '&') token.advance(); // for mutators, &
                break;

            case tkArrayOpen:
                ops.push(expr);
                if (not ops.empty() and ops.top()->is(tkSubscript))
                    rpn.push(expr);
                if (lookAhead == ']') rpn.push(nullptr); // empty arr[]
                break;

            case tkBraceOpen:
                ops.push(expr);
                if (not ops.empty() and ops.top()->is(tkObjectInit))
                    rpn.push(expr);
                if (lookAhead == '}') rpn.push(nullptr); // empty Obj {}
                break;

            case tkParenClose:
            case tkArrayClose:
            case tkBraceClose:

                revBrkt = reverseBracket(expr->kind);
                if (ops.empty()) { // need atleast the opening bracket
                    errs.syntaxError(expr, "mismatched bracket");
                    goto error;
                } else {
                    while (not ops.empty()) {
                        p = ops.pop();
                        if (p->is(revBrkt)) break;
                        rpn.push(p);
                    }
                }

                // tkArrayOpen is a unary op, used for list literals
                if ((p and p->is(tkArrayOpen)))
                    if ((ops.empty()
                            or (rpn.top() and not ops.top()->is(tkSubscript)))
                        // don't do this if its part of a subscript
                        or (rpn.empty() or (not rpn.top()->is(tkOpColon))))
                        // or aa range. range exprs are handled separately. by
                        // themselves they don't need a surrounding [], but for
                        // grouping like 2+[8:66] they do.
                        rpn.push(p);

                // a dict literal (another unary op).
                if ((p and p->is(tkBraceOpen))
                    and (ops.empty()
                        or (rpn.top() and ops.top()->kind != tkObjectInit)))
                    // not if it is an object init
                    rpn.push(p);
                // Object { member1 = 3, member3 = "train" }
                // ^^ this is an object init, not a dict
                // { "bing" = 34, "whang" = 33 }
                // ^^ this is a dict

                break;

            case tkKeyword_check: ops.push(expr); break;
            case tkExclamation:
                if (rpn.empty() or not rpn.top()->is(tkIdentifier))
                    errs.syntaxError(expr, "invalid use of '!'");

                break;

            case tkKeyword_return: // for empty return, push a nullptr
                ops.push(expr);
                if (lookAhead == '~' or lookAhead == '\n') rpn.push(nullptr);
                break;

            case tkUnaryDot:
                expr->kind = tkPeriod;
                expr->unary = false;
                rpn.push(nullptr); // will be substituted later with a dummy var
                                   // of the inferred enum type.
                fallthrough;

            default:
                if (prec) {
                    if (expr->is(tkOpColon)) {
                        if (rpn.empty()
                            or (not rpn.top() and !ops.empty()
                                and not ops.top()->is(tkOpColon))
                            or (rpn.top()->is(tkOpColon) and !ops.empty()
                                and (ops.top()->in(tkOpComma, tkArrayOpen))))
                            // TODO: better way to parse :, 1:, :-1, etc.
                            // while passing tokens to RPN, if you see a :
                            // with nothing on the RPN or comma or [, push a
                            // nullptr. while unwinding the op stack, if you
                            // pop a : and see a nullptr or comma on the rpn,
                            // push another nullptr.
                            rpn.push(&expr_const_0); // indicates empty operand
                    }
                    while (not ops.empty()) {
                        prec_top = ops.top()->prec;
                        if (not prec_top or prec > prec_top) break;
                        if (prec == prec_top and rassoc) break;
                        p = ops.pop();

                        if (not(p->in(tkOpComma, tkOpSemiColon, tkFunctionCall,
                                tkSubscript))
                            and rpn.top() and rpn.top()->is(tkOpComma)) {
                            errs.unexpectedExpr(*p, "unsupported use of comma");
                            goto error;
                        }

                        if (not(p->prec or p->unary)
                            and not p->in(
                                tkFunctionCall, tkOpColon, tkSubscript)
                            and rpn.count() < 2) {
                            errs.unexpectedExpr(*p, "need 2 operands to binop");
                            goto error;
                        }

                        rpn.push(p);
                    }

                    if (rpn.empty() and not expr->unary) {
                        errs.unexpectedExpr(
                            *p, "binary op with no left operand");
                        goto error;
                    }
                    if (expr->is(tkOpColon) and isin(lookAhead, ",:])"))
                        rpn.push(&expr_const_0);
                    ops.push(expr);
                } else {
                    rpn.push(expr);
                }
            }
            token.advance();
            ignore(tkOneSpace);
        }
    exitloop:

        while (not ops.empty()) {
            p = ops.pop();

            if (not p->in(tkOpComma, tkFunctionCall, tkSubscript, tkArrayOpen)
                and rpn.top() and rpn.top()->is(tkOpComma)) {
                errs.unexpectedExpr(*rpn.top());
                goto error;
            }

            if (not(p->prec or p->unary)
                and not(p->in(tkFunctionCall, tkSubscript))
                and rpn.count() < 2) {
                errs.syntaxError(*p, "invalid use of comma");
                goto error;
                // TODO: even if you have more than two, neither of the top
                // two should be a comma
            }

            // if (p->is(tkIdentifier, tkSubscript)) resolve(p, nullptr, scope);

            rpn.push(p);
        }

        // *** STEP 2 CONVERT RPN INTO EXPR TREE

        Expr* arg;
        for (int i = 0; i < rpn.count(); i++) {
            if ((p = rpn[i])) goto justpush;
            switch (p->kind) {
            case tkFunctionCall:
            case tkSubscript:
                if (result.count() > 0) {
                    arg = result.pop();
                    if (arg and p->is(tkSubscript)) {
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
            case tkLineComment: break;

            default:
                // everything else is a nonterminal, needs left/right
                if (not p->prec) {
                    errs.syntaxError(*p, "unexpected token with no precedence");
                    goto error;
                }

                if (result.empty()) {
                    errs.syntaxError(*p, "no operands");
                    goto error;
                }

                p->right = result.pop();

                if (not p->unary) {
                    if (result.empty()) {
                        errs.syntaxError(*p, "need 2 operands to binary op");
                        goto error;
                    }
                    p->left = result.pop();
                }
            }
        justpush:
            result.push(p);
        }

        if (not result.count()) {
            errs.unexpectedToken("nothing parsed");
        } else if (result.count() != 1) {
            if (not result.top()->is(tkLineComment)) {
                errs.syntaxError(*p, "more than 1 result");
            }
        } else {
            Expr* ret = result[0];
            ops.reset();
            rpn.reset();
            result.reset();

            token.advance();
            resolve(*ret, nullptr, scope);
            return ret;
        }

    error:

        token.skipLine();
        if (ops.count()) eputs("ops: "), ops.print(stderr), eputs(";;");
        if (rpn.count()) eputs("rpn: "), rpn.print(stderr), eputs(";;");
        if (result.count())
            eputs("result: "), result.print(stderr), eputs(";;");
        if (p) eputs("p: "), print(p, stderr);
        eputs("\n");

        ops.reset(); // "reset" stacks
        rpn.reset();
        result.reset();
        return nullptr;
    }

#include "Parser_resolve.hpp"
};

void print(Expr* p, FILE* file = stdout) {
    if (not p)
        fputs("NUL ", file);
    else
        fprintf(
            file, "%.*s ", 32, p->prec ? TokenKinds_repr[p->kind] : p->string);
}