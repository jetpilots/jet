class Parser {
    char* filename;
    char* moduleName;
    char* mangledName;
    char* capsMangledName;
    char *data, *end;
    char* noext;
    Array<char*> orig;

    Token token;
    IssueMgr issues;

    ParserMode mode : 8;
    bool generateCommentExprs;

    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, ui : 1;
    } requires;

private:
    void ASTScope_checkUnusedVars(Parser* parser, ASTScope* scope) {
        for (auto& var : scope->locals)
            if (!var->used) Parser_warnUnusedVar(parser, var);

        for (auto& stmt : scope->stmts)
            if (isCtrlExpr(stmt) && stmt->body)
                ASTScope_checkUnusedVars(parser, stmt->body);
    }

    void ASTFunc_checkUnusedVars(Parser* parser, ASTFunc* func) {
        for (auto& arg : func->args)
            if (!arg->used) Parser_warnUnusedArg(parser, arg);

        ASTScope_checkUnusedVars(parser, func->body);
    }

    void ASTTest_checkUnusedVars(Parser* parser, ASTTest* test) {
        ASTScope_checkUnusedVars(parser, test->body);
    }

    void resolveMember(Parser* parser, ASTExpr* expr, ASTType* type) {
        assert(expr->kind == tkIdentifier || expr->kind == tkSubscript);
        TokenKind ret = (expr->kind == tkIdentifier) ? tkIdentifierResolved
                                                     : tkSubscriptResolved;
        ASTVar* found = NULL;
        if (type->body) found = ASTScope_getVar(type->body, expr->string);
        if (found) {
            expr->kind = ret;
            expr->var = found;
            expr->var->used = true;
        } else {
            Parser_errorUnrecognizedMember(parser, type, expr);
        }
    }

    void resolveVars(
        Parser* parser, ASTExpr* expr, ASTScope* scope, bool inFuncCall) {

        if (!expr) return;
        switch (expr->kind) {
        case tkIdentifierResolved:
            break;

        case tkIdentifier:
        case tkSubscript: {
            TokenKind ret = (expr->kind == tkIdentifier) ? tkIdentifierResolved
                                                         : tkSubscriptResolved;
            ASTVar* found = ASTScope_getVar(scope, expr->string);
            if (found) {
                expr->kind = ret;
                expr->var = found;
                expr->var->used = true;

            } else {
                Parser_errorUnrecognizedVar(parser, expr);
            }
            if (expr->kind == tkSubscriptResolved
                || expr->kind == tkSubscript) {
                resolveVars(parser, expr->left, scope, inFuncCall);
            }
            break;
        }
        case tkFunctionCall:
            if (expr->left) resolveVars(parser, expr->left, scope, true);
            break;

        case tkPeriod:
            if (expr->left) resolveVars(parser, expr->left, scope, inFuncCall);

            if (expr->right->kind == tkSubscript
                || expr->right->kind == tkSubscriptResolved)
                resolveVars(parser, expr->right->left, scope, inFuncCall);

            break;

        case tkString: {

            char* pos = strchr(expr->string, '$');
            while (pos) {
                if (pos[-1] != '\\') {
                    if (pos[1] == '(') pos++;
                    size_t len = strspn(pos + 1,
                        "abcdefghijklmnopqrstuvwxyz"
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "1234567890");
                    if (len) {
                        if (len > 31) len = 31;
                        char buf[32];
                        strncpy(buf, pos + 1, len);
                        buf[len] = 0;
                        buf[31] = 0;

                        ASTVar* var = ASTScope_getVar(scope, buf);
                        if (!var) {
                            char* orig = expr->string;
                            expr->string = buf;
                            expr->col += (pos - orig) + 1;
                            Parser_errorUnrecognizedVar(parser, expr);
                            expr->col -= (pos - orig) + 1;
                            expr->string = orig;
                        } else {

                            memcpy(pos + 1, var->name, len);
                        }

                    } else {
                        Parser_errorStringInterp(parser, expr, pos);
                    }
                }
                pos = strchr(pos + 1, '$');
            }
        } break;

        default:
            if (expr->prec) {
                if (!expr->unary) {
                    if (inFuncCall && expr->kind == tkOpAssign) {

                    } else {
                        resolveVars(parser, expr->left, scope, inFuncCall);
                    }
                }
                resolveVars(parser, expr->right, scope, inFuncCall);

                if (isSelfMutOp(expr)) {
                    ASTVar* var = NULL;
                    ASTExpr* varExpr = expr->left;
                    if (varExpr->kind == tkIdentifierResolved
                        || varExpr->kind == tkSubscriptResolved) {
                        var = varExpr->var;
                    } else if (varExpr->kind == tkPeriod && varExpr->left
                        && varExpr->left->kind == tkIdentifierResolved) {
                        varExpr = varExpr->left;
                        var = varExpr->var;
                    }
                    if (var) {

                        var->changed = true;
                        if (expr->kind == tkOpAssign) var->reassigned = true;
                        if (!var->isVar)
                            Parser_errorReadOnlyVar(parser, varExpr);
                    }
                }
            }
        }
    }

    void Parser::fini(Parser* parser) {
        free(parser->data);
        free(parser->noext);
        free(parser->moduleName);
        free(parser->mangledName);
        free(parser->capsMangledName);
    }

    void recordNewlines(Parser* parser) {

        char* cptr = parser->orig.ref[0];
        char* cend = cptr + (parser->end - parser->data);
        for (char* c = cptr; c < cend; c++) {
            if (*c == '\n') {
                *c = 0;

                PtrArray::push(&parser->orig, c + 1);
            }
        }
    }
    Parser* Parser::fromFile(char* filename, bool skipws, ParserMode mode) {

        size_t flen = CString::length(filename);

        if (!CString::endsWith(filename, flen, ".jet", 4)) {
            eprintf(
                "jet: file '%s' invalid: name must end in '.jet'.\n", filename);
            return NULL;
        }

        struct stat sb;

        if (stat(filename, &sb) != 0) {
            eprintf("jet: file '%s' not found.\n", filename);
            return NULL;
        } else if (S_ISDIR(sb.st_mode)) {

            eprintf(
                "jet: '%s' is a folder; only files are accepted.\n", filename);
            return NULL;
        } else if (access(filename, R_OK) == -1) {

            eprintf("jet: no permission to read file '%s'.\n", filename);
            return NULL;
        }

        FILE* file = fopen(filename, "r");
        assert(file);

        Parser* ret = new Parser;

        ret->filename = filename;
        ret->noext = CString::noext(filename);
        fseek(file, 0, SEEK_END);
        const size_t size = ftell(file) + 2;

        if (size < FILE_SIZE_MAX) {
            ret->data = (char*)malloc(size);
            fseek(file, 0, SEEK_SET);
            if (fread(ret->data, size - 2, 1, file) != 1) {
                eprintf(
                    "F+: the whole file '%s' could not be read.\n", filename);
                fclose(file);
                return NULL;
            }
            ret->data[size - 1] = 0;
            ret->data[size - 2] = 0;
            ret->moduleName = CString::tr(ret->noext, '/', '.');
            ret->mangledName = CString::tr(ret->noext, '/', '_');
            ret->capsMangledName = CString::upper(ret->mangledName);
            ret->end = ret->data + size;
            ret->orig = (PtrArray) {};
            PtrArray::push(&ret->orig, malloc(size));
            memcpy(ret->orig.ref[0], ret->data, size);
            ret->token.pos = ret->data;
            ret->token.skipWhiteSpace = skipws;
            ret->token.mergeArrayDims = false;
            ret->token.kind = tkUnknown;
            ret->token.line = 1;
            ret->token.col = 1;
            ret->mode = mode;
            ret->issues.errCount = 0;
            ret->issues.warnCount = 0;
            ret->issues.errLimit = 50000;
            ret->generateCommentExprs = (ret->mode == PMLint);

            if (ret->mode != PMLint) ret->issues.errLimit = 1;

        } else {
            eputs("Source files larger than 16MB are not allowed.\n");
        }

        fclose(file);
        recordNewlines(ret);
        return ret;
    }

    ASTExpr* exprFromCurrentToken(Parser* parser) {
        ASTExpr* expr = ASTExpr::fromToken(&parser->token);
        Token::advance(&parser->token);
        return expr;
    }

    ASTExpr* next_token_node(
        Parser* parser, TokenKind expected, const bool ignore_error) {
        if (parser->token.kind == expected) {
            return exprFromCurrentToken(parser);
        } else {
            if (!ignore_error) Parser::errorExpectedToken(parser, expected);
            return NULL;
        }
    }

    ASTExpr* Parser::match(Parser* parser, TokenKind expected) {
        return next_token_node(parser, expected, false);
    }

    ASTExpr* Parser::trymatch(Parser* parser, TokenKind expected) {
        return next_token_node(parser, expected, true);
    }

    bool Parser::matches(Parser* parser, TokenKind expected) {
        return (parser->token.kind == expected);
    }

    bool Parser::ignore(Parser* parser, TokenKind expected) {
        bool ret;
        if ((ret = Parser::matches(parser, expected)))
            Token::advance(&parser->token);
        return ret;
    }

    void Parser::consume(Parser* parser, TokenKind expected) {
        if (!Parser::ignore(parser, expected))
            Parser::errorExpectedToken(parser, expected);
    }

    char* parseIdent(Parser* parser) {
        if (parser->token.kind != tkIdentifier)
            Parser::errorExpectedToken(parser, tkIdentifier);
        char* p = parser->token.pos;
        Token::advance(&parser->token);
        return p;
    }
};
