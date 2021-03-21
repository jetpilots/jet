
static void setStmtFuncTypeInfo(Parser* parser, ASTFunc* func) {

    ASTExpr* stmt = func->body->stmts->item;
    if (!func->returnSpec->typeType)
        func->returnSpec->typeType = stmt->typeType;
    else if (func->returnSpec->typeType != stmt->typeType)
        Parser_errorTypeMismatchBinOp(parser, stmt);
}

static void analyseType(Parser* parser, ASTType* type, ASTModule* mod);
static void ASTFunc_analyse(Parser* parser, ASTFunc* func, ASTModule* mod);

static void analyseDictLiteral(Parser* parser, ASTExpr* expr, ASTModule* mod) {

}

static void ASTExpr_prepareInterp(
    Parser* parser, ASTExpr* expr, ASTScope* scope) {
    static Array(Ptr) vars;
    assert(expr->kind == tkString);
    PtrList** exprvars = &expr->vars;

    char* pos = expr->string + 1;
    int line = expr->line, col = expr->col;

    while (*pos) {

        char* dollar = pos;
        while (*dollar && *dollar != '$') {
            dollar++;
            col++;
            if (*dollar == '\n') {
                col = 0;
                line++;
            }
        }
        if (!*dollar) break;

        long lentxt = dollar - pos;

        char *varname = dollar + 1, *varend;
        bool wasbracket = varname[0] == '(';

        varend = varname;
        while (*varend && isalnum(*varend)) ++varend;

        col += varend - varname;
        char endchar = *varend;
        *varend = 0;
        ASTVar* var = ASTScope_getVar(scope, varname);
        if (!var)
            unreachable("undefined var found: %s", varname);
        else
            strcpy(varname, var->name);
        *varend = endchar;

        ASTExpr* ex = NEW(ASTExpr);
        ASTExpr* exdot = NULL;
        ex->kind = tkIdentifierResolved;
        ex->line = line, ex->col = col;
        ex->var = var;
        var->used = true;

        exdot = ex;
        while (endchar == '.') {
            if (!var || var->typeSpec->typeType != TYObject) {
                unreachable(
                    "using a . for a non-object type (string interp: $%.*s)",
                    (int)(varend - varname), varname);
                break;
            }

            varname = ++varend;
            while (*varend && isalnum(*varend)) ++varend;
            col += varend - varname;
            endchar = *varend;
            *varend = 0;
            ASTType* type = var->typeSpec->type;
            var = ASTType_getVar(type, varname);
            if (!var) {
                exdot = NULL;
                Parser_errorUnrecognizedMember(parser, type,
                    &(ASTExpr) { .string = varname,
                        .line = line,
                        .col = col,
                        .kind = tkIdentifier });
            } else {
                var->used = true;
                strcpy(varname, var->name);
                exdot = NEW(ASTExpr);
                exdot->kind = tkPeriod;

                exdot->left = ex;
                exdot->right = NEW(ASTExpr);
                exdot->right->kind = tkIdentifierResolved;
                exdot->right->line = line,
                exdot->right->col = col + varend - varname;
                exdot->right->var = var;
                ex = exdot;
            }
            *varend = endchar;
        }

        if (exdot) exprvars = PtrList_append(exprvars, exdot);
        pos = varend;
    }
}

static void ASTExpr_setEnumBase(
    Parser* parser, ASTExpr* expr, ASTTypeSpec* spec, ASTModule* mod) {
    switch (expr->kind) {
    case tkPeriod:
        if (expr->left) return;
        expr->left = NEW(ASTExpr);
        expr->left->kind = tkIdentifier;
        expr->left->string
            = spec->typeType == TYObject ? spec->type->name : spec->name;
        expr->left->line = expr->line;
        expr->left->col = expr->col;
        resolveVars(parser, expr, mod->scope, false);
    case tkPlus:
    case tkOpComma:
    case tkOpEQ:
        ASTExpr_setEnumBase(parser, expr->left, spec, mod);
        ASTExpr_setEnumBase(parser, expr->right, spec, mod);
    default:;
    }
}

static void analyseExpr(Parser* parser, ASTExpr* expr, ASTScope* scope,
    ASTModule* mod, ASTFunc* ownerFunc, bool inFuncArgs) {
    switch (expr->kind) {

    case tkFunctionCallResolved: {
        if (ownerFunc) {

            PtrList_shift(&ownerFunc->callees, expr->func);
            PtrList_shift(&expr->func->callers, ownerFunc);
        }
        if (ASTExpr_countCommaList(expr->left) != expr->func->argCount)
            Parser_errorArgsCountMismatch(parser, expr);
        expr->typeType = expr->func->returnSpec
            ? expr->func->returnSpec->typeType
            : TYNoType;
        expr->collectionType = expr->func->returnSpec
            ? expr->func->returnSpec->collectionType
            : CTYNone;
        expr->elemental = expr->func->elemental;
        if (expr->func->returnSpec) expr->dims = expr->func->returnSpec->dims;

        if (!expr->left) break;
        expr->elemental = expr->elemental && expr->left->elemental;
        expr->throws = expr->left->throws || expr->func->throws;
        ASTExpr* currArg = expr->left;
        foreach (ASTVar*, arg, expr->func->args) {
            ASTExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) cArg = cArg->right;
            if (cArg->typeType != arg->typeSpec->typeType)
                Parser_errorArgTypeMismatch(parser, cArg, arg);

            if (!(currArg = currArg->right)) break;
        }

        currArg = expr->left;
        while (currArg) {
            ASTExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) {

                assert(cArg->left->kind == tkIdentifier);
                ASTVar* theArg = NULL;
                foreach (ASTVar*, arg, expr->func->args) {
                    if (cArg->left->string ^= arg->name) theArg = arg;
                }
                if (!theArg) {
                    unreachable("unresolved argument %s!", cArg->left->string);

                } else {
                    cArg->left->var = theArg;
                    cArg->left->kind = tkIdentifierResolved;
                }
            }

            currArg = currArg->kind == tkOpComma ? currArg->right : NULL;
        }

    } break;

    case tkFunctionCall: {
        char buf[128] = {};
        char* bufp = buf;
        if (expr->left)
            analyseExpr(parser, expr->left, scope, mod, ownerFunc, true);

        ASTExpr* arg1 = expr->left;
        if (arg1 && arg1->kind == tkOpComma) arg1 = arg1->left;
        const char* typeName = ASTExpr_typeName(arg1);

        if (arg1) bufp += sprintf(bufp, "%s_", typeName);
        bufp += sprintf(bufp, "%s", expr->string);
        if (expr->left)
            ASTExpr_strarglabels(expr->left, bufp, 128 - ((int)(bufp - buf)));

        ASTFunc* found = ASTModule_getFunc(mod, buf);
        if (found) {
            expr->kind = tkFunctionCallResolved;
            expr->func = found;
            ASTFunc_analyse(parser, found, mod);
            analyseExpr(parser, expr, scope, mod, ownerFunc, inFuncArgs);
            return;
        }
        if (!strncmp(buf, "Void_", 5))
            Parser_errorCallingFuncWithVoid(parser, expr, arg1);
        else {
            Parser_errorUnrecognizedFunc(parser, expr, buf);

            if (*buf != '<') {
                int sugg = 0;
                foreach (ASTFunc*, func, mod->funcs) {
                    if (expr->string ^= func->name) {
                        eprintf("\e[36;1minfo:\e[0;2m   not viable: %s with %d "
                                "arguments at %s:%d\e[0m\n",
                            func->prettySelector, func->argCount,
                            parser->filename, func->line);
                        sugg++;
                    }
                    if (!func->intrinsic && strcasecmp(expr->string, func->name)
                        && leven(expr->string, func->name, expr->slen,
                               func->nameLen)
                            < 3
                        && func->argCount == PtrList_count(func->args)) {
                        eprintf("\e[36;1minfo:\e[0m did you mean: "
                                "\e[34m%s\e[0m (%s at "
                                "./%s:%d)\n",
                            func->name, func->prettySelector, parser->filename,
                            func->line);
                        sugg++;
                    }
                }
                if (sugg) eputs("-----x\n");
            }
        }
    } break;

    case tkVarAssign: {
        ASTExpr* const init = expr->var->init;
        ASTTypeSpec* const typeSpec = expr->var->typeSpec;

        if (typeSpec->typeType == TYUnresolved)
            resolveTypeSpec(parser, typeSpec, mod);

        if (!init) {

            if (!typeSpec->dims) switch (typeSpec->typeType) {
                case TYReal64:
                    expr->var->init = expr_const_0;
                    break;
                case TYString:
                    expr->var->init = expr_const_empty;
                    break;
                case TYBool:
                    expr->var->init = expr_const_no;
                    break;
                case TYObject:
                    if (!typeSpec->type->isEnum) {
                        expr->var->init = expr_const_nil;
                        break;
                    }
                default:
                errorMissingInit:

                    Parser_errorMissingInit(parser, expr);
                }
        } else {

            if (typeSpec->typeType == TYObject && typeSpec->type->isEnum)
                ASTExpr_setEnumBase(parser, init, typeSpec, mod);

            analyseExpr(parser, init, scope, mod, ownerFunc, false);

            expr->typeType = init->typeType;
            expr->collectionType = init->collectionType;
            expr->nullable = init->nullable;
            expr->elemental = init->elemental;
            expr->throws = init->throws;
            expr->impure = init->impure;
            expr->dims = init->dims;

            if (typeSpec->typeType == TYUnresolved) {
                typeSpec->typeType = init->typeType;
                if (init->typeType == TYObject) {
                    if (init->kind == tkFunctionCallResolved) {
                        expr->var->typeSpec = init->func->returnSpec;

                        typeSpec->type = init->func->returnSpec->type;
                        typeSpec->dims = init->func->returnSpec->dims;
                    } else if (init->kind == tkIdentifierResolved) {
                        expr->var->typeSpec = init->var->typeSpec;
                        typeSpec->type = init->var->typeSpec->type;
                        typeSpec->dims = init->var->typeSpec->dims;
                    } else if (init->kind == tkArrayOpen)
                        typeSpec->type = init->elementType;
                    else if (init->kind == tkBraceOpen)
                        typeSpec->type = init->elementType;
                    else if (init->kind == tkPeriod) {
                        ASTExpr* e = init;
                        if (init->left->var->typeSpec->type->isEnum) {
                            typeSpec->type = init->left->var->typeSpec->type;
                        } else {
                            while (e->kind == tkPeriod) e = e->right;

                            typeSpec->type = e->var->typeSpec->type;
                            typeSpec->dims = e->var->typeSpec->dims;
                        }

                    } else {
                        unreachable("%s", "var type inference failed");
                    }
                    analyseType(parser, typeSpec->type, mod);
                }
            } else if (typeSpec->typeType != init->typeType
                && init->typeType != TYAnyType) {

                Parser_errorInitMismatch(parser, expr);
                expr->typeType = TYErrorType;
            }

            if (typeSpec->dims == 0) {
                typeSpec->collectionType = init->collectionType;
                typeSpec->dims = init->collectionType == CTYTensor ? init->dims
                    : init->collectionType == CTYArray             ? 1
                                                                   : 0;
            } else if (typeSpec->dims != 1
                && init->collectionType == CTYArray) {
                Parser_errorInitDimsMismatch(parser, expr, 1);
                expr->typeType = TYErrorType;
            } else if (typeSpec->dims != 2
                && init->collectionType == CTYTensor) {
                Parser_errorInitDimsMismatch(parser, expr, 2);
                expr->typeType = TYErrorType;

            } else if (typeSpec->dims != 0 && init->collectionType == CTYNone) {
                Parser_errorInitDimsMismatch(parser, expr, 0);
                expr->typeType = TYErrorType;
            }
        }
    } break;

    case tkKeyword_match: {
        ASTExpr* cond = expr->left;
        if (cond) {
            analyseExpr(parser, cond, scope, mod, ownerFunc, false);
            ASTTypeSpec* tsp = ASTExpr_getObjectTypeSpec(cond);
            if (expr->body && tsp && tsp->type && tsp->type->isEnum) {
                foreach (ASTExpr*, cas, expr->body->stmts)
                    if (cas->left)
                        ASTExpr_setEnumBase(parser, cas->left, tsp, mod);
            }
        }

        foreach (ASTExpr*, stmt, expr->body->stmts) {
            analyseExpr(parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
            if (cond && stmt->kind == tkKeyword_case && stmt->left
                && (stmt->left->typeType != cond->typeType
                    || (cond->typeType == TYObject
                        && ASTExpr_getTypeOrEnum(stmt->left)
                            != ASTExpr_getTypeOrEnum(cond))))
                Parser_errorTypeMismatch(parser, cond, stmt->left);
        }
    } break;
    case tkKeyword_else:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_elif:
    case tkKeyword_while:
    case tkKeyword_case: {
        if (expr->left)
            analyseExpr(parser, expr->left, scope, mod, ownerFunc, false);
        foreach (ASTExpr*, stmt, expr->body->stmts)
            analyseExpr(parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
    } break;

    case tkSubscriptResolved: {

        int nhave = ASTExpr_countCommaList(expr->left);
        if (nhave != expr->var->typeSpec->dims)
            Parser_errorIndexDimsMismatch(parser, expr, nhave);
    }

    case tkSubscript:
        if (expr->left)
            analyseExpr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
        if (expr->kind == tkSubscriptResolved) {
            expr->typeType = expr->var->typeSpec->typeType;
            expr->collectionType = expr->var->typeSpec->collectionType;

            expr->elemental = expr->left ? expr->left->elemental : true;
        }
        break;

    case tkString:
    case tkRawString:
        expr->typeType = TYString;
        ASTExpr_prepareInterp(parser, expr, scope);
        break;

    case tkNumber:
        expr->typeType = TYReal64;
        break;

    case tkKeyword_yes:
    case tkKeyword_no:
        expr->typeType = TYBool;
        break;

    case tkKeyword_nil:
        expr->typeType = TYAnyType;
        break;

    case tkIdentifier:

        expr->typeType = TYErrorType;
        break;

    case tkIdentifierResolved:
        expr->typeType = expr->var->typeSpec->typeType;
        expr->collectionType = expr->var->typeSpec->collectionType;
        expr->elemental = expr->collectionType != CTYNone;
        expr->dims = expr->var->typeSpec->dims;

        if (expr->typeType == TYObject)
            analyseType(parser, expr->var->typeSpec->type, mod);
        break;

    case tkArrayOpen:
        expr->collectionType = CTYArray;
        if (expr->right) {
            analyseExpr(parser, expr->right, scope, mod, ownerFunc, inFuncArgs);
            expr->typeType = expr->right->typeType;
            expr->collectionType
                = expr->right->kind == tkOpSemiColon ? CTYTensor : CTYArray;
            expr->dims = expr->right->kind == tkOpSemiColon ? 2 : 1;

            if (expr->typeType == TYObject) {

                ASTExpr* first = expr->right;
                while (first->kind == tkOpComma || first->kind == tkOpSemiColon)
                    first = first->left;
                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->typeSpec->type;
                    if (first->var->typeSpec->dims
                        || first->var->typeSpec->collectionType != CTYNone)
                        unreachable(
                            "trying to make array of arrays %d", expr->line);
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    if (first->func->returnSpec->dims
                        || first->var->typeSpec->collectionType != CTYNone)
                        unreachable("trying to make array of arrays line %d",
                            expr->line);
                    break;
                default:
                    break;
                }
            }
        }
        break;

    case tkBraceOpen:
        if (expr->right) {
            analyseExpr(parser, expr->right, scope, mod, ownerFunc, true);

            analyseDictLiteral(parser, expr->right, mod);
            expr->typeType = expr->right->typeType;
            if (expr->typeType == TYObject) {

                ASTExpr* first = expr->right;
                while (first->kind == tkOpComma) first = first->left;
                if (first->kind == tkOpAssign) first = first->right;

                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->typeSpec->type;
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    break;
                default:
                    break;
                }
            }
            expr->collectionType = CTYDictS;
        }
        break;
    case tkPeriod: {

        if (!expr->left) {
            Parser_errorNoEnumInferred(parser, expr->right);
            break;
        }

        assert(expr->left->kind == tkIdentifierResolved
            || expr->left->kind == tkIdentifier);
        analyseExpr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);

        if (!expr->left->typeType) break;

        ASTExpr* member = expr->right;
        if (member->kind == tkPeriod) {
            member = member->left;
            if (member->kind != tkIdentifier) {
                Parser_errorUnexpectedExpr(parser, member);
                break;
            }
        }

        if (member->kind != tkIdentifier && member->kind != tkSubscript) {
            Parser_errorUnexpectedExpr(parser, member);
            break;
        }

        if (member->kind != tkIdentifier)
            analyseExpr(
                parser, member->left, scope, mod, ownerFunc, inFuncArgs);

        if (expr->left->kind != tkIdentifierResolved) break;

        if (expr->left->var->typeSpec->typeType == TYErrorType) {
            expr->typeType = TYErrorType;
            break;
        }

        ASTType* type = expr->left->var->typeSpec->type;
        if (!type) {
            expr->typeType = TYErrorType;
            break;
        }

        resolveMember(parser, member, type);

        if (member->kind != tkIdentifierResolved) {
            expr->typeType = TYErrorType;
            break;
        }
        analyseExpr(parser, member, scope, mod, ownerFunc, inFuncArgs);

        if (expr->right->kind == tkPeriod)
            analyseExpr(parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

        expr->typeType = type->isEnum ? TYObject : expr->right->typeType;
        expr->collectionType = expr->right->collectionType;
        expr->elemental = expr->right->elemental;
        expr->dims = expr->right->dims;
    } break;

    case tkLineComment:
        break;

    case tkArgumentLabel:

        break;

    default:
        if (expr->prec) {
            if (!expr->unary && expr->left)
                analyseExpr(
                    parser, expr->left, scope, mod, ownerFunc, inFuncArgs);

            if (expr->right)
                analyseExpr(
                    parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

            if (expr->kind == tkKeyword_or && expr->left->typeType != TYBool) {

                ;
            } else if (isCmpOp(expr) || isBoolOp(expr)
                || expr->kind == tkKeyword_in) {

                expr->typeType
                    = (expr->right->typeType == TYErrorType
                          || (!expr->unary
                              && expr->left->typeType == TYErrorType))
                    ? TYErrorType
                    : TYBool;
            } else {

                if (expr->right) {
                    expr->typeType = expr->right->typeType;
                    expr->collectionType = expr->right->collectionType;
                }
            }
            if (expr->right)
                expr->elemental
                    = expr->right->elemental || expr->kind == tkOpColon;

            if (!expr->unary && expr->left) {
                expr->elemental = expr->elemental || expr->left->elemental;
                expr->dims = expr->right->dims;

                if (expr->dims != expr->left->dims
                    && !(inFuncArgs
                        && (expr->kind == tkOpComma
                            || expr->kind == tkOpAssign))) {

                    if (expr->left->dims != 0 && expr->right->dims != 0) {
                        Parser_errorBinOpDimsMismatch(parser, expr);

                        expr->right->typeType = TYErrorType;
                    } else if (expr->kind == tkPlus || expr->kind == tkMinus
                        || expr->kind == tkTimes || expr->kind == tkSlash
                        || expr->kind == tkPower || expr->kind == tkOpMod) {
                        expr->dims = expr->left->dims + expr->right->dims;
                        expr->collectionType = max(expr->left->collectionType,
                            expr->right->collectionType);

                    } else if (expr->kind == tkKeyword_in
                        && expr->left->dims == 0 && expr->right->dims == 1) {

                    } else if (expr->kind == tkOpColon && expr->left->dims == 1
                        && expr->left->kind == tkOpColon
                        && expr->right->dims == 0) {

                    } else {

                        Parser_errorBinOpDimsMismatch(parser, expr);
                    }

                } else {
                }

                if (expr->kind == tkOpColon) expr->dims = 1;
            }
            if (!expr->unary && expr->left
                && !(inFuncArgs
                    && (expr->kind == tkOpComma || expr->kind == tkOpAssign))) {

                TypeTypes leftType = expr->left->typeType;
                TypeTypes rightType = expr->right->typeType;

                if (leftType == TYBool
                    && (expr->kind == tkOpLE || expr->kind == tkOpLT)) {

                    ;
                } else if (leftType != rightType) {

                    Parser_errorTypeMismatchBinOp(parser, expr);
                    expr->typeType = TYErrorType;
                } else if (leftType == TYString
                    && (expr->kind == tkOpAssign || expr->kind == tkOpEQ
                        || expr->kind == tkOpNE)) {

                    ;
                } else if (leftType == TYBool
                    && (expr->kind == tkOpAssign || expr->kind == tkOpEQ
                        || expr->kind == tkKeyword_and
                        || expr->kind == tkKeyword_or
                        || expr->kind == tkKeyword_not
                        || expr->kind == tkKeyword_notin))
                    ;
                else if (isArithOp(expr)
                    && (!TypeType_isnum(leftType)
                        || !TypeType_isnum(rightType))) {

                    Parser_errorInvalidTypeForOp(parser, expr);
                }

                if (leftType == TYErrorType) expr->typeType = leftType;
            }

        } else {
            unreachable("unknown expr kind: %s", TokenKind_str[expr->kind]);
        }
    }
}

static void analyseType(Parser* parser, ASTType* type, ASTModule* mod) {
    if (type->analysed) return;

    if (type->super) {
        resolveTypeSpec(parser, type->super, mod);
        if (type->super->type == type)
            Parser_errorTypeInheritsSelf(parser, type);
    }

    foreach (ASTType*, type2, mod->types) {
        if (type2 == type) break;
        if (type->name ^= type2->name)
            Parser_errorDuplicateType(parser, type, type2);
    }

    type->analysed = true;

    if (type->body) foreach (ASTExpr*, stmt, type->body->stmts)
            analyseExpr(parser, stmt, type->body, mod, NULL, false);
}
static void ASTFunc_hashExprs(/* Parser* parser,  */ ASTFunc* func);

static void ASTFunc_analyse(Parser* parser, ASTFunc* func, ASTModule* mod) {
    if (func->analysed) return;

    bool isCtor = false;

    foreach (ASTType*, type, mod->types) {
        if (func->name ^= type->name) {
            if (func->returnSpec && !(func->isStmt || func->isDefCtor))
                Parser_errorCtorHasType(parser, func, type);
            if (!func->returnSpec) {
                func->returnSpec = ASTTypeSpec_new(TYObject, CTYNone);
                func->returnSpec->type = type;

                func->returnsNewObjectAlways = true;
            }

            if (!isupper(*func->name)) Parser_warnCtorCase(parser, func);

            func->name = type->name;
            isCtor = true;
        }
    }

    if (!func->isDefCtor && !isCtor && isupper(*func->name))
        Parser_errorUnrecognizedCtor(parser, func);

    if (!func->body) {
        func->analysed = true;
        return;
    }

    foreach (ASTFunc*, func2, mod->funcs) {
        if (func == func2) break;
        if (func->selector ^= func2->selector)
            Parser_errorDuplicateFunc(parser, func, func2);
    }

    ASTFunc_checkUnusedVars(parser, func);

    func->analysed = true;

    foreach (ASTExpr*, stmt, func->body->stmts)
        analyseExpr(parser, stmt, func->body, mod, NULL, false);

    if (func->isStmt) setStmtFuncTypeInfo(parser, func);

    if (!parser->issues.errCount && parser->mode != PMLint) {

        ASTFunc_hashExprs(func);

        ASTScope_lowerElementalOps(func->body);

        ASTScope_promoteCandidates(func->body);
    }
}

static void analyseTest(Parser* parser, ASTTest* test, ASTModule* mod) {
    if (!test->body) return;

    foreach (ASTTest*, test2, mod->tests) {
        if (test == test2) break;
        if (test->name ^= test2->name)
            Parser_errorDuplicateTest(parser, test, test2);
    }

    ASTTest_checkUnusedVars(parser, test);

    foreach (ASTExpr*, stmt, test->body->stmts)
        analyseExpr(parser, stmt, test->body, mod, NULL, false);

    if (!parser->issues.errCount && parser->mode != PMLint) {
        ASTScope_lowerElementalOps(test->body);
        ASTScope_promoteCandidates(test->body);
    }
}

static void ASTModule_unmarkTypesVisited(ASTModule* mod);
static int ASTExpr_markTypesVisited(Parser* parser, ASTExpr* expr);
static int ASTType_checkCycles(Parser* parser, ASTType* type);

void analyseModule(Parser* parser, ASTModule* mod) {

    foreach (ASTFunc*, func, mod->funcs) {
        foreach (ASTVar*, arg, func->args)
            resolveTypeSpec(parser, arg->typeSpec, mod);
        if (func->returnSpec) resolveTypeSpec(parser, func->returnSpec, mod);
        getSelector(func);
    }

    ASTFunc* fstart = NULL;

    foreach (ASTFunc*, func, mod->funcs)
        if (!strcmp(func->name, "start")) fstart = func;

    if (parser->mode == PMGenTests || parser->mode == PMLint) {
        foreach (ASTExpr*, stmt, mod->scope->stmts)
            analyseExpr(parser, stmt, mod->scope, mod, NULL, false);

        foreach (ASTTest*, test, mod->tests)
            analyseTest(parser, test, mod);
        foreach (ASTFunc*, func, mod->funcs)
            ASTFunc_analyse(parser, func, mod);
        foreach (ASTType*, type, mod->types)
            analyseType(parser, type, mod);
        foreach (ASTType*, en, mod->enums)
            analyseType(parser, en, mod);

    } else if (fstart) {
        /* TODO: what if you have tests and a start()? Now you will have to
         analyse the tests anyway */
        foreach (ASTExpr*, stmt, mod->scope->stmts)
            analyseExpr(parser, stmt, mod->scope, mod, NULL, false);

        ASTFunc_analyse(parser, fstart, mod);

        foreach (ASTFunc*, func, mod->funcs)
            if (!func->analysed && !func->isDefCtor)
                Parser_warnUnusedFunc(parser, func);
        foreach (ASTType*, type, mod->types)
            if (!type->analysed) Parser_warnUnusedType(parser, type);

    } else {
        eputs("\n\e[31m*** error:\e[0m cannot find function "
              "\e[33mstart\e[0m.\n");
        parser->issues.errCount++;
    }

    foreach (ASTType*, type, mod->types)
        if (type->analysed && type->body && !type->visited) {
            if (ASTType_checkCycles(parser, type)) {

                eprintf(" ...%s\n", "\e[0m");

            } else
                ASTModule_unmarkTypesVisited(mod);
        }
}

static int ASTType_checkCycles(Parser* parser, ASTType* type) {
    foreach (ASTExpr*, stmt, type->body->stmts)
        if (ASTExpr_markTypesVisited(parser, stmt)) {
            eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
                type->name, parser->filename, stmt->line, stmt->col);
            return -1;
        }
    return 0;
}

static int ASTExpr_markTypesVisited(Parser* parser, ASTExpr* expr) {
    ASTType* type = NULL;
    if (!expr) return 0;
    switch (expr->kind) {
    case tkVarAssign:
        return ASTExpr_markTypesVisited(parser, expr->var->init);
    case tkFunctionCall:
        return ASTExpr_markTypesVisited(parser, expr->left);
    case tkFunctionCallResolved:
        if (ASTExpr_markTypesVisited(parser, expr->left)) return -1;

        if (expr->func->returnSpec->typeType == TYObject
            && expr->func->returnsNewObjectAlways)
            type = expr->func->returnSpec->type;
        break;
    case tkSubscript:
    case tkSubscriptResolved:
        return ASTExpr_markTypesVisited(parser, expr->left);
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
            if (!expr->unary)
                ret += ASTExpr_markTypesVisited(parser, expr->left);
            ret += ASTExpr_markTypesVisited(parser, expr->right);
            if (ret) return ret;
        } else
            unreachable("unknown expr kind: %s at %d:%d\n",
                TokenKind_str[expr->kind], expr->line, expr->col);
    }
    if (!type) return 0;
    if (type->visited) {
        Parser_errorConstructorHasCycle(parser, type);
        eprintf("%s", "\e[;2m");
        return -1;
    }
    type->visited = true;
    return ASTType_checkCycles(parser, type);
}

static void ASTModule_unmarkTypesVisited(ASTModule* mod) {

    foreach (ASTType*, type, mod->types)
        type->visited = false;
}

#include "cse.h"
