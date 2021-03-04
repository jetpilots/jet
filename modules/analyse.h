
static void setStmtFuncTypeInfo(Parser* parser, ASTFunc* func) {
    // this assumes that setExprTypeInfo has been called on the func body
    const ASTExpr* stmt = func->body->stmts->item;
    if (not func->returnSpec->typeType)
        func->returnSpec->typeType = stmt->typeType;
    else if (func->returnSpec->typeType != stmt->typeType)
        Parser_errorTypeMismatchBinOp(parser, stmt);
}

// TODO make sempassModule -> same as analyseModule now
static void analyseType(Parser* parser, ASTType* type, ASTModule* mod);
static void ASTFunc_analyse(Parser* parser, ASTFunc* func, ASTModule* mod);

///////////////////////////////////////////////////////////////////////////
static bool isCmpOp(ASTExpr* expr) {
    return expr->kind == tkOpLE //
        or expr->kind == tkOpLT //
        or expr->kind == tkOpGT //
        or expr->kind == tkOpGE //
        or expr->kind == tkOpEQ //
        or expr->kind == tkOpNE;
}

///////////////////////////////////////////////////////////////////////////
static bool isBoolOp(ASTExpr* expr) {
    return expr->kind == tkKeyword_and //
        or expr->kind == tkKeyword_or //
        or expr->kind == tkKeyword_not;
}

///////////////////////////////////////////////////////////////////////////
static void analyseDictLiteral(Parser* parser, ASTExpr* expr, ASTModule* mod) {
    // check that
    // - dict keys in the dict literal are of the same type.
    // - values are of the same type.
    // - all exprs within the commas are tkOpAssign.
    // assign the typeSpec pair somehow to the tkDictLiteral expr.
    // maybe alloc twice the size and set the pointer so you can access
    // spec[0] and spec[1].
}

///////////////////////////////////////////////////////////////////////////
static void analyseExpr(
    Parser* parser, ASTExpr* expr, ASTModule* mod, bool inFuncArgs) {
    switch (expr->kind) {

        // -------------------------------------------------- //
    case tkFunctionCallResolved: {

        if (ASTExpr_countCommaList(expr->left) != expr->func->argCount)
            Parser_errorArgsCountMismatch(parser, expr);
        expr->typeType = expr->func->returnSpec
            ? expr->func->returnSpec->typeType
            : TYNoType; // should actually be TYVoid
        expr->collectionType = expr->func->returnSpec
            ? expr->func->returnSpec->collectionType
            : CTYNone; // should actually be TYVoid
        expr->elemental = expr->func->elemental;
        if (expr->func->returnSpec) expr->dims = expr->func->returnSpec->dims;
        // isElementalFunc means the func is only defined for (all)
        // Number arguments and another definition for vector args
        // doesn't exist. Basically during typecheck this should see
        // if a type mismatch is only in terms of collectionType.
        if (not expr->left) break;
        expr->elemental = expr->elemental and expr->left->elemental;
        expr->throws = expr->left->throws or expr->func->throws;
        ASTExpr* currArg = expr->left;
        foreach (ASTVar*, arg, expr->func->args) {
            ASTExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) cArg = cArg->right;
            if (cArg->typeType != arg->typeSpec->typeType)
                Parser_errorArgTypeMismatch(parser, cArg, arg);
            // TODO: check dims mismatch
            // TODO: check units mismatch
            if (not(currArg = currArg->right)) break;
        }

        currArg = expr->left;
        while (currArg) {
            ASTExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) {
                // LHS will be a tkIdentifier. You should resolve it to one
                // of the function's arguments and set it to tkArgumentLabel.
                assert(cArg->left->kind == tkIdentifier);
                ASTVar* theArg = NULL;
                foreach (ASTVar*, arg, expr->func->args) {
                    if (!strcasecmp(cArg->left->string, arg->name))
                        theArg = arg;
                }
                if (!theArg) {
                    unreachable("unresolved argument %s!", cArg->left->string);
                    // cArg->left->kind = tkIdentifier;
                    // change it back to identifier
                } // TODO: change this to parser error
                else {
                    cArg->left->var = theArg;
                    cArg->left->kind = tkIdentifierResolved;
                }
            }

            currArg = currArg->kind == tkOpComma ? currArg->right : NULL;
        }

    } break;

        // -------------------------------------------------- //
    case tkFunctionCall: {
        char buf[128] = {};
        char* bufp = buf;
        if (expr->left) analyseExpr(parser, expr->left, mod, true);

        // TODO: need a function to make and return selector
        ASTExpr* arg1 = expr->left;
        if (arg1 and arg1->kind == tkOpComma) arg1 = arg1->left;
        const char* typeName = ASTExpr_typeName(arg1);
        //        const char* collName = "";
        //        if (arg1)
        //        collName=CollectionType_nativeName(arg1->collectionType);
        if (arg1) bufp += sprintf(bufp, "%s_", typeName);
        bufp += sprintf(bufp, "%s", expr->string);
        if (expr->left)
            ASTExpr_strarglabels(expr->left, bufp, 128 - ((int)(bufp - buf)));

        ASTFunc* found = ASTModule_getFunc(mod, buf);
        if (found) {
            expr->kind = tkFunctionCallResolved;
            expr->func = found;
            ASTFunc_analyse(parser, found, mod);
            analyseExpr(parser, expr, mod, inFuncArgs);
            return;
        }
        if (not strncmp(buf, "Void_", 5))
            Parser_errorCallingFuncWithVoid(parser, expr, arg1);
        else {
            Parser_errorUnrecognizedFunc(parser, expr, buf);

            if (*buf != '<') // not invalid type
                foreach (ASTFunc*, func, mod->funcs) {
                    if (not strcasecmp(expr->string, func->name))
                        eprintf("\e[;2m ./%s:%d: \e[;1;2m%s\e[0;2m with %d "
                                "arguments is not viable.\e[0m\n",
                            parser->filename, func->line, func->selector,
                            func->argCount);
                    if (not func->intrinsic
                        and leven(expr->string, func->name, expr->slen,
                                func->nameLen)
                            < 3
                        and func->argCount == PtrList_count(func->args))
                        eprintf(" Did you mean: \e[;1m%s\e[0m (%s at "
                                "./%s:%d)\n",
                            func->name, func->selector, parser->filename,
                            func->line);
                }
        }
    } break;

        // -------------------------------------------------- //
    case tkVarAssign: {
        ASTExpr* const init = expr->var->init;
        ASTTypeSpec* const typeSpec = expr->var->typeSpec;

        if (not init)
            Parser_errorMissingInit(parser, expr);
        else {
            if (typeSpec->typeType == TYUnresolved)
                resolveTypeSpec(parser, typeSpec, mod);

            analyseExpr(parser, init, mod, false);

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
                        // ^ TODO: DROP old expr->var->typeSpec!!
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
                        while (e->kind == tkPeriod) e = e->right;
                        // at this point, it must be a resolved ident or
                        // subscript
                        typeSpec->type = e->var->typeSpec->type;
                        typeSpec->dims = e->var->typeSpec->dims;

                    } else {
                        unreachable("%s", "var type inference failed");
                    }
                    analyseType(parser, typeSpec->type, mod);
                }
            } else if (typeSpec->typeType != init->typeType) {
                Parser_errorInitMismatch(parser, expr);
                expr->typeType = TYErrorType;
            }

            if (typeSpec->dims == 0) {
                typeSpec->collectionType = init->collectionType;
                typeSpec->dims = init->collectionType == CTYTensor ? init->dims
                    : init->collectionType == CTYArray             ? 1
                                                                   : 0;
            } else if (typeSpec->dims != 1
                and init->collectionType == CTYArray) {
                Parser_errorInitDimsMismatch(parser, expr, 1);
                expr->typeType = TYErrorType;
            } else if (typeSpec->dims != 2
                and init->collectionType == CTYTensor) {
                Parser_errorInitDimsMismatch(parser, expr, 2);
                expr->typeType = TYErrorType;

            } else if (typeSpec->dims != 0
                and init->collectionType == CTYNone) {
                Parser_errorInitDimsMismatch(parser, expr, 0);
                expr->typeType = TYErrorType;
            }
        }
    } break;

        // -------------------------------------------------- //
    case tkKeyword_else:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_elif:
    case tkKeyword_while: {
        if (expr->left) analyseExpr(parser, expr->left, mod, false);
        foreach (ASTExpr*, stmt, expr->body->stmts)
            analyseExpr(parser, stmt, mod, inFuncArgs);
    } break;

        // -------------------------------------------------- //
    case tkSubscriptResolved: {
        // assert(expr->left->kind == tkArrayOpen);
        int nhave = ASTExpr_countCommaList(expr->left);
        if (nhave != expr->var->typeSpec->dims)
            Parser_errorIndexDimsMismatch(parser, expr, nhave);
    }
        // fallthru
    case tkSubscript:
        if (expr->left) analyseExpr(parser, expr->left, mod, inFuncArgs);
        if (expr->kind == tkSubscriptResolved) {
            expr->typeType = expr->var->typeSpec->typeType;
            expr->collectionType = expr->var->typeSpec->collectionType;
            // TODO: since it is a subscript, if it has no left (i.e. arr[])
            // i'm guessing it is a full array, and therefore can be an
            // elemental op
            expr->elemental = expr->left ? expr->left->elemental : true;
            // TODO: check args in the same way as for funcs below, not
            // directly checking expr->left.}
            // TODO: dims is actually a bit complicated here. For each dim
            // indexed with a scalar (not a range), you reduce the dim of the
            // subscript result by 1. For now the default is to keep the dims at
            // 0, which is what it would be if you indexed something with a
            // scalar index in each dimension.
            // analyseExpr for a : op should set dims to 1. Then you just walk
            // the comma op in expr->right here and check which index exprs have
            // dims=0.
            // Special case is when the index expr is a logical, meaning you are
            // filtering the array, in this case the result's dims is always 1.
        }
        break;

    case tkString:
    case tkRawString:
        expr->typeType = TYString;
        break;

    case tkNumber:
        expr->typeType = TYReal64;
        break;

    case tkIdentifier:
        expr->typeType = TYErrorType;
        break;

    case tkIdentifierResolved:
        expr->typeType = expr->var->typeSpec->typeType;
        expr->collectionType = expr->var->typeSpec->collectionType;
        expr->elemental = expr->collectionType != CTYNone;
        expr->dims = expr->var->typeSpec->dims;
        break;

    case tkArrayOpen:
        expr->collectionType = CTYArray;
        if (expr->right) {
            analyseExpr(parser, expr->right, mod, inFuncArgs);
            expr->typeType = expr->right->typeType;
            expr->collectionType
                = expr->right->kind == tkOpSemiColon ? CTYTensor : CTYArray;
            expr->dims = expr->right->kind == tkOpSemiColon ? 2 : 1;
            // using array literals you can only init 1D or 2D
            if (expr->typeType == TYObject) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                ASTExpr* first = expr->right;
                while (first->kind == tkOpComma or first->kind == tkOpSemiColon)
                    first = first->left;
                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->typeSpec->type;
                    if (first->var->typeSpec->dims
                        or first->var->typeSpec->collectionType != CTYNone)
                        unreachable(
                            "trying to make array of arrays %d", expr->line);
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    if (first->func->returnSpec->dims
                        or first->var->typeSpec->collectionType != CTYNone)
                        unreachable("trying to make array of arrays line %d",
                            expr->line);
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tkObjectInitResolved:
                    // expr->elementType = first->var->typeSpec->type;break;
                }
                // expr->elementType =
            }
        }
        break;

    case tkBraceOpen:
        if (expr->right) {
            analyseExpr(parser, expr->right, mod, true);
            // TODO: you told analyseExpr to not care about what's on the LHS of
            // tkOpAssign exprs. Now you handle it yourself. Ensure that they're
            // all of the same type and set that type to the expr somehow.
            analyseDictLiteral(parser, expr->right, mod);
            expr->typeType = expr->right->typeType;
            if (expr->typeType == TYObject) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                ASTExpr* first = expr->right;
                while (first->kind == tkOpComma) first = first->left;
                if (first->kind == tkOpAssign) first = first->right;
                // we care about the value in the key-value pair. We'll figure
                // out the key type later or not, whatever.
                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->typeSpec->type;
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tkObjectInitResolved:
                    // expr->elementType = first->var->typeSpec->type;break;
                }
            }
            expr->collectionType = CTYDictS;
            // these are only Dicts! Sets are normal [] when you detect they are
            // only used for querying membership.
            // TODO: what were the gazillion Dict subtypes for?
        }
        break;
    case tkPeriod: {
        assert(expr->left->kind == tkIdentifierResolved
            or expr->left->kind == tkIdentifier);
        analyseExpr(parser, expr->left, mod, inFuncArgs);

        // The name/type resolution of expr->left may have failed.
        if (not expr->left->typeType) break;

        ASTExpr* member = expr->right;
        if (member->kind == tkPeriod) {
            member = member->left;
            if (member->kind != tkIdentifier) {
                Parser_errorUnexpectedExpr(parser, member);
                break;
            }
        }

        if (member->kind != tkIdentifier and member->kind != tkSubscript) {
            Parser_errorUnexpectedExpr(parser, member);
            break;
        }
        //  or member->kind == tkFunctionCall);
        if (member->kind != tkIdentifier)
            analyseExpr(parser, member->left, mod, inFuncArgs);

        // the left must be a resolved ident
        if (expr->left->kind != tkIdentifierResolved) break;

        ASTType* type = expr->left->var->typeSpec->type;
        // Resolve the member in the scope of the type definition.
        resolveMember(parser, member, type);
        // Name resolution may fail...
        if (member->kind != tkIdentifierResolved) {
            expr->typeType = TYErrorType;
            break;
        }
        analyseExpr(parser, member, mod, inFuncArgs);

        if (expr->right->kind == tkPeriod)
            analyseExpr(parser, expr->right, mod, inFuncArgs);

        expr->typeType = expr->right->typeType;
        expr->collectionType = expr->right->collectionType;
        expr->elemental = expr->right->elemental;
        expr->dims = expr->right->dims;
    } break;

    case tkLineComment:
        break;

    case tkArgumentLabel:

        break;

        //    case tkRawString:
        // TODO: analyse regex, compile it already, whatever
        //        break;
        // -------------------------------------------------- //
    default:
        if (expr->prec) {
            if (not expr->unary)
                analyseExpr(parser, expr->left, mod, inFuncArgs);
            // some exprs like return can be used without any args
            if (expr->right) analyseExpr(parser, expr->right, mod, inFuncArgs);

            if (expr->kind == tkKeyword_or and expr->left->typeType != TYBool) {
                // Handle the 'or' keyword used to provide alternatives for
                // a nullable expression.
                ;
            } else if (isCmpOp(expr) or isBoolOp(expr)
                or expr->kind == tkKeyword_in) {
                // Handle comparison and logical operators (always return a
                // bool)
                expr->typeType = (expr->left->typeType == TYErrorType
                                     or expr->right->typeType == TYErrorType)
                    ? TYErrorType
                    : TYBool;
            } else {
                // Set the type from the ->right expr for now. if an error
                // type is on the right, this is accounted for.
                if (expr->right) {
                    expr->typeType = expr->right->typeType;
                    expr->collectionType = expr->right->collectionType;
                }
            }
            if (expr->right)
                expr->elemental
                    = expr->right->elemental or expr->kind == tkOpColon;
            // TODO: actually, indexing by an array of integers is also an
            // indication of an elemental op

            if (not expr->unary) {
                expr->elemental = expr->elemental or expr->left->elemental;
                expr->dims = expr->right->dims;
                // if (expr->kind == tkOpColon and expr->right->kind ==
                // tkOpColon)
                //     expr->right->dims = 0; // temporarily set it to 0 to
                //     allow
                // parsing stepped ranges 1:s:n

                if (expr->dims != expr->left->dims
                    and not(inFuncArgs
                        and (expr->kind == tkOpComma
                            or expr->kind == tkOpAssign))) {
                    // if either one has 0 dims (scalar) it is an elemental op
                    // with a scalar.
                    if (expr->left->dims != 0 and expr->right->dims != 0) {
                        Parser_errorBinOpDimsMismatch(parser, expr);
                        //                         unreachable("TODO: make
                        //                         error: dims mismatch at line
                        //                         "
                        //         "%d col %d\n    %d vs %d\n",
                        // expr->line, expr->col, expr->left->dims,
                        // expr->right->dims);
                        expr->right->typeType = TYErrorType;
                    } else if (expr->kind == tkPlus or expr->kind == tkMinus
                        or expr->kind == tkTimes or expr->kind == tkSlash
                        or expr->kind == tkPower or expr->kind == tkOpMod) {
                        expr->dims = expr->left->dims + expr->right->dims;
                        expr->collectionType = max(expr->left->collectionType,
                            expr->right->collectionType);
                        // todo: stop distinguishing array and tensor!!! then
                        // you dont need this. this strongly depends on the op &
                        // is too much repeated work
                        // eprintf("ok `[+-*/^%]` dims@ %d %d %d %d\n",
                        // expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                    } else if (expr->kind == tkKeyword_in
                        and expr->left->dims == 0 and expr->right->dims == 1) {
                        // eprintf("ok `in` dims@ %d %d %d %d\n", expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                    } else if (expr->kind == tkOpColon //
                        and expr->left->dims == 1
                        and expr->left->kind == tkOpColon
                        and expr->right->dims == 0) {
                        // eprintf("ok `:` dims@ %d %d %d %d\n", expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                        // expr->dims = 1;
                    } else {
                        // unreachable(
                        //     "TODO: make error: dims mismatch for op '%s' line
                        //     "
                        //     "%d col %d\n    %d vs %d\n",
                        //     tkrepr[expr->kind], expr->line, expr->col,
                        //     expr->left->dims, expr->right->dims);
                        Parser_errorBinOpDimsMismatch(parser, expr);
                    }

                    // ^ TODO: but you can also have some ops on 2D and 1D
                    // operands e.g. linear solve. what about those?
                } else {
                    // eprintf("(ignore) dims@ %d %d %d %d\n", expr->line,
                    //     expr->col, expr->left->dims, expr->right->dims);
                }
                // ranges always create a 1D entity (not always array, but 1D)
                if (expr->kind == tkOpColon) expr->dims = 1;
            }
            if (not expr->unary
                and not(inFuncArgs
                    and (expr->kind == tkOpComma
                        or expr->kind == tkOpAssign))) {
                // ignore , and = inside function call arguments. thing is
                // array or dict literals passed as args will have , and =
                // which should be checked. so when you descend into their
                // args, unset inFuncArgs.
                TypeTypes leftType = expr->left->typeType;
                TypeTypes rightType = expr->right->typeType;

                if (leftType == TYBool
                    and (expr->kind == tkOpLE or expr->kind == tkOpLT)) {
                    // Special case: chained LE/LT operators: e.g. 0 <= yCH4
                    // <= 1.
                    ;
                } else if (leftType != rightType) {
                    // Type mismatch for left and right operands is always
                    // an error.
                    Parser_errorTypeMismatchBinOp(parser, expr);
                    expr->typeType = TYErrorType;
                } else if (leftType == TYString
                    and (expr->kind == tkOpAssign or expr->kind == tkOpEQ
                        or expr->kind == tkOpNE)) {
                    // Allow assignment, equality test and != for strings.
                    // TODO: might even allow comparison operators, and
                    // perhaps allow +=, or better .= or something. Or
                    // perhaps append(x!) is clearer
                    ;
                } else if (leftType == TYBool
                    and (expr->kind == tkOpAssign //
                        or expr->kind == tkOpEQ //
                        or expr->kind == tkKeyword_and //
                        or expr->kind == tkKeyword_or))
                    ;
                else if (not TypeType_isnum(leftType)) {
                    // Arithmetic operators are only relevant for numeric
                    // types.
                    Parser_errorInvalidTypeForOp(parser, expr);
                }
                // check if an error type is on the left, if yes, set the
                // expr type
                if (leftType == TYErrorType) expr->typeType = leftType;
            }
            // TODO: here statements like return etc. that are not binary
            // but need to have their types checked w.r.t. an expected type
            // TODO: some ops have a predefined type e.g. : is of type Range
            // etc,
        } else {
            unreachable("unknown expr kind: %s", TokenKind_str[expr->kind]);
        }
    }
}

///////////////////////////////////////////////////////////////////////////
static void analyseType(Parser* parser, ASTType* type, ASTModule* mod) {
    if (type->analysed) return;
    // eprintf(
    //     "analyseExpr: %s at ./%s:%d\n", type->name, parser->filename,
    //     type->line);
    if (type->super) {
        resolveTypeSpec(parser, type->super, mod);
        if (type->super->type == type)
            Parser_errorTypeInheritsSelf(parser, type);
    }
    // TODO: this should be replaced by a dict query
    foreach (ASTType*, type2, mod->types) {
        if (type2 == type) break;
        if (not strcasecmp(type->name, type2->name))
            Parser_errorDuplicateType(parser, type, type2);
    }
    // Mark the semantic pass as done for this type, so that recursive
    // paths through calls found in initializers will not cause the compiler
    // to recur. This might be a problem if e.g. the type has a, b, c and
    // the initializer for b has a dependency on the type's .c member, whose
    // type has not been set by the time b's initializer is processed.
    // However if you set analysed after all statements, then you risk
    // getting caught in a recursive path. One way to fix it is to have
    // granularity at the member level, so not entire types but their
    // individual members are processed. In that case the only problem can
    // be a recursive path between a member var and a function that it calls
    // in its initializer.
    type->analysed = true;
    // nothing to do for declared/empty types etc. with no body
    if (type->body) //
        foreach (ASTExpr*, stmt, type->body->stmts)
            analyseExpr(parser, stmt, mod, false);
}
static void ASTFunc_hashExprs(/* Parser* parser,  */ ASTFunc* func);

///////////////////////////////////////////////////////////////////////////
static void ASTFunc_analyse(Parser* parser, ASTFunc* func, ASTModule* mod) {
    if (func->analysed) return;
    // eprintf("analyseExpr: %s at ./%s:%d\n", func->selector,
    // parser->filename, func->line);

    bool isCtor = false;
    // Check if the function is a constructor call and identify the type.
    // TODO: this should be replaced by a dict query
    foreach (ASTType*, type, mod->types) {
        if (not strcasecmp(func->name, type->name)) {
            if (func->returnSpec and not(func->isStmt or func->isDefCtor))
                Parser_errorCtorHasType(parser, func, type);
            if (not func->returnSpec) {
                func->returnSpec = ASTTypeSpec_new(TYObject, CTYNone);
                func->returnSpec->type = type;
                // Ctors must AlWAYS return a new object.
                // even Ctors with args.
                func->returnsNewObjectAlways = true;
            }
            // TODO: isStmt Ctors should have the correct type so e.g.
            // you cannot have
            // Point(x as Number) := 3 + 5 * 12
            // but must return a Point instead. This cannot be enforced
            // here since type resolution hasn't been done at this
            // stage. Check this after the type inference step when the
            // stmt func has its return type assigned.
            // if (func->isStmt)
            //     Parser_errorCtorHasType(this, func, type);
            if (not isupper(*func->name)) Parser_warnCtorCase(parser, func);

            func->name = type->name;
            isCtor = true;
        }
    }

    // Capitalized names are not allowed unless they are constructors.
    if (not func->isDefCtor and not isCtor and isupper(*func->name))
        Parser_errorUnrecognizedCtor(parser, func);

    // The rest of the processing is on the contents of the function.
    if (not func->body) {
        func->analysed = true;
        return;
    }

    // Check for duplicate functions (same selectors) and report errors.
    // TODO: this should be replaced by a dict query
    foreach (ASTFunc*, func2, mod->funcs) {
        if (func == func2) break;
        if (not strcasecmp(func->selector, func2->selector))
            Parser_errorDuplicateFunc(parser, func, func2);
    }

    // Check unused variables in the function and report warnings.
    ASTFunc_checkUnusedVars(parser, func);

    // Mark the semantic pass as done for this function, so that recursive
    // calls found in the statements will not cause the compiler to recur.
    func->analysed = true;

    // Run the statement-level semantic pass on the function body.
    foreach (ASTExpr*, stmt, func->body->stmts)
        analyseExpr(parser, stmt, mod, false);

    // Statement functions are written without an explicit return type.
    // Figure out the type (now that the body has been analyzed).
    if (func->isStmt) setStmtFuncTypeInfo(parser, func);
    // TODO: for normal funcs, analyseExpr should check return statements to
    // have the same type as the declared return type.

    // Do optimisations or ANY lowering only if there are no errors
    if (not parser->issues.errCount and parser->mode != PMLint) {

        // do (try) CSE
        ASTFunc_hashExprs(/* parser, */ func);

        // Handle elemental operations like arr[4:50] = mx[14:60] + 3
        ASTScope_lowerElementalOps(func->body);
        // Extract subexprs like count(arr[arr<1e-15]) and promote them to
        // full statements corresponding to their C macros e.g.
        // Number _1; Array_count_filter(arr, arr<1e-15, _1);
        ASTScope_promoteCandidates(func->body);
    }
}

static void analyseTest(Parser* parser, ASTTest* test, ASTModule* mod) {
    if (not test->body) return;

    // Check for duplicate test names and report errors.
    // TODO: this should be replaced by a dict query
    foreach (ASTTest*, test2, mod->tests) {
        if (test == test2) break;
        if (not strcasecmp(test->name, test2->name))
            Parser_errorDuplicateTest(parser, test, test2);
    }

    // Check unused variables in the function and report warnings.
    ASTTest_checkUnusedVars(parser, test);

    // Run the statement-level semantic pass on the function body.
    foreach (ASTExpr*, stmt, test->body->stmts)
        analyseExpr(parser, stmt, mod, false);

    // Do optimisations or ANY lowering only if there are no errors
    if (not parser->issues.errCount and parser->mode != PMLint) {
        ASTScope_lowerElementalOps(test->body);
        ASTScope_promoteCandidates(test->body);
    }
}

#include "cse.h"
