
static void setStmtFuncTypeInfo(Parser* parser, ASTFunc* func) {
    // this assumes that setExprTypeInfo has been called on the func body
    ASTExpr* stmt = func->body->stmts->item;
    if (!func->returnSpec->typeType)
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
        || expr->kind == tkOpLT //
        || expr->kind == tkOpGT //
        || expr->kind == tkOpGE //
        || expr->kind == tkOpEQ //
        || expr->kind == tkOpNE;
}

///////////////////////////////////////////////////////////////////////////
static bool isBoolOp(ASTExpr* expr) {
    return expr->kind == tkKeyword_and //
        || expr->kind == tkKeyword_or //
        || expr->kind == tkKeyword_not;
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

// TODO: you should have a flag that tells
// you whether there is any string
// interpolation at all, and if yes what the expected size might be, and if
// it can be put on the stack

// actually the string may have embedded %s, so you need to process it
// in any case, unless you plan on doing puts.
// there are 3 things to do.
// 1. compute the format string
// 2. compute a guess for the size
// 3. keep a stack of vars that are in the string in the right order
static void ASTExpr_prepareInterp(
    Parser* parser, ASTExpr* expr, ASTScope* scope) {
    static Array(Ptr) vars;
    assert(expr->kind == tkString);
    PtrList** exprvars = &expr->vars;
    // at some point you should make it so that only strings with a preceding
    // $ get scanned for vars.
    // e.g. $"this is a $kindof day" gets processed but "this is a $kindof day"
    // remains as is.

    char* pos = expr->string + 1; // starts with '"'
    int line = expr->line, col = expr->col;

    // ******
    // The first var should already have been added during resolveVars!!!!
    // So check if there is any var in there, only then you should bother.
    // Otherwise an error will have been shown already in resolveVars.
    while (*pos) {
        // in this loop you print the text preceding a $, then text
        // suceeding a $, then loop. Here's the text before the $
        char* dollar = pos; // strchr(pos, '$');
        while (*dollar && *dollar != '$') {
            dollar++;
            col++;
            if (*dollar == '\n') {
                col = 0;
                line++;
            }
        }
        if (!*dollar) break; // no dollars
        // not using $..., using {...}
        long lentxt = dollar - pos;
        // printf(",\"%.*s\"", lentxt, pos);
        // after the $ is a variable, so look it up etc.
        char *varname = dollar + 1, *varend;
        bool wasbracket = varname[0] == '(';
        if (wasbracket) varname++;
        // char endchar = 0;
        // if () {
        //     // $(varName)
        //     endchar = ')';
        //     varend = strchrnul(varname, ')');

        // } else {
        // $varName
        varend = varname;
        while (*varend && isalnum(*varend)) ++varend;
        // }
        col += varend - varname;
        if (*varend == ')') *varend++ = 0;
        char endchar = *varend; // store the char in a temp
        *varend = 0;
        ASTVar* var = ASTScope_getVar(scope, varname);

        if (var) strcpy(varname, var->name); // fix case
        *varend = endchar;
        // ^ TODO: this should be getQualVar which looks up a.b.c and
        // returns c
        if (!var) {
            unreachable("undefined var found: %s", varname);
            return;
        }
        // You should have checked for all var refs to be valid in the analysis
        // phase!

        ASTExpr* ex = NEW(ASTExpr);
        ASTExpr* exdot = NULL;
        ex->kind = tkIdentifierResolved;
        ex->line = line, ex->col = col;
        ex->var = var;
        var->used = true;

        exdot = ex;
        if (!wasbracket)
            while (endchar == '.') {
                if (!var || var->typeSpec->typeType != TYObject) {
                    unreachable("using a . for a non-object type (string "
                                "interp: $%.*s)",
                        (int)(varend - varname), varname);
                    break;
                }

                // varend++;
                varname = ++varend;
                while (*varend && isalnum(*varend)) ++varend;
                col += varend - varname;
                endchar = *varend;
                *varend = 0;
                ASTType* type = var->typeSpec->type;
                var = ASTType_getVar(type, varname);
                if (!var) {
                    exdot = NULL; // reset it if was set along the chain
                    Parser_errorUnrecognizedMember(parser, type,
                        &(ASTExpr) { .string = varname,
                            .line = line,
                            .col = col,
                            .kind = tkIdentifier });
                } else {
                    var->used = true;
                    strcpy(varname, var->name); // fix case
                    exdot = NEW(ASTExpr);
                    exdot->kind = tkPeriod;
                    // exdot->line=line,exdot->col=ex->col+varend-varname;
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

        // Here var is the final one ie c in a.b.c

        // const char* fmtstr;
        // if (var->typeSpec->typeType == TYObject) {
        //     fmtstr = "%s";
        //     // you'll have to call Type_str(...) for it. Or actually why
        //     // not just do Type_print?
        // } else {
        //     fmtstr = TypeType_format(var->typeSpec->typeType, false);
        // }
        // printf(",%s", var->name);
        // ^ this should be qualified name or the cgen version
        // of accessing the actual member for a.b.c

        if (exdot) exprvars = PtrList_append(exprvars, exdot);
        pos = varend;
    }
}
//     break;
// case tkRawString:
// case tkNumber:
// case tkIdentifierResolved:
// case tkIdentifier:
// case tkArgumentLabel:
//     break;
// case tkFunctionCallResolved:
// case tkFunctionCall:
// case tkSubscript:
// case tkObjectInit:
// case tkObjectInitResolved:
// case tkSubscriptResolved:
//     if (expr->left) ASTExpr_prepareInterp(expr->left, scope);
//     break;
// case tkVarAssign:
//     ASTExpr_prepareInterp(expr->var->init, scope);
//     break;
// case tkKeyword_for:
// case tkKeyword_if:
// case tkKeyword_else:
// case tkKeyword_match:
// case tkKeyword_case:
// case tkKeyword_elif:
// case tkKeyword_while:
//     ASTExpr_prepareInterp(expr->left, scope);
//     foreach (ASTExpr*, subexpr, expr->body->stmts)
//         ASTExpr_prepareInterp(subexpr, expr->body);
//     break;
// default:
//     if (expr->prec) {
//         if (!expr->unary) ASTExpr_prepareInterp(expr->left, scope);
//         if (expr->right) ASTExpr_prepareInterp(expr->right, scope);
//     } else {
//         unreachable("unknown token kind %s", TokenKind_str[expr->kind]);
//     }
// }

// #this string
//     "The quick brown fox $jumps over $the lazy dog.";
// #becomes
//     `"%s%s%s%s%s", "The quick brown fox ", jumps, " over ", the, "lazy
//     dog.",
//         ""`;

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

///////////////////////////////////////////////////////////////////////////
static void analyseExpr(Parser* parser, ASTExpr* expr, ASTScope* scope,
    ASTModule* mod, ASTFunc* ownerFunc, bool inFuncArgs) {
    switch (expr->kind) {

        // -------------------------------------------------- //
    case tkFunctionCallResolved: {
        if (ownerFunc) { // TODO: ownerFunc should not be NULL, even top-level
                         // and type implicit ctors should have a func created
            PtrList_shift(&ownerFunc->callees, expr->func);
            PtrList_shift(&expr->func->callers, ownerFunc);
        }
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
            // TODO: check dims mismatch
            // TODO: check units mismatch
            // TODO: set enum base
            if (!(currArg = currArg->right)) break;
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
        if (expr->left)
            analyseExpr(parser, expr->left, scope, mod, ownerFunc, true);

        // TODO: need a function to make and return selector
        ASTExpr* arg1 = expr->left;
        if (arg1 && arg1->kind == tkOpComma) arg1 = arg1->left;
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
            analyseExpr(parser, expr, scope, mod, ownerFunc, inFuncArgs);
            return;
        }
        if (!strncmp(buf, "Void_", 5))
            Parser_errorCallingFuncWithVoid(parser, expr, arg1);
        else {
            Parser_errorUnrecognizedFunc(parser, expr, buf);

            if (*buf != '<') // not invalid type
            {
                int sugg = 0;
                foreach (ASTFunc*, func, mod->funcs) {
                    if (!strcasecmp(expr->string, func->name)) {
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

        // -------------------------------------------------- //
    case tkVarAssign: {
        ASTExpr* const init = expr->var->init;
        ASTTypeSpec* const typeSpec = expr->var->typeSpec;

        if (typeSpec->typeType == TYUnresolved)
            resolveTypeSpec(parser, typeSpec, mod);

        if (!init) {
            // if the typespec is given, generate the init expr yourself
            // this is only for basic types like primitives.
            // and arrays of anything can be left without an init.
            if (!typeSpec->dims) // goto errorMissingInit;
                switch (typeSpec->typeType) {
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
                    // this is an error, no way to find out what you want
                    Parser_errorMissingInit(parser, expr);
                }
        } else {
            // if (typeSpec->typeType == TYUnresolved)
            //     resolveTypeSpec(parser, typeSpec, mod);
            // first try to set enum base if applicable.
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
                        if (init->left->var->typeSpec->type->isEnum) {
                            typeSpec->type = init->left->var->typeSpec->type;
                        } else {
                            while (e->kind == tkPeriod) e = e->right;
                            // at this point, it must be a resolved ident or
                            // subscript
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
                // init can be nil, which is a TYAnyType
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

        // -------------------------------------------------- //
    case tkKeyword_match: {
        ASTExpr* cond = expr->left;
        if (cond) {
            analyseExpr(parser, cond, scope, mod, ownerFunc, false);
            ASTTypeSpec* tsp = ASTExpr_getObjectTypeSpec(cond);
            if (expr->body && tsp && tsp->type
                && tsp->type
                       ->isEnum) { // left->typeType == TYObject&&->isEnum) {
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

        // -------------------------------------------------- //
    case tkSubscriptResolved: {
        // assert(expr->left->kind == tkArrayOpen);
        int nhave = ASTExpr_countCommaList(expr->left);
        if (nhave != expr->var->typeSpec->dims)
            Parser_errorIndexDimsMismatch(parser, expr, nhave);
    }
        // fallthru
    case tkSubscript:
        if (expr->left)
            analyseExpr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
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
        // by the time analysis is called, all vars must have been resolved
        expr->typeType = TYErrorType;
        break;

    case tkIdentifierResolved:
        expr->typeType = expr->var->typeSpec->typeType;
        expr->collectionType = expr->var->typeSpec->collectionType;
        expr->elemental = expr->collectionType != CTYNone;
        expr->dims = expr->var->typeSpec->dims;

        // this was done just to ensure enums are caught and analysed in
        // non-lint mode. In lint mode they are done anyway.
        // TODO: remove this, you shouldnt be doing it on each var use it will
        // be too intensive. let it just be done on each var decl and you figure
        // out a way to set the enum type on var decls.
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
            // using array literals you can only init 1D or 2D
            if (expr->typeType == TYObject) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
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
            analyseExpr(parser, expr->right, scope, mod, ownerFunc, true);
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

        if (!expr->left) {
            Parser_errorNoEnumInferred(parser, expr->right);
            break;
        }

        assert(expr->left->kind == tkIdentifierResolved
            || expr->left->kind == tkIdentifier);
        analyseExpr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);

        // The name/type resolution of expr->left may have failed.
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
        //  or member->kind == tkFunctionCall);
        if (member->kind != tkIdentifier)
            analyseExpr(
                parser, member->left, scope, mod, ownerFunc, inFuncArgs);

        // the left must be a resolved ident
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

        // Resolve the member in the scope of the type definition.
        resolveMember(parser, member, type);
        // Name resolution may fail...
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

        //    case tkRawString:
        // TODO: analyse regex, compile it already, whatever
        //        break;
        // -------------------------------------------------- //
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left)
                analyseExpr(
                    parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
            // some exprs like return can be used without any args
            if (expr->right)
                analyseExpr(
                    parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

            if (expr->kind == tkKeyword_or && expr->left->typeType != TYBool) {
                // Handle the 'or' keyword used to provide alternatives for
                // a nullable expression.
                ;
            } else if (isCmpOp(expr) || isBoolOp(expr)
                || expr->kind == tkKeyword_in) {
                // Handle comparison and logical operators (always return a
                // bool)
                expr->typeType
                    = (expr->right->typeType == TYErrorType
                          || (!expr->unary
                              && expr->left->typeType == TYErrorType))
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
                    = expr->right->elemental || expr->kind == tkOpColon;
            // TODO: actually, indexing by an array of integers is also an
            // indication of an elemental op

            if (!expr->unary && expr->left) {
                expr->elemental = expr->elemental || expr->left->elemental;
                expr->dims = expr->right->dims;
                // if (expr->kind == tkOpColon and expr->right->kind ==
                // tkOpColon)
                //     expr->right->dims = 0; // temporarily set it to 0 to
                //     allow
                // parsing stepped ranges 1:s:n

                if (expr->dims != expr->left->dims
                    && !(inFuncArgs
                        && (expr->kind == tkOpComma
                            || expr->kind == tkOpAssign))) {
                    // if either one has 0 dims (scalar) it is an elemental op
                    // with a scalar.
                    if (expr->left->dims != 0 && expr->right->dims != 0) {
                        Parser_errorBinOpDimsMismatch(parser, expr);
                        //                         unreachable("TODO: make
                        //                         error: dims mismatch at line
                        //                         "
                        //         "%d col %d\n    %d vs %d\n",
                        // expr->line, expr->col, expr->left->dims,
                        // expr->right->dims);
                        expr->right->typeType = TYErrorType;
                    } else if (expr->kind == tkPlus //
                        || expr->kind == tkMinus //
                        || expr->kind == tkTimes //
                        || expr->kind == tkSlash //
                        || expr->kind == tkPower //
                        || expr->kind == tkOpMod) {
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
                        && expr->left->dims == 0 && expr->right->dims == 1) {
                        // eprintf("ok `in` dims@ %d %d %d %d\n", expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                    } else if (expr->kind == tkOpColon //
                        && expr->left->dims == 1
                        && expr->left->kind == tkOpColon
                        && expr->right->dims == 0) {
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
            if (!expr->unary && expr->left
                && !(inFuncArgs
                    && (expr->kind == tkOpComma || expr->kind == tkOpAssign))) {
                // ignore , and = inside function call arguments. thing is
                // array or dict literals passed as args will have , and =
                // which should be checked. so when you descend into their
                // args, unset inFuncArgs.
                TypeTypes leftType = expr->left->typeType;
                TypeTypes rightType = expr->right->typeType;

                if (leftType == TYBool
                    && (expr->kind == tkOpLE || expr->kind == tkOpLT)) {
                    // Special case: chained LE/LT operators: e.g. 0 <= yCH4
                    // <= 1.
                    ;
                } else if (leftType != rightType) {
                    // Type mismatch for left and right operands is always
                    // an error.
                    Parser_errorTypeMismatchBinOp(parser, expr);
                    expr->typeType = TYErrorType;
                } else if (leftType == TYString
                    && (expr->kind == tkOpAssign || expr->kind == tkOpEQ
                        || expr->kind == tkOpNE)) {
                    // Allow assignment, equality test and != for strings.
                    // TODO: might even allow comparison operators, and
                    // perhaps allow +=, or better .= or something. Or
                    // perhaps append(x!) is clearer
                    ;
                } else if (leftType == TYBool
                    && (expr->kind == tkOpAssign //
                        || expr->kind == tkOpEQ //
                        || expr->kind == tkKeyword_and //
                        || expr->kind == tkKeyword_or
                        || expr->kind == tkKeyword_not
                        || expr->kind == tkKeyword_notin))
                    ;
                else if (isArithOp(expr)
                    && (!TypeType_isnum(leftType)
                        || !TypeType_isnum(rightType))) {
                    // Arithmetic operators are only relevant for numeric
                    // types.
                    // if(  leftType==TYObject&&expr->left->)
                    // TODO: allow enum +
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
        if (!strcasecmp(type->name, type2->name))
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
            analyseExpr(parser, stmt, type->body, mod, NULL, false);
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
        if (!strcasecmp(func->name, type->name)) {
            if (func->returnSpec && !(func->isStmt || func->isDefCtor))
                Parser_errorCtorHasType(parser, func, type);
            if (!func->returnSpec) {
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
            if (!isupper(*func->name)) Parser_warnCtorCase(parser, func);

            func->name = type->name;
            isCtor = true;
        }
    }

    // Capitalized names are not allowed unless they are constructors.
    if (!func->isDefCtor && !isCtor && isupper(*func->name))
        Parser_errorUnrecognizedCtor(parser, func);

    // The rest of the processing is on the contents of the function.
    if (!func->body) {
        func->analysed = true;
        return;
    }

    // Check for duplicate functions (same selectors) and report errors.
    // TODO: this should be replaced by a dict query
    foreach (ASTFunc*, func2, mod->funcs) {
        if (func == func2) break;
        if (!strcasecmp(func->selector, func2->selector))
            Parser_errorDuplicateFunc(parser, func, func2);
    }

    // Check unused variables in the function and report warnings.
    ASTFunc_checkUnusedVars(parser, func);

    // Mark the semantic pass as done for this function, so that recursive
    // calls found in the statements will not cause the compiler to recur.
    func->analysed = true;

    // Run the statement-level semantic pass on the function body.
    foreach (ASTExpr*, stmt, func->body->stmts)
        analyseExpr(parser, stmt, func->body, mod, NULL, false);

    // Statement functions are written without an explicit return type.
    // Figure out the type (now that the body has been analyzed).
    if (func->isStmt) setStmtFuncTypeInfo(parser, func);
    // TODO: for normal funcs, analyseExpr should check return statements to
    // have the same type as the declared return type.

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {

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
    if (!test->body) return;

    // Check for duplicate test names and report errors.
    // TODO: this should be replaced by a dict query
    foreach (ASTTest*, test2, mod->tests) {
        if (test == test2) break;
        if (!strcasecmp(test->name, test2->name))
            Parser_errorDuplicateTest(parser, test, test2);
    }

    // Check unused variables in the function and report warnings.
    ASTTest_checkUnusedVars(parser, test);

    // Run the statement-level semantic pass on the function body.
    foreach (ASTExpr*, stmt, test->body->stmts)
        analyseExpr(parser, stmt, test->body, mod, NULL, false);

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {
        ASTScope_lowerElementalOps(test->body);
        ASTScope_promoteCandidates(test->body);
    }
}

#include "cse.h"
