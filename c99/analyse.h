
static void setStmtFuncTypeInfo(Parser* parser, JetFunc* func) {
    // this assumes that setExprTypeInfo has been called on the func body
    JetExpr* stmt = func->body->stmts->item;
    if (!func->spec->typeType)
        func->spec->typeType = stmt->typeType;
    else if (func->spec->typeType != stmt->typeType)
        Parser_errorTypeMismatchBinOp(parser, stmt);
}

// TODO make sempassModule -> same as JetModule_analyse now
static void JetType_analyse(Parser* parser, JetType* type, JetModule* mod);
static void JetFunc_analyse(Parser* parser, JetFunc* func, JetModule* mod);

static void analyseDictLiteral(Parser* parser, JetExpr* expr, JetModule* mod) {
    // check that
    // - dict keys in the dict literal are of the same type.
    // - values are of the same type.
    // - all exprs within the commas are tkOpAssign.
    // assign the spec pair somehow to the tkDictLiteral expr.
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
static void JetExpr_prepareInterp(
    Parser* parser, JetExpr* expr, JetScope* scope) {
    static Array(Ptr) vars;
    assert(expr->kind == tkString || expr->kind == tkRawString);
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
        JetVar* var = JetScope_getVar(scope, varname);

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

        JetExpr* ex = NEW(JetExpr);
        JetExpr* exdot = NULL;
        ex->kind = tkIdentifierResolved;
        ex->line = line, ex->col = col;
        ex->var = var;
        var->used = true;

        exdot = ex;
        if (!wasbracket)
            while (endchar == '.') {
                if (!var || var->spec->typeType != TYObject) {
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
                JetType* type = var->spec->type;
                var = JetType_getVar(type, varname);
                if (!var) {
                    exdot = NULL; // reset it if was set along the chain
                    Parser_errorUnrecognizedMember(parser, type,
                        &(JetExpr) { .string = varname,
                            .line = line,
                            .col = col,
                            .kind = tkIdentifier });
                } else {
                    var->used = true;
                    strcpy(varname, var->name); // fix case
                    exdot = NEW(JetExpr);
                    exdot->kind = tkPeriod;
                    // exdot->line=line,exdot->col=ex->col+varend-varname;
                    exdot->left = ex;
                    exdot->right = NEW(JetExpr);
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
        // if (var->spec->typeType == TYObject) {
        //     fmtstr = "%s";
        //     // you'll have to call Type_str(...) for it. Or actually why
        //     // not just do Type_print?
        // } else {
        //     fmtstr = TypeType_format(var->spec->typeType, false);
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
//     if (expr->left) JetExpr_prepareInterp(expr->left, scope);
//     break;
// case tkVarAssign:
//     JetExpr_prepareInterp(expr->var->init, scope);
//     break;
// case tkKeyword_for:
// case tkKeyword_if:
// case tkKeyword_else:
// case tkKeyword_match:
// case tkKeyword_case:
// case tkKeyword_elif:
// case tkKeyword_while:
//     JetExpr_prepareInterp(expr->left, scope);
//     foreach (JetExpr*, subexpr, expr->body->stmts)
//         JetExpr_prepareInterp(subexpr, expr->body);
//     break;
// default:
//     if (expr->prec) {
//         if (!expr->unary) JetExpr_prepareInterp(expr->left, scope);
//         if (expr->right) JetExpr_prepareInterp(expr->right, scope);
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

static void JetExpr_setEnumBase(
    Parser* parser, JetExpr* expr, JetTypeSpec* spec, JetModule* mod) {
    switch (expr->kind) {
    case tkPeriod:
        if (expr->left) return;
        expr->left = NEW(JetExpr);
        expr->left->kind = tkIdentifier;
        expr->left->string
            = spec->typeType == TYObject ? spec->type->name : spec->name;
        expr->left->line = expr->line;
        expr->left->col = expr->col;
        resolveVars(parser, expr, mod->scope, false);
    case tkOpPlus:
    case tkOpComma:
    case tkOpEQ:
    case tkOpNE:
        JetExpr_setEnumBase(parser, expr->left, spec, mod);
        fallthrough;
    case tkOpAssign:
    case tkArrayOpen: JetExpr_setEnumBase(parser, expr->right, spec, mod);
    default:;
    }
}

static void JetExpr_reduceVarUsage(JetExpr* expr) {
    // vars and subscripts, but also function calls have their usage counts
    // reduced when being involved in an expr that inits a known unused
    // variable.
    switch (expr->kind) {
    case tkString: break; // TODO: vars within string intrp
    case tkSubscript:
    case tkFunctionCall:
        if (expr->left) JetExpr_reduceVarUsage(expr->left);
        break;
    case tkSubscriptResolved:
        if (expr->left) JetExpr_reduceVarUsage(expr->left); // fallthru
    case tkIdentifierResolved:
        // reduce this var's usage, and if it drops to zero reduce the usage of
        // all vars in this var's init. Sort of like a compile time ref count.
        if (!--expr->var->used) {
            if (expr->var->init) JetExpr_reduceVarUsage(expr->var->init);
            // if (expr->var->spec->typeType == TYObject)
            //     --expr->var->spec->type->used;
        }
        break;
    case tkFunctionCallResolved:
        if (expr->left) JetExpr_reduceVarUsage(expr->left);
        expr->func->used--;
        break;
    case tkPeriod:
        if (expr->left) JetExpr_reduceVarUsage(expr->left);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left) JetExpr_reduceVarUsage(expr->left);
            if (expr->right) JetExpr_reduceVarUsage(expr->right);
        };
    }
}

static bool JetFunc_calls(JetFunc* func, JetFunc* target) {
    bool ret = false;
    if (!func->visited) {
        func->visited = true;
        foreach (JetFunc*, fn, func->callees) {
            if (!fn->intrinsic && //
                (fn == target || JetFunc_calls(fn, target))) {
                ret = true;
                break;
            }
        }
        func->visited = false;
    }
    return ret;
}

static void JetFunc_checkRecursion(JetFunc* func) {
    if (!func->recursivity) func->recursivity = 1 + JetFunc_calls(func, func);
    // 0 means not checked
}

static void JetExpr_analyse(Parser* parser, JetExpr* expr, JetScope* scope,
    JetModule* mod, JetFunc* ownerFunc, bool inFuncArgs) {
    switch (expr->kind) {

        // -------------------------------------------------- //
    case tkFunctionCallResolved: {
        if (ownerFunc) { // TODO: ownerFunc should not be NULL, even top-level
                         // and type implicit ctors should have a func created
            PtrList_shift(&ownerFunc->callees, expr->func);
            PtrList_shift(&expr->func->callers, ownerFunc);
        }
        if (JetExpr_countCommaList(expr->left) != expr->func->argCount)
            Parser_errorArgsCountMismatch(parser, expr);
        expr->typeType = expr->func->spec
            ? expr->func->spec->typeType
            : TYNoType; // should actually be TYVoid
        expr->collectionType = expr->func->spec
            ? expr->func->spec->collectionType
            : CTYNone; // should actually be TYVoid
        expr->elemental = expr->func->elemental;
        if (expr->func->spec) expr->dims = expr->func->spec->dims;
        // isElementalFunc means the func is only defined for (all)
        // Number arguments and another definition for vector args
        // doesn't exist. Basically during typecheck this should see
        // if a type mismatch is only in terms of collectionType.
        if (!expr->left) break;
        expr->elemental = expr->elemental && expr->left->elemental;
        expr->throws = expr->left->throws || expr->func->throws;
        JetExpr* currArg = expr->left;
        foreach (JetVar*, arg, expr->func->args) {
            JetExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) cArg = cArg->right;
            if (cArg->typeType != arg->spec->typeType)
                Parser_errorArgTypeMismatch(parser, cArg, arg);
            // TODO: check dims mismatch
            // TODO: check units mismatch
            // TODO: set enum base
            if (!(currArg = currArg->right)) break;
        }

        currArg = expr->left;
        while (currArg) {
            JetExpr* cArg
                = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) {
                // LHS will be a tkIdentifier. You should resolve it to one
                // of the function's arguments and set it to tkArgumentLabel.
                assert(cArg->left->kind == tkIdentifier);
                JetVar* theArg = NULL;
                foreach (JetVar*, arg, expr->func->args) {
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
            JetExpr_analyse(parser, expr->left, scope, mod, ownerFunc, true);

        // TODO: need a function to make and return selector
        JetExpr* arg1 = expr->left;
        if (arg1 && arg1->kind == tkOpComma) arg1 = arg1->left;
        const char* typeName = JetExpr_typeName(arg1);
        //        const char* collName = "";
        //        if (arg1)
        //        collName=CollectionType_nativeName(arg1->collectionType);
        if (arg1) bufp += sprintf(bufp, "%s_", typeName);
        bufp += sprintf(bufp, "%s", expr->string);
        if (expr->left)
            JetExpr_strarglabels(expr->left, bufp, 128 - ((int)(bufp - buf)));

        JetFunc* found = JetModule_getFunc(mod, buf);
        if (!found) found = JetModule_getFuncByTypeMatch(mod, expr);
        if (found) {
            expr->kind = tkFunctionCallResolved;
            expr->func = found;
            expr->func->used++;
            JetFunc_analyse(parser, found, mod);
            JetExpr_analyse(parser, expr, scope, mod, ownerFunc, inFuncArgs);
            return;
        }
        if (!strncmp(buf, "Void_", 5))
            Parser_errorCallingFuncWithVoid(parser, expr, arg1);
        else {
            Parser_errorUnrecognizedFunc(parser, expr, buf);

            if (*buf != '<') // not invalid type
            {
                int sugg = 0;
                foreach (JetFunc*, func, mod->funcs) {
                    if (!strcasecmp(expr->string, func->name)) {
                        eprintf("\e[36;1minfo:\e[0;2m   not viable: %s with %d "
                                "arguments at %s:%d\e[0m\n",
                            func->prettySelector, func->argCount, mod->filename,
                            func->line);
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
                            func->name, func->prettySelector, mod->filename,
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
        JetExpr* const init = expr->var->init;
        JetTypeSpec* const spec = expr->var->spec;

        if (spec->typeType == TYUnresolved) resolveTypeSpec(parser, spec, mod);

        if (!init) {
            // if the typespec is given, generate the init expr yourself
            // this is only for basic types like primitives.
            // and arrays of anything can be left without an init.
            if (!spec->dims) // goto errorMissingInit;
                switch (spec->typeType) {
                case TYReal64: expr->var->init = expr_const_0; break;
                case TYString: expr->var->init = expr_const_empty; break;
                case TYBool: expr->var->init = expr_const_no; break;
                case TYObject:
                    if (!spec->type->isEnum) {
                        expr->var->init = expr_const_nil;
                        break;
                    }
                default:
                errorMissingInit:
                    // this is an error, no way to find out what you want
                    Parser_errorMissingInit(parser, expr);
                }
        } else {
            // if (spec->typeType == TYUnresolved)
            //     resolveTypeSpec(parser, spec, mod);
            // first try to set enum base if applicable.
            if (spec->typeType == TYObject && spec->type->isEnum)
                JetExpr_setEnumBase(parser, init, spec, mod);

            JetExpr_analyse(parser, init, scope, mod, ownerFunc, false);

            if (!expr->var->used) {
                // assigning to an unused var on the left of =. Decrement the
                // usage counts of all vars referenced on the RHS because well
                // being used for an unused var is not actually being used.
                // TODO: this should also be done for += -= *= etc.
                JetExpr_reduceVarUsage(init);
            }

            if (init->typeType != TYNilType) {
                // if not nil, get type info from init expr.
                expr->typeType = init->typeType;
                expr->collectionType = init->collectionType;
                expr->nullable = init->nullable;
                expr->elemental = init->elemental;
                expr->throws = init->throws;
                expr->impure = init->impure;
                expr->dims = init->dims;
            } else if (spec->typeType == TYObject) {
                // if nil, and typespec given and resolved, set type info from
                // typespec.
                expr->typeType = spec->typeType;

            } else if (spec->typeType == TYUnresolved) {
                // this will already have caused an error while resolution.
                // since specified type is unresolved and init is nil, there
                // is nothing to do except to mark it an error type.
                spec->typeType = expr->typeType = TYErrorType;

            } else {
                // this would be something like init'ing primitives with nil.
                Parser_errorInitMismatch(parser, expr);
                spec->typeType = expr->typeType = TYErrorType;
            }

            if (spec->typeType == TYUnresolved) {
                spec->typeType = init->typeType;
                if (init->typeType == TYObject) {
                    if (init->kind == tkFunctionCallResolved) {
                        expr->var->spec = init->func->spec;
                        // ^ TODO: DROP old expr->var->spec!!
                        spec->type = init->func->spec->type;
                        spec->dims = init->func->spec->dims;
                    } else if (init->kind == tkIdentifierResolved) {
                        expr->var->spec = init->var->spec;
                        spec->type = init->var->spec->type;
                        spec->dims = init->var->spec->dims;
                    } else if (init->kind == tkArrayOpen)
                        spec->type = init->elementType;
                    else if (init->kind == tkBraceOpen)
                        spec->type = init->elementType;
                    else if (init->kind == tkPeriod) {
                        JetExpr* e = init;
                        if (init->left->var->spec->type->isEnum) {
                            spec->type = init->left->var->spec->type;
                        } else {
                            while (e->kind == tkPeriod) e = e->right;
                            // at this point, it must be a resolved ident or
                            // subscript
                            spec->type = e->var->spec->type;
                            spec->dims = e->var->spec->dims;
                        }

                    } else {
                        unreachable("%s", "var type inference failed");
                    }
                    JetType_analyse(parser, spec->type, mod);
                }
            } else if (spec->typeType != init->typeType
                // && init->typeType != TYNilType
            ) {
                // init can be nil, which is a TYNilType
                Parser_errorInitMismatch(parser, expr);
                expr->typeType = TYErrorType;
            }

            if (spec->dims == 0) {
                spec->collectionType = init->collectionType;
                spec->dims = init->collectionType == CTYTensor ? init->dims
                    : init->collectionType == CTYArray         ? 1
                                                               : 0;
            } else if (spec->dims != 1 && init->collectionType == CTYArray) {
                Parser_errorInitDimsMismatch(parser, expr, 1);
                expr->typeType = TYErrorType;
            } else if (spec->dims != 2 && init->collectionType == CTYTensor) {
                Parser_errorInitDimsMismatch(parser, expr, 2);
                expr->typeType = TYErrorType;

            } else if (spec->dims != 0 && init->collectionType == CTYNone) {
                Parser_errorInitDimsMismatch(parser, expr, 0);
                expr->typeType = TYErrorType;
            }
        }
    } break;

        // -------------------------------------------------- //
    case tkKeyword_match: {
        JetExpr* cond = expr->left;
        if (cond) {
            JetExpr_analyse(parser, cond, scope, mod, ownerFunc, false);
            JetTypeSpec* tsp = JetExpr_getObjectTypeSpec(cond);
            if (expr->body && tsp && tsp->type
                && tsp->type
                       ->isEnum) { // left->typeType == TYObject&&->isEnum) {
                foreach (JetExpr*, cas, expr->body->stmts)
                    if (cas->left)
                        JetExpr_setEnumBase(parser, cas->left, tsp, mod);
            }
        }

        foreach (JetExpr*, stmt, expr->body->stmts) {
            JetExpr_analyse(
                parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
            if (cond && stmt->kind == tkKeyword_case && stmt->left
                && (stmt->left->typeType != cond->typeType
                    || (cond->typeType == TYObject
                        && JetExpr_getTypeOrEnum(stmt->left)
                            != JetExpr_getTypeOrEnum(cond))))
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
            JetExpr_analyse(parser, expr->left, scope, mod, ownerFunc, false);
        foreach (JetExpr*, stmt, expr->body->stmts)
            JetExpr_analyse(
                parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
    } break;

        // -------------------------------------------------- //
    case tkSubscriptResolved: {
        // assert(expr->left->kind == tkArrayOpen);
        int nhave = JetExpr_countCommaList(expr->left);
        if (nhave != expr->var->spec->dims)
            Parser_errorIndexDimsMismatch(parser, expr, nhave);
    }
        // fallthru
    case tkSubscript:
        if (expr->left)
            JetExpr_analyse(
                parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
        if (expr->kind == tkSubscriptResolved) {
            expr->typeType = expr->var->spec->typeType;
            expr->collectionType = expr->var->spec->collectionType;
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
            // JetExpr_analyse for a : op should set dims to 1. Then you just
            // walk the comma op in expr->right here and check which index exprs
            // have dims=0. Special case is when the index expr is a logical,
            // meaning you are filtering the array, in this case the result's
            // dims is always 1.
        }
        break;

    case tkString:
    case tkRawString:
        expr->typeType = TYString;
        JetExpr_prepareInterp(parser, expr, scope);
        break;

    case tkNumber: expr->typeType = TYReal64; break;

    case tkKeyword_yes:
    case tkKeyword_no: expr->typeType = TYBool; break;

    case tkKeyword_nil: expr->typeType = TYNilType; break;

    case tkIdentifier:
        // by the time analysis is called, all vars must have been resolved
        // Parser_errorUnrecognizedVar(parser, expr);
        // func call labels, enum names etc remain identifiers
        expr->typeType = TYErrorType;
        break;

    case tkIdentifierResolved:
        expr->typeType = expr->var->spec->typeType;
        expr->collectionType = expr->var->spec->collectionType;
        expr->elemental = expr->collectionType != CTYNone;
        expr->dims = expr->var->spec->dims;

        // this was done just to ensure enums are caught and analysed in
        // non-lint mode. In lint mode they are done anyway.
        // TODO: remove this, you shouldnt be doing it on each var use it will
        // be too intensive. let it just be done on each var decl and you figure
        // out a way to set the enum type on var decls.
        if (expr->typeType == TYObject)
            JetType_analyse(parser, expr->var->spec->type, mod);
        break;

    case tkArrayOpen:
        expr->collectionType = CTYArray;
        if (expr->right) {
            JetExpr_analyse(
                parser, expr->right, scope, mod, ownerFunc, inFuncArgs);
            expr->typeType = expr->right->typeType;
            expr->collectionType
                = expr->right->kind == tkOpSemiColon ? CTYTensor : CTYArray;
            expr->dims = expr->right->kind == tkOpSemiColon ? 2 : 1;
            // using array literals you can only init 1D or 2D
            if (expr->typeType == TYObject) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                JetExpr* first = expr->right;
                while (first->kind == tkOpComma || first->kind == tkOpSemiColon)
                    first = first->left;
                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->spec->type;
                    if (first->var->spec->dims
                        || first->var->spec->collectionType != CTYNone)
                        unreachable(
                            "trying to make array of arrays %d", expr->line);
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->spec->type;
                    if (first->func->spec->dims
                        || first->var->spec->collectionType != CTYNone)
                        unreachable("trying to make array of arrays line %d",
                            expr->line);
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tkObjectInitResolved:
                    // expr->elementType = first->var->spec->type;break;
                }
                // expr->elementType =
            }
        }
        break;

    case tkBraceOpen:
        if (expr->right) {
            JetExpr_analyse(parser, expr->right, scope, mod, ownerFunc, true);
            // TODO: you told JetExpr_analyse to not care about what's on the
            // LHS of tkOpAssign exprs. Now you handle it yourself. Ensure that
            // they're all of the same type and set that type to the expr
            // somehow.
            analyseDictLiteral(parser, expr->right, mod);
            expr->typeType = expr->right->typeType;
            if (expr->typeType == TYObject) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                JetExpr* first = expr->right;
                while (first->kind == tkOpComma) first = first->left;
                if (first->kind == tkOpAssign) first = first->right;
                // we care about the value in the key-value pair. We'll figure
                // out the key type later or not, whatever.
                switch (first->kind) {
                case tkIdentifierResolved:
                    expr->elementType = first->var->spec->type;
                    break;
                case tkFunctionCallResolved:
                    expr->elementType = first->func->spec->type;
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tkObjectInitResolved:
                    // expr->elementType = first->var->spec->type;break;
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
        JetExpr_analyse(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);

        // The name/type resolution of expr->left may have failed.
        if (!expr->left->typeType) break;

        JetExpr* member = expr->right;
        if (member->kind == tkPeriod) {
            member = member->left;
            if (member->kind != tkIdentifier) {
                Parser_errorUnexpectedExpr(parser, member);
                break;
            }
        }

        if (!ISIN(3, member->kind, tkIdentifier, tkSubscript, tkFunctionCall)) {
            Parser_errorUnexpectedExpr(parser, member);
            break;
        }
        //  or member->kind == tkFunctionCall);
        if (member->kind != tkIdentifier && member->left)
            JetExpr_analyse(
                parser, member->left, scope, mod, ownerFunc, inFuncArgs);

        // the left must be a resolved ident
        if (expr->left->kind != tkIdentifierResolved) break;

        if (expr->left->var->spec->typeType == TYErrorType) {
            expr->typeType = TYErrorType;
            break;
        }
        assert(expr->left->var->spec->typeType == TYObject); //!= TYNilType);

        JetType* type = expr->left->var->spec->type;
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
        JetExpr_analyse(parser, member, scope, mod, ownerFunc, inFuncArgs);

        if (expr->right->kind == tkPeriod)
            JetExpr_analyse(
                parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

        expr->typeType = type->isEnum ? TYObject : expr->right->typeType;
        expr->collectionType = expr->right->collectionType;
        expr->elemental = expr->right->elemental;
        expr->dims = expr->right->dims;
    } break;

    case tkLineComment: break;

    case tkArgumentLabel:

        break;

        //    case tkRawString:
        // TODO: analyse regex, compile it already, whatever
        //        break;
        // -------------------------------------------------- //
        // case tkKeyword_in:
        // case tkKeyword_notin:
        // these ops may take an array of enums, so set their base type.
        // if (expr->left->kind==tkIdentifierResolved)

        // there's some work being done on these as standard binops, so
        // fallthrough;
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left)
                JetExpr_analyse(
                    parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
            // some exprs like return can be used without any args

            if (ISIN(5, expr->kind, tkKeyword_in, tkKeyword_notin, tkOpAssign,
                    tkOpEQ, tkOpNE)) {
                JetTypeSpec* spec = JetExpr_getObjectTypeSpec(expr->left);
                if (spec && spec->typeType == TYObject && spec->type->isEnum) {
                    JetExpr_setEnumBase(parser, expr->right, spec, mod);
                } else {
                    spec = JetExpr_getObjectTypeSpec(expr->left);
                    if (spec && spec->typeType == TYObject
                        && spec->type->isEnum) {
                        JetExpr_setEnumBase(parser, expr->right, spec, mod);
                    }
                }
            }

            if (expr->right)
                JetExpr_analyse(
                    parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

            if (expr->kind == tkKeyword_or && expr->left->typeType != TYBool) {
                // Handle the 'or' keyword used to provide alternatives for
                // a nullable expression.
                ;
            } else if (isCmpOp(expr) || isBoolOp(expr)) {
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
                        expr->right->typeType = TYErrorType;
                    } else if (expr->kind == tkOpPlus //
                        || expr->kind == tkOpMinus //
                        || expr->kind == tkOpTimes //
                        || expr->kind == tkOpSlash //
                        || expr->kind == tkOpPower //
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
                    } else if ((expr->kind == tkKeyword_in
                                   || expr->kind == tkKeyword_notin)
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
            unreachable("unknown expr kind: %s", TokenKind_names[expr->kind]);
        }
    }
}

static void JetType_analyse(Parser* parser, JetType* type, JetModule* mod) {
    if (type->analysed) return;
    // eprintf(
    //     "JetExpr_analyse: %s at ./%s:%d\n", type->name, parser->filename,
    //     type->line);
    if (type->super) {
        resolveTypeSpec(parser, type->super, mod);
        if (type->super->type == type)
            Parser_errorTypeInheritsSelf(parser, type);
    }
    // TODO: this should be replaced by a dict query
    foreach (JetType*, type2, mod->types) {
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
        foreach (JetExpr*, stmt, type->body->stmts)
            JetExpr_analyse(parser, stmt, type->body, mod, NULL, false);
}
static void JetFunc_hashExprs(Parser* parser, JetFunc* func);

static void JetFunc_analyse(Parser* parser, JetFunc* func, JetModule* mod) {
    if (func->analysed) return;
    // eprintf("JetExpr_analyse: %s at ./%s:%d\n", func->selector,
    // parser->filename, func->line);

    bool isCtor = false;
    // Check if the function is a constructor call and identify the type.
    // TODO: this should be replaced by a dict query
    foreach (JetType*, type, mod->types) {
        if (!strcasecmp(func->name, type->name)) {
            if (func->spec && !(func->isStmt || func->isDefCtor))
                Parser_errorCtorHasType(parser, func, type);
            if (!func->spec) {
                func->spec = JetTypeSpec_new(TYObject, CTYNone);
                func->spec->type = type;
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
    foreach (JetFunc*, func2, mod->funcs) {
        if (func == func2) break;
        if (!strcasecmp(func->selector, func2->selector))
            Parser_errorDuplicateFunc(parser, func, func2);
    }

    // Mark the semantic pass as done for this function, so that recursive
    // calls found in the statements will not cause the compiler to recur.
    func->analysed = true;

    // Run the statement-level semantic pass on the function body.
    foreach (JetExpr*, stmt, func->body->stmts)
        JetExpr_analyse(parser, stmt, func->body, mod, func, false);

    // Check unused variables in the function and report warnings.
    JetFunc_checkUnusedVars(parser, func);
    // Statement functions are written without an explicit return type.
    // Figure out the type (now that the body has been analyzed).
    if (func->isStmt) setStmtFuncTypeInfo(parser, func);
    // TODO: for normal funcs, JetExpr_analyse should check return statements to
    // have the same type as the declared return type.

    // this is done here so that linter can give hints on what is picked up for
    // CSE. This func should not modify the Jet except marking CSE candidates!
    JetFunc_hashExprs(parser, func);

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {

        // Handle elemental operations like arr[4:50] = mx[14:60] + 3
        JetScope_lowerElementalOps(func->body);
        // Extract subexprs like count(arr[arr<1e-15]) and promote them to
        // full statements corresponding to their C macros e.g.
        // Number _1; Array_count_filter(arr, arr<1e-15, _1);
        JetScope_promoteCandidates(func->body);
    }
}

static void JetTest_analyse(Parser* parser, JetTest* test, JetModule* mod) {
    if (!test->body) return;

    // Check for duplicate test names and report errors.
    // TODO: this should be replaced by a dict query
    foreach (JetTest*, test2, mod->tests) {
        if (test == test2) break;
        if (!strcasecmp(test->name, test2->name))
            Parser_errorDuplicateTest(parser, test, test2);
    }

    // Check unused variables in the function and report warnings.
    JetTest_checkUnusedVars(parser, test);

    // Run the statement-level semantic pass on the function body.
    foreach (JetExpr*, stmt, test->body->stmts)
        JetExpr_analyse(parser, stmt, test->body, mod, NULL, false);

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {
        JetScope_lowerElementalOps(test->body);
        JetScope_promoteCandidates(test->body);
    }
}

static void JetModule_unmarkTypesVisited(JetModule* mod);
static int JetExpr_markTypesVisited(Parser* parser, JetExpr* expr);
static int JetType_checkCycles(Parser* parser, JetType* type);

void JetModule_analyse(Parser* parser, JetModule* mod) {
    // If function calls are going to be resolved based on the type of
    // first arg, then ALL functions must be visited in order to
    // generate their selectors and resolve their typespecs. (this does
    // not set the resolved flag on the func -- that is done by the
    // semantic pass)
    foreach (JetFunc*, func, mod->funcs) {
        foreach (JetVar*, arg, func->args)
            resolveTypeSpec(parser, arg->spec, mod);
        if (func->spec) resolveTypeSpec(parser, func->spec, mod);
        getSelector(func);
    }

    JetFunc* fstart = NULL;
    // don't break on the first match, keep looking so that duplicate starts
    // can be found
    foreach (JetFunc*, func, mod->funcs) {
        if (!strcmp(func->name, "start")) {
            fstart = func;
            fstart->used = 1;
        }
    }

    // If we are linting,
    //     the whole file must be analysed.this happens
    // regardless of whether start was found or not
    // if (parser->mode == PMTest || parser->mode == PMLint) {
    foreach (JetExpr*, stmt, mod->scope->stmts)
        JetExpr_analyse(parser, stmt, mod->scope, mod, NULL, false);
    // foreach (JetVar*, var, mod->scope->locals)
    //     if (var->init)
    //         JetExpr_analyse(parser, var->init, mod->scope, mod, false);
    foreach (JetTest*, test, mod->tests)
        JetTest_analyse(parser, test, mod);
    foreach (JetFunc*, func, mod->funcs)
        JetFunc_analyse(parser, func, mod);
    foreach (JetType*, type, mod->types)
        JetType_analyse(parser, type, mod);
    foreach (JetType*, en, mod->enums)
        JetType_analyse(parser, en, mod);

    // } else if (fstart) {
    /* TODO: what if you have tests and a start()? Now you will have to
     analyse the tests anyway */
    // foreach (JetExpr*, stmt, mod->scope->stmts)
    // JetExpr_analyse(parser, stmt, mod->scope, mod, NULL, false);
    // foreach (JetVar*, var, mod->scope->locals)
    //     if (var->init)
    //         JetExpr_analyse(parser, var->init, mod->scope, mod, false);

    // JetFunc_analyse(parser, fstart, mod);

    // Check dead code -- unused funcs and types, and report warnings.

    if (!fstart) { // TODO: new error, unless you want to get rid of start
        eputs("\n\e[31m*** error:\e[0m cannot find function "
              "\e[33mstart\e[0m.\n");
        parser->issues.errCount++;
    }
    foreach (JetFunc*, func, mod->funcs)
        if (!func->intrinsic
            && (!func->used || (!func->analysed && !func->isDefCtor)))
            Parser_warnUnusedFunc(parser, func);
    foreach (JetType*, type, mod->types)
        if (!type->used || !type->analysed) Parser_warnUnusedType(parser, type);

    foreach (JetFunc*, func, mod->funcs)
        JetFunc_checkRecursion(func);
    // now that funcs are marked recursive you can do a second pass analysis,
    // which deals with var storage decisions, inlining,  etc. or perhaps this
    // pass can be called 'optimising'.

    // Check each type for cycles in inheritance graph.
    // Actually if there is no inheritance and composition is favoured, you
    // have to check each statement in the type body instead of just walking
    // up the super chain. If any statements are initialized by
    // constructors, mark the type of that statement as visited and recur
    // into that type to check its statements to see if you ever revisit
    // anything. Unfortunately it does not seem that this would be easy to
    // do iteratively (not recursively), as it can be done for just checking
    // supers. foreach (JetType*, type, mod->types) {
    //     if (! type->analysed or not type->super) continue;
    //     assert(type->super->typeType == TYObject);

    //     // traverse the type hierarchy for this type and see if you
    //     revisit any JetType* superType = type->super->type; while
    //     (superType) {
    //         if (superType->visited) {
    //             Parser_errorInheritanceCycle(self, type);
    //             break;
    //         }
    //         superType->visited = true;
    //         if (! superType->super) break;
    //         assert(superType->super->typeType == TYObject);
    //         superType = superType->super->type;
    //     }

    //     // reset the cycle check flag on all types
    //     foreach (JetType*, etype, mod->types)
    //         if (type->analysed) etype->visited = false;
    // }

    // check each stmt in each type to find cycles.
    foreach (JetType*, type, mod->types)
        if (type->analysed && type->body && !type->visited) {
            if (JetType_checkCycles(parser, type)) {
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
                JetModule_unmarkTypesVisited(mod);
        }
}

// return 0 on no cycle found, -1 on cycle found
static int JetType_checkCycles(Parser* parser, JetType* type) {
    foreach (JetExpr*, stmt, type->body->stmts)
        if (JetExpr_markTypesVisited(parser, stmt)) {
            eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
                type->name, parser->filename, stmt->line, stmt->col);
            return -1;
        }
    return 0;
}

static int JetExpr_markTypesVisited(Parser* parser, JetExpr* expr) {
    JetType* type = NULL;
    if (!expr) return 0;
    switch (expr->kind) {
    case tkVarAssign: return JetExpr_markTypesVisited(parser, expr->var->init);
    case tkFunctionCall: return JetExpr_markTypesVisited(parser, expr->left);
    case tkFunctionCallResolved:
        if (JetExpr_markTypesVisited(parser, expr->left)) return -1;
        // if (expr->func->isDefCtor) type =
        // expr->func->spec->type;
        if (expr->func->spec->typeType == TYObject
            && expr->func->returnsNewObjectAlways)
            type = expr->func->spec->type;
        break;
    case tkSubscript:
    case tkSubscriptResolved:
        return JetExpr_markTypesVisited(parser, expr->left);
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
    case tkLineComment: return 0;
    default:
        if (expr->prec) {
            int ret = 0;
            if (!expr->unary)
                ret += JetExpr_markTypesVisited(parser, expr->left);
            ret += JetExpr_markTypesVisited(parser, expr->right);
            if (ret) return ret;
        } else
            unreachable("unknown expr kind: %s at %d:%d\n",
                TokenKind_names[expr->kind], expr->line, expr->col);
    }
    if (!type) return 0;
    if (type->visited) {
        Parser_errorConstructorHasCycle(parser, type);
        eprintf("%s", "\e[;2m"); // Backtrace (innermost first):\n");
        return -1;
    }
    type->visited = true;
    return JetType_checkCycles(parser, type);
}

static void JetModule_unmarkTypesVisited(JetModule* mod) {
    // reset the cycle check flag on all types
    foreach (JetType*, type, mod->types)
        type->visited = false;
}

#include "cse.h"
