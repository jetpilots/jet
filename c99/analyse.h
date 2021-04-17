
static void setStmtFuncTypeInfo(parser_t* parser, ast_func_t* func) {
    // this assumes that setExprTypeInfo has been called on the func body
    ast_expr_t* stmt = func->body->stmts->item;
    if (!func->returnSpec->typeType)
        func->returnSpec->typeType = stmt->typeType;
    else if (func->returnSpec->typeType != stmt->typeType)
        Parser_errorTypeMismatchBinOp(parser, stmt);
}

// TODO make sempassModule -> same as analyse_module now
static void analyse_type(parser_t* parser, ast_type_t* type, ast_module_t* mod);
static void analyse_func(parser_t* parser, ast_func_t* func, ast_module_t* mod);

///////////////////////////////////////////////////////////////////////////
static void analyseDictLiteral(
    parser_t* parser, ast_expr_t* expr, ast_module_t* mod) {
    // check that
    // - dict keys in the dict literal are of the same type.
    // - values are of the same type.
    // - all exprs within the commas are tk_opAssign.
    // assign the typespec pair somehow to the tk_dictLiteral expr.
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
static void prepareInterp_expr(
    parser_t* parser, ast_expr_t* expr, ast_scope_t* scope) {
    static Array(Ptr) vars;
    assert(expr->kind == tk_string || expr->kind == tk_rawString);
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
        ast_var_t* var = getVar_scope(scope, varname);

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

        ast_expr_t* ex = NEW(ast_expr_t);
        ast_expr_t* exdot = NULL;
        ex->kind = tk_identifierResolved;
        ex->line = line, ex->col = col;
        ex->var = var;
        var->used = true;

        exdot = ex;
        if (!wasbracket)
            while (endchar == '.') {
                if (!var || var->typespec->typeType != ty_object) {
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
                ast_type_t* type = var->typespec->type;
                var = getVar_type(type, varname);
                if (!var) {
                    exdot = NULL; // reset it if was set along the chain
                    Parser_errorUnrecognizedMember(parser, type,
                        &(ast_expr_t) { .string = varname,
                            .line = line,
                            .col = col,
                            .kind = tk_identifier });
                } else {
                    var->used = true;
                    strcpy(varname, var->name); // fix case
                    exdot = NEW(ast_expr_t);
                    exdot->kind = tk_period;
                    // exdot->line=line,exdot->col=ex->col+varend-varname;
                    exdot->left = ex;
                    exdot->right = NEW(ast_expr_t);
                    exdot->right->kind = tk_identifierResolved;
                    exdot->right->line = line,
                    exdot->right->col = col + varend - varname;
                    exdot->right->var = var;
                    ex = exdot;
                }
                *varend = endchar;
            }

        // Here var is the final one ie c in a.b.c

        // const char* fmtstr;
        // if (var->typespec->typeType == ty_object) {
        //     fmtstr = "%s";
        //     // you'll have to call Type_str(...) for it. Or actually why
        //     // not just do Type_print?
        // } else {
        //     fmtstr = typetype_e_format(var->typespec->typeType, false);
        // }
        // printf(",%s", var->name);
        // ^ this should be qualified name or the cgen version
        // of accessing the actual member for a.b.c

        if (exdot) exprvars = list_append(exprvars, exdot);
        pos = varend;
    }
}
//     break;
// case tk_rawString:
// case tk_number:
// case tk_identifierResolved:
// case tk_identifier:
// case tk_argumentLabel:
//     break;
// case tk_functionCallResolved:
// case tk_functionCall:
// case tk_subscript:
// case tk_objectInit:
// case tk_objectInitResolved:
// case tk_subscriptResolved:
//     if (expr->left) prepareInterp_expr(expr->left, scope);
//     break;
// case tk_varAssign:
//     prepareInterp_expr(expr->var->init, scope);
//     break;
// case tk_keyword_for:
// case tk_keyword_if:
// case tk_keyword_else:
// case tk_keyword_match:
// case tk_keyword_case:
// case tk_keyword_elif:
// case tk_keyword_while:
//     prepareInterp_expr(expr->left, scope);
//     foreach (ast_expr_t*, subexpr, expr->body->stmts)
//         prepareInterp_expr(subexpr, expr->body);
//     break;
// default:
//     if (expr->prec) {
//         if (!expr->unary) prepareInterp_expr(expr->left, scope);
//         if (expr->right) prepareInterp_expr(expr->right, scope);
//     } else {
//         unreachable("unknown token kind %s", tokenkind_e_str[expr->kind]);
//     }
// }

// #this string
//     "The quick brown fox $jumps over $the lazy dog.";
// #becomes
//     `"%s%s%s%s%s", "The quick brown fox ", jumps, " over ", the, "lazy
//     dog.",
//         ""`;

static void setEnumBase_expr(parser_t* parser, ast_expr_t* expr,
    ast_typespec_t* spec, ast_module_t* mod) {
    switch (expr->kind) {
    case tk_period:
        if (expr->left) return;
        expr->left = NEW(ast_expr_t);
        expr->left->kind = tk_identifier;
        expr->left->string
            = spec->typeType == ty_object ? spec->type->name : spec->name;
        expr->left->line = expr->line;
        expr->left->col = expr->col;
        resolveVars(parser, expr, mod->scope, false);
    case tk_opPlus:
    case tk_opComma:
    case tk_opEQ:
    case tk_opNE: setEnumBase_expr(parser, expr->left, spec, mod); fallthrough;
    case tk_opAssign:
    case tk_arrayOpen: setEnumBase_expr(parser, expr->right, spec, mod);
    default:;
    }
}

static void reduceVarUsage_expr(ast_expr_t* expr) {
    // vars and subscripts, but also function calls have their usage counts
    // reduced when being involved in an expr that inits a known unused
    // variable.
    switch (expr->kind) {
    case tk_string: break; // TODO: vars within string intrp
    case tk_subscriptResolved:
        if (expr->left) reduceVarUsage_expr(expr->left); // fallthru
    case tk_identifierResolved:
        // reduce this var's usage, and if it drops to zero reduce the usage of
        // all vars in this var's init. Sort of like a compile time ref count.
        if (!--expr->var->used) {
            if (expr->var->init) reduceVarUsage_expr(expr->var->init);
            // if (expr->var->typespec->typeType == ty_object)
            //     --expr->var->typespec->type->used;
        }
        break;
    case tk_functionCallResolved:
        if (expr->left) reduceVarUsage_expr(expr->left);
        expr->func->used--;
        break;
    case tk_period: reduceVarUsage_expr(expr->left); break;
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left) reduceVarUsage_expr(expr->left);
            if (expr->right) reduceVarUsage_expr(expr->right);
        };
    }
}

// might func be calling target directly?
static bool calls_func(ast_func_t* func, ast_func_t* target) {
    foreach (ast_func_t*, fn, func->callees) {
        if (fn == target) return true;
        eprintf(
            "callee %s of %s is not %s\n", fn->name, func->name, target->name);
    }
    return false;
}
// might func be calling target indirectly through other calls?
// static bool recurs_func(ast_func_t* func);
static void checkRecursion_func(ast_func_t* func);

// static int hasPathTo_func(ast_func_t* func, ast_func_t* target) {
//     if (calls_func(func, target)) return 1;
//     foreach (ast_func_t*, fn, func->callees) {
//         if (fn->intrinsic) continue;
//         checkRecursion_func(fn);
//         if (func->recursivity == 1) continue;
//         // ^ a nonrecurs func cannot be a path anyway.
//         if (hasPathTo_func(fn, target)) return 2;
//         eprintf("no indirect %s -> %s -> %s call\n", func->name, fn->name,
//             target->name);
//     }
//     return 0;
// }

static bool haspathto(ast_func_t* func, ast_func_t* target) {
    bool ret = false;
    if (!func->visited) {
        func->visited = true;

        foreach (ast_func_t*, fn, func->callees) {
            if (fn->intrinsic) continue;
            if (fn == target || haspathto(fn, target)) {
                ret = true;
                eputs("ok\n");
                break;
            }
            eputs("no\n");
        }

        func->visited = false;
    } else {
    }
    return ret;
}

static void checkRecursion_func(ast_func_t* func) {
    if (!func->recursivity) // 0 means not checked
        func->recursivity = 1 + haspathto(func, func);
}

///////////////////////////////////////////////////////////////////////////
static void analyse_expr(parser_t* parser, ast_expr_t* expr, ast_scope_t* scope,
    ast_module_t* mod, ast_func_t* ownerFunc, bool inFuncArgs) {
    switch (expr->kind) {

        // -------------------------------------------------- //
    case tk_functionCallResolved: {
        if (ownerFunc) { // TODO: ownerFunc should not be NULL, even top-level
                         // and type implicit ctors should have a func created
            list_shift(&ownerFunc->callees, expr->func);
            list_shift(&expr->func->callers, ownerFunc);
        }
        if (countCommaList_expr(expr->left) != expr->func->argCount)
            Parser_errorArgsCountMismatch(parser, expr);
        expr->typeType = expr->func->returnSpec
            ? expr->func->returnSpec->typeType
            : ty_noType; // should actually be ty_void
        expr->collectionType = expr->func->returnSpec
            ? expr->func->returnSpec->collectionType
            : cty_none; // should actually be ty_void
        expr->elemental = expr->func->elemental;
        if (expr->func->returnSpec) expr->dims = expr->func->returnSpec->dims;
        // isElementalFunc means the func is only defined for (all)
        // Number arguments and another definition for vector args
        // doesn't exist. Basically during typecheck this should see
        // if a type mismatch is only in terms of collectionType.
        if (!expr->left) break;
        expr->elemental = expr->elemental && expr->left->elemental;
        expr->throws = expr->left->throws || expr->func->throws;
        ast_expr_t* currArg = expr->left;
        foreach (ast_var_t*, arg, expr->func->args) {
            ast_expr_t* cArg
                = (currArg->kind == tk_opComma) ? currArg->left : currArg;
            if (cArg->kind == tk_opAssign) cArg = cArg->right;
            if (cArg->typeType != arg->typespec->typeType)
                Parser_errorArgTypeMismatch(parser, cArg, arg);
            // TODO: check dims mismatch
            // TODO: check units mismatch
            // TODO: set enum base
            if (!(currArg = currArg->right)) break;
        }

        currArg = expr->left;
        while (currArg) {
            ast_expr_t* cArg
                = (currArg->kind == tk_opComma) ? currArg->left : currArg;
            if (cArg->kind == tk_opAssign) {
                // LHS will be a tk_identifier. You should resolve it to one
                // of the function's arguments and set it to tk_argumentLabel.
                assert(cArg->left->kind == tk_identifier);
                ast_var_t* theArg = NULL;
                foreach (ast_var_t*, arg, expr->func->args) {
                    if (!strcasecmp(cArg->left->string, arg->name))
                        theArg = arg;
                }
                if (!theArg) {
                    unreachable("unresolved argument %s!", cArg->left->string);
                    // cArg->left->kind = tk_identifier;
                    // change it back to identifier
                } // TODO: change this to parser error
                else {
                    cArg->left->var = theArg;
                    cArg->left->kind = tk_identifierResolved;
                }
            }

            currArg = currArg->kind == tk_opComma ? currArg->right : NULL;
        }

    } break;

        // -------------------------------------------------- //
    case tk_functionCall: {
        char buf[128] = {};
        char* bufp = buf;
        if (expr->left)
            analyse_expr(parser, expr->left, scope, mod, ownerFunc, true);

        // TODO: need a function to make and return selector
        ast_expr_t* arg1 = expr->left;
        if (arg1 && arg1->kind == tk_opComma) arg1 = arg1->left;
        const char* typeName = typeName_expr(arg1);
        //        const char* collName = "";
        //        if (arg1)
        //        collName=collectiontype_e_nativeName(arg1->collectionType);
        if (arg1) bufp += sprintf(bufp, "%s_", typeName);
        bufp += sprintf(bufp, "%s", expr->string);
        if (expr->left)
            strarglabels_expr(expr->left, bufp, 128 - ((int)(bufp - buf)));

        ast_func_t* found = getFunc_module(mod, buf);
        if (!found) found = getFuncByTypeMatch_module(mod, expr);
        if (found) {
            expr->kind = tk_functionCallResolved;
            expr->func = found;
            expr->func->used++;
            analyse_func(parser, found, mod);
            analyse_expr(parser, expr, scope, mod, ownerFunc, inFuncArgs);
            return;
        }
        if (!strncmp(buf, "Void_", 5))
            Parser_errorCallingFuncWithVoid(parser, expr, arg1);
        else {
            Parser_errorUnrecognizedFunc(parser, expr, buf);

            if (*buf != '<') // not invalid type
            {
                int sugg = 0;
                foreach (ast_func_t*, func, mod->funcs) {
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
                        && func->argCount == list_count(func->args)) {
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
    case tk_varAssign: {
        ast_expr_t* const init = expr->var->init;
        ast_typespec_t* const typespec = expr->var->typespec;

        if (typespec->typeType == ty_unresolved)
            resolveTypeSpec(parser, typespec, mod);

        if (!init) {
            // if the typespec is given, generate the init expr yourself
            // this is only for basic types like primitives.
            // and arrays of anything can be left without an init.
            if (!typespec->dims) // goto errorMissingInit;
                switch (typespec->typeType) {
                case ty_real64: expr->var->init = expr_const_0; break;
                case ty_string: expr->var->init = expr_const_empty; break;
                case ty_bool: expr->var->init = expr_const_no; break;
                case ty_object:
                    if (!typespec->type->isEnum) {
                        expr->var->init = expr_const_nil;
                        break;
                    }
                default:
                errorMissingInit:
                    // this is an error, no way to find out what you want
                    Parser_errorMissingInit(parser, expr);
                }
        } else {
            // if (typespec->typeType == ty_unresolved)
            //     resolveTypeSpec(parser, typespec, mod);
            // first try to set enum base if applicable.
            if (typespec->typeType == ty_object && typespec->type->isEnum)
                setEnumBase_expr(parser, init, typespec, mod);

            analyse_expr(parser, init, scope, mod, ownerFunc, false);

            if (!expr->var->used) {
                // assigning to an unused var on the left of =. Decrement the
                // usage counts of all vars referenced on the RHS because well
                // being used for an unused var is not actually being used.
                // TODO: this should also be done for += -= *= etc.
                reduceVarUsage_expr(init);
            }

            if (init->typeType != ty_nilType) {
                // if not nil, get type info from init expr.
                expr->typeType = init->typeType;
                expr->collectionType = init->collectionType;
                expr->nullable = init->nullable;
                expr->elemental = init->elemental;
                expr->throws = init->throws;
                expr->impure = init->impure;
                expr->dims = init->dims;
            } else if (typespec->typeType == ty_object) {
                // if nil, and typespec given and resolved, set type info from
                // typespec.
                expr->typeType = typespec->typeType;

            } else if (typespec->typeType == ty_unresolved) {
                // this will already have caused an error while resolution.
                // since specified type is unresolved and init is nil, there
                // is nothing to do except to mark it an error type.
                typespec->typeType = expr->typeType = ty_errorType;

            } else {
                // this would be something like init'ing primitives with nil.
                Parser_errorInitMismatch(parser, expr);
                typespec->typeType = expr->typeType = ty_errorType;
            }

            if (typespec->typeType == ty_unresolved) {
                typespec->typeType = init->typeType;
                if (init->typeType == ty_object) {
                    if (init->kind == tk_functionCallResolved) {
                        expr->var->typespec = init->func->returnSpec;
                        // ^ TODO: DROP old expr->var->typespec!!
                        typespec->type = init->func->returnSpec->type;
                        typespec->dims = init->func->returnSpec->dims;
                    } else if (init->kind == tk_identifierResolved) {
                        expr->var->typespec = init->var->typespec;
                        typespec->type = init->var->typespec->type;
                        typespec->dims = init->var->typespec->dims;
                    } else if (init->kind == tk_arrayOpen)
                        typespec->type = init->elementType;
                    else if (init->kind == tk_braceOpen)
                        typespec->type = init->elementType;
                    else if (init->kind == tk_period) {
                        ast_expr_t* e = init;
                        if (init->left->var->typespec->type->isEnum) {
                            typespec->type = init->left->var->typespec->type;
                        } else {
                            while (e->kind == tk_period) e = e->right;
                            // at this point, it must be a resolved ident or
                            // subscript
                            typespec->type = e->var->typespec->type;
                            typespec->dims = e->var->typespec->dims;
                        }

                    } else {
                        unreachable("%s", "var type inference failed");
                    }
                    analyse_type(parser, typespec->type, mod);
                }
            } else if (typespec->typeType != init->typeType
                // && init->typeType != ty_nilType
            ) {
                // init can be nil, which is a ty_nilType
                Parser_errorInitMismatch(parser, expr);
                expr->typeType = ty_errorType;
            }

            if (typespec->dims == 0) {
                typespec->collectionType = init->collectionType;
                typespec->dims = init->collectionType == cty_tensor ? init->dims
                    : init->collectionType == cty_array             ? 1
                                                                    : 0;
            } else if (typespec->dims != 1
                && init->collectionType == cty_array) {
                Parser_errorInitDimsMismatch(parser, expr, 1);
                expr->typeType = ty_errorType;
            } else if (typespec->dims != 2
                && init->collectionType == cty_tensor) {
                Parser_errorInitDimsMismatch(parser, expr, 2);
                expr->typeType = ty_errorType;

            } else if (typespec->dims != 0
                && init->collectionType == cty_none) {
                Parser_errorInitDimsMismatch(parser, expr, 0);
                expr->typeType = ty_errorType;
            }
        }
    } break;

        // -------------------------------------------------- //
    case tk_keyword_match: {
        ast_expr_t* cond = expr->left;
        if (cond) {
            analyse_expr(parser, cond, scope, mod, ownerFunc, false);
            ast_typespec_t* tsp = getObjectTypeSpec_expr(cond);
            if (expr->body && tsp && tsp->type
                && tsp->type
                       ->isEnum) { // left->typeType == ty_object&&->isEnum) {
                foreach (ast_expr_t*, cas, expr->body->stmts)
                    if (cas->left)
                        setEnumBase_expr(parser, cas->left, tsp, mod);
            }
        }

        foreach (ast_expr_t*, stmt, expr->body->stmts) {
            analyse_expr(parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
            if (cond && stmt->kind == tk_keyword_case && stmt->left
                && (stmt->left->typeType != cond->typeType
                    || (cond->typeType == ty_object
                        && getTypeOrEnum_expr(stmt->left)
                            != getTypeOrEnum_expr(cond))))
                Parser_errorTypeMismatch(parser, cond, stmt->left);
        }
    } break;
    case tk_keyword_else:
    case tk_keyword_if:
    case tk_keyword_for:
    case tk_keyword_elif:
    case tk_keyword_while:
    case tk_keyword_case: {
        if (expr->left)
            analyse_expr(parser, expr->left, scope, mod, ownerFunc, false);
        foreach (ast_expr_t*, stmt, expr->body->stmts)
            analyse_expr(parser, stmt, expr->body, mod, ownerFunc, inFuncArgs);
    } break;

        // -------------------------------------------------- //
    case tk_subscriptResolved: {
        // assert(expr->left->kind == tk_arrayOpen);
        int nhave = countCommaList_expr(expr->left);
        if (nhave != expr->var->typespec->dims)
            Parser_errorIndexDimsMismatch(parser, expr, nhave);
    }
        // fallthru
    case tk_subscript:
        if (expr->left)
            analyse_expr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
        if (expr->kind == tk_subscriptResolved) {
            expr->typeType = expr->var->typespec->typeType;
            expr->collectionType = expr->var->typespec->collectionType;
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
            // analyse_expr for a : op should set dims to 1. Then you just
            // walk the comma op in expr->right here and check which index exprs
            // have dims=0. Special case is when the index expr is a logical,
            // meaning you are filtering the array, in this case the result's
            // dims is always 1.
        }
        break;

    case tk_string:
    case tk_rawString:
        expr->typeType = ty_string;
        prepareInterp_expr(parser, expr, scope);
        break;

    case tk_number: expr->typeType = ty_real64; break;

    case tk_keyword_yes:
    case tk_keyword_no: expr->typeType = ty_bool; break;

    case tk_keyword_nil: expr->typeType = ty_nilType; break;

    case tk_identifier:
        // by the time analysis is called, all vars must have been resolved
        // Parser_errorUnrecognizedVar(parser, expr);
        // func call labels, enum names etc remain identifiers
        expr->typeType = ty_errorType;
        break;

    case tk_identifierResolved:
        expr->typeType = expr->var->typespec->typeType;
        expr->collectionType = expr->var->typespec->collectionType;
        expr->elemental = expr->collectionType != cty_none;
        expr->dims = expr->var->typespec->dims;

        // this was done just to ensure enums are caught and analysed in
        // non-format mode. In format mode they are done anyway.
        // TODO: remove this, you shouldnt be doing it on each var use it will
        // be too intensive. let it just be done on each var decl and you figure
        // out a way to set the enum type on var decls.
        if (expr->typeType == ty_object)
            analyse_type(parser, expr->var->typespec->type, mod);
        break;

    case tk_arrayOpen:
        expr->collectionType = cty_array;
        if (expr->right) {
            analyse_expr(
                parser, expr->right, scope, mod, ownerFunc, inFuncArgs);
            expr->typeType = expr->right->typeType;
            expr->collectionType
                = expr->right->kind == tk_opSemiColon ? cty_tensor : cty_array;
            expr->dims = expr->right->kind == tk_opSemiColon ? 2 : 1;
            // using array literals you can only init 1D or 2D
            if (expr->typeType == ty_object) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                ast_expr_t* first = expr->right;
                while (
                    first->kind == tk_opComma || first->kind == tk_opSemiColon)
                    first = first->left;
                switch (first->kind) {
                case tk_identifierResolved:
                    expr->elementType = first->var->typespec->type;
                    if (first->var->typespec->dims
                        || first->var->typespec->collectionType != cty_none)
                        unreachable(
                            "trying to make array of arrays %d", expr->line);
                    break;
                case tk_functionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    if (first->func->returnSpec->dims
                        || first->var->typespec->collectionType != cty_none)
                        unreachable("trying to make array of arrays line %d",
                            expr->line);
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tk_objectInitResolved:
                    // expr->elementType = first->var->typespec->type;break;
                }
                // expr->elementType =
            }
        }
        break;

    case tk_braceOpen:
        if (expr->right) {
            analyse_expr(parser, expr->right, scope, mod, ownerFunc, true);
            // TODO: you told analyse_expr to not care about what's on the
            // LHS of tk_opAssign exprs. Now you handle it yourself. Ensure that
            // they're all of the same type and set that type to the expr
            // somehow.
            analyseDictLiteral(parser, expr->right, mod);
            expr->typeType = expr->right->typeType;
            if (expr->typeType == ty_object) {
                // you need to save the exact type of the elements, it's not a
                // primitive type. You'll find it in the first element.
                ast_expr_t* first = expr->right;
                while (first->kind == tk_opComma) first = first->left;
                if (first->kind == tk_opAssign) first = first->right;
                // we care about the value in the key-value pair. We'll figure
                // out the key type later or not, whatever.
                switch (first->kind) {
                case tk_identifierResolved:
                    expr->elementType = first->var->typespec->type;
                    break;
                case tk_functionCallResolved:
                    expr->elementType = first->func->returnSpec->type;
                    break;
                default:
                    break;
                    // TODO: object init literals
                    // case tk_objectInitResolved:
                    // expr->elementType = first->var->typespec->type;break;
                }
            }
            expr->collectionType = cty_dictS;
            // these are only Dicts! Sets are normal [] when you detect they are
            // only used for querying membership.
            // TODO: what were the gazillion Dict subtypes for?
        }
        break;
    case tk_period: {

        if (!expr->left) {
            Parser_errorNoEnumInferred(parser, expr->right);
            break;
        }

        assert(expr->left->kind == tk_identifierResolved
            || expr->left->kind == tk_identifier);
        analyse_expr(parser, expr->left, scope, mod, ownerFunc, inFuncArgs);

        // The name/type resolution of expr->left may have failed.
        if (!expr->left->typeType) break;

        ast_expr_t* member = expr->right;
        if (member->kind == tk_period) {
            member = member->left;
            if (member->kind != tk_identifier) {
                Parser_errorUnexpectedExpr(parser, member);
                break;
            }
        }

        if (!ISIN(3, member->kind, tk_identifier, tk_subscript,
                tk_functionCall)) {
            Parser_errorUnexpectedExpr(parser, member);
            break;
        }
        //  or member->kind == tk_functionCall);
        if (member->kind != tk_identifier && member->left)
            analyse_expr(
                parser, member->left, scope, mod, ownerFunc, inFuncArgs);

        // the left must be a resolved ident
        if (expr->left->kind != tk_identifierResolved) break;

        if (expr->left->var->typespec->typeType == ty_errorType) {
            expr->typeType = ty_errorType;
            break;
        }
        assert(expr->left->var->typespec->typeType != ty_nilType);

        ast_type_t* type = expr->left->var->typespec->type;
        if (!type) {
            expr->typeType = ty_errorType;
            break;
        }

        // Resolve the member in the scope of the type definition.
        resolveMember(parser, member, type);
        // Name resolution may fail...
        if (member->kind != tk_identifierResolved) {
            expr->typeType = ty_errorType;
            break;
        }
        analyse_expr(parser, member, scope, mod, ownerFunc, inFuncArgs);

        if (expr->right->kind == tk_period)
            analyse_expr(
                parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

        expr->typeType = type->isEnum ? ty_object : expr->right->typeType;
        expr->collectionType = expr->right->collectionType;
        expr->elemental = expr->right->elemental;
        expr->dims = expr->right->dims;
    } break;

    case tk_lineComment: break;

    case tk_argumentLabel:

        break;

        //    case tk_rawString:
        // TODO: analyse regex, compile it already, whatever
        //        break;
        // -------------------------------------------------- //
        // case tk_keyword_in:
        // case tk_keyword_notin:
        // these ops may take an array of enums, so set their base type.
        // if (expr->left->kind==tk_identifierResolved)

        // there's some work being done on these as standard binops, so
        // fallthrough;
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left)
                analyse_expr(
                    parser, expr->left, scope, mod, ownerFunc, inFuncArgs);
            // some exprs like return can be used without any args

            if (ISIN(5, expr->kind, tk_keyword_in, tk_keyword_notin,
                    tk_opAssign, tk_opEQ, tk_opNE)) {
                ast_typespec_t* spec = getObjectTypeSpec_expr(expr->left);
                if (spec && spec->typeType == ty_object && spec->type->isEnum) {
                    setEnumBase_expr(parser, expr->right, spec, mod);
                } else {
                    spec = getObjectTypeSpec_expr(expr->left);
                    if (spec && spec->typeType == ty_object
                        && spec->type->isEnum) {
                        setEnumBase_expr(parser, expr->right, spec, mod);
                    }
                }
            }

            if (expr->right)
                analyse_expr(
                    parser, expr->right, scope, mod, ownerFunc, inFuncArgs);

            if (expr->kind == tk_keyword_or
                && expr->left->typeType != ty_bool) {
                // Handle the 'or' keyword used to provide alternatives for
                // a nullable expression.
                ;
            } else if (isCmpOp(expr) || isBoolOp(expr)) {
                // Handle comparison and logical operators (always return a
                // bool)
                expr->typeType
                    = (expr->right->typeType == ty_errorType
                          || (!expr->unary
                              && expr->left->typeType == ty_errorType))
                    ? ty_errorType
                    : ty_bool;
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
                    = expr->right->elemental || expr->kind == tk_opColon;
            // TODO: actually, indexing by an array of integers is also an
            // indication of an elemental op

            if (!expr->unary && expr->left) {
                expr->elemental = expr->elemental || expr->left->elemental;
                expr->dims = expr->right->dims;
                // if (expr->kind == tk_opColon and expr->right->kind ==
                // tk_opColon)
                //     expr->right->dims = 0; // temporarily set it to 0 to
                //     allow
                // parsing stepped ranges 1:s:n

                if (expr->dims != expr->left->dims
                    && !(inFuncArgs
                        && (expr->kind == tk_opComma
                            || expr->kind == tk_opAssign))) {
                    // if either one has 0 dims (scalar) it is an elemental op
                    // with a scalar.
                    if (expr->left->dims != 0 && expr->right->dims != 0) {
                        Parser_errorBinOpDimsMismatch(parser, expr);
                        expr->right->typeType = ty_errorType;
                    } else if (expr->kind == tk_opPlus //
                        || expr->kind == tk_opMinus //
                        || expr->kind == tk_opTimes //
                        || expr->kind == tk_opSlash //
                        || expr->kind == tk_opPower //
                        || expr->kind == tk_opMod) {
                        expr->dims = expr->left->dims + expr->right->dims;
                        expr->collectionType = max(expr->left->collectionType,
                            expr->right->collectionType);
                        // todo: stop distinguishing array and tensor!!! then
                        // you dont need this. this strongly depends on the op &
                        // is too much repeated work
                        // eprintf("ok `[+-*/^%]` dims@ %d %d %d %d\n",
                        // expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                    } else if ((expr->kind == tk_keyword_in
                                   || expr->kind == tk_keyword_notin)
                        && expr->left->dims == 0 && expr->right->dims == 1) {
                        // eprintf("ok `in` dims@ %d %d %d %d\n", expr->line,
                        //     expr->col, expr->left->dims, expr->right->dims);
                    } else if (expr->kind == tk_opColon //
                        && expr->left->dims == 1
                        && expr->left->kind == tk_opColon
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
                if (expr->kind == tk_opColon) expr->dims = 1;
            }
            if (!expr->unary && expr->left
                && !(inFuncArgs
                    && (expr->kind == tk_opComma
                        || expr->kind == tk_opAssign))) {
                // ignore , and = inside function call arguments. thing is
                // array or dict literals passed as args will have , and =
                // which should be checked. so when you descend into their
                // args, unset inFuncArgs.
                typetype_e leftType = expr->left->typeType;
                typetype_e rightType = expr->right->typeType;

                if (leftType == ty_bool
                    && (expr->kind == tk_opLE || expr->kind == tk_opLT)) {
                    // Special case: chained LE/LT operators: e.g. 0 <= yCH4
                    // <= 1.
                    ;
                } else if (leftType != rightType) {
                    // Type mismatch for left and right operands is always
                    // an error.
                    Parser_errorTypeMismatchBinOp(parser, expr);
                    expr->typeType = ty_errorType;
                } else if (leftType == ty_string
                    && (expr->kind == tk_opAssign || expr->kind == tk_opEQ
                        || expr->kind == tk_opNE)) {
                    // Allow assignment, equality test and != for strings.
                    // TODO: might even allow comparison operators, and
                    // perhaps allow +=, or better .= or something. Or
                    // perhaps append(x!) is clearer
                    ;
                } else if (leftType == ty_bool
                    && (expr->kind == tk_opAssign //
                        || expr->kind == tk_opEQ //
                        || expr->kind == tk_keyword_and //
                        || expr->kind == tk_keyword_or
                        || expr->kind == tk_keyword_not
                        || expr->kind == tk_keyword_notin))
                    ;
                else if (isArithOp(expr)
                    && (!typetype_e_isnum(leftType)
                        || !typetype_e_isnum(rightType))) {
                    // Arithmetic operators are only relevant for numeric
                    // types.
                    // if(  leftType==ty_object&&expr->left->)
                    // TODO: allow enum +
                    Parser_errorInvalidTypeForOp(parser, expr);
                }
                // check if an error type is on the left, if yes, set the
                // expr type
                if (leftType == ty_errorType) expr->typeType = leftType;
            }
            // TODO: here statements like return etc. that are not binary
            // but need to have their types checked w.r.t. an expected type
            // TODO: some ops have a predefined type e.g. : is of type Range
            // etc,
        } else {
            unreachable("unknown expr kind: %s", tokenkind_e_names[expr->kind]);
        }
    }
}

///////////////////////////////////////////////////////////////////////////
static void analyse_type(
    parser_t* parser, ast_type_t* type, ast_module_t* mod) {
    if (type->analysed) return;
    // eprintf(
    //     "analyse_expr: %s at ./%s:%d\n", type->name, parser->filename,
    //     type->line);
    if (type->super) {
        resolveTypeSpec(parser, type->super, mod);
        if (type->super->type == type)
            Parser_errorTypeInheritsSelf(parser, type);
    }
    // TODO: this should be replaced by a dict query
    foreach (ast_type_t*, type2, mod->types) {
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
        foreach (ast_expr_t*, stmt, type->body->stmts)
            analyse_expr(parser, stmt, type->body, mod, NULL, false);
}
static void hashExprs_func(parser_t* parser, ast_func_t* func);

///////////////////////////////////////////////////////////////////////////
static void analyse_func(
    parser_t* parser, ast_func_t* func, ast_module_t* mod) {
    if (func->analysed) return;
    // eprintf("analyse_expr: %s at ./%s:%d\n", func->selector,
    // parser->filename, func->line);

    bool isCtor = false;
    // Check if the function is a constructor call and identify the type.
    // TODO: this should be replaced by a dict query
    foreach (ast_type_t*, type, mod->types) {
        if (!strcasecmp(func->name, type->name)) {
            if (func->returnSpec && !(func->isStmt || func->isDefCtor))
                Parser_errorCtorHast_ype(parser, func, type);
            if (!func->returnSpec) {
                func->returnSpec = new_typespec(ty_object, cty_none);
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
            //     Parser_errorCtorHast_ype(this, func, type);
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
    foreach (ast_func_t*, func2, mod->funcs) {
        if (func == func2) break;
        if (!strcasecmp(func->selector, func2->selector))
            Parser_errorDuplicateFunc(parser, func, func2);
    }

    // Mark the semantic pass as done for this function, so that recursive
    // calls found in the statements will not cause the compiler to recur.
    func->analysed = true;

    // Run the statement-level semantic pass on the function body.
    foreach (ast_expr_t*, stmt, func->body->stmts)
        analyse_expr(parser, stmt, func->body, mod, func, false);

    // Check unused variables in the function and report warnings.
    checkUnusedVars_func(parser, func);
    // Statement functions are written without an explicit return type.
    // Figure out the type (now that the body has been analyzed).
    if (func->isStmt) setStmtFuncTypeInfo(parser, func);
    // TODO: for normal funcs, analyse_expr should check return statements
    // to have the same type as the declared return type.

    // this is done here so that formater can give hints on what is picked up
    // for CSE. This func should not modify the AST except marking CSE
    // candidates!
    hashExprs_func(parser, func);

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {

        // Handle elemental operations like arr[4:50] = mx[14:60] + 3
        lowerElementalOps_scope(func->body);
        // Extract subexprs like count(arr[arr<1e-15]) and promote them to
        // full statements corresponding to their C macros e.g.
        // Number _1; Array_count_filter(arr, arr<1e-15, _1);
        promoteCandidates_scope(func->body);
    }
}

static void analyse_test(
    parser_t* parser, ast_test_t* test, ast_module_t* mod) {
    if (!test->body) return;

    // Check for duplicate test names and report errors.
    // TODO: this should be replaced by a dict query
    foreach (ast_test_t*, test2, mod->tests) {
        if (test == test2) break;
        if (!strcasecmp(test->name, test2->name))
            Parser_errorDuplicateTest(parser, test, test2);
    }

    // Check unused variables in the function and report warnings.
    checkUnusedVars_test(parser, test);

    // Run the statement-level semantic pass on the function body.
    foreach (ast_expr_t*, stmt, test->body->stmts)
        analyse_expr(parser, stmt, test->body, mod, NULL, false);

    // Do optimisations or ANY lowering only if there are no errors
    if (!parser->issues.errCount && parser->mode != PMLint) {
        lowerElementalOps_scope(test->body);
        promoteCandidates_scope(test->body);
    }
}

static void unmarkTypesVisited_module(ast_module_t* mod);
static int markTypesVisited_expr(parser_t* parser, ast_expr_t* expr);
static int checkCycles_type(parser_t* parser, ast_type_t* type);

void analyse_module(parser_t* parser, ast_module_t* mod) {
    // If function calls are going to be resolved based on the type of
    // first arg, then ALL functions must be visited in order to
    // generate their selectors and resolve their typespecs. (this does
    // not set the resolved flag on the func -- that is done by the
    // semantic pass)
    foreach (ast_func_t*, func, mod->funcs) {
        foreach (ast_var_t*, arg, func->args)
            resolveTypeSpec(parser, arg->typespec, mod);
        if (func->returnSpec) resolveTypeSpec(parser, func->returnSpec, mod);
        getSelector(func);
    }

    ast_func_t* fstart = NULL;
    // don't break on the first match, keep looking so that duplicate starts
    // can be found
    foreach (ast_func_t*, func, mod->funcs) {
        if (!strcmp(func->name, "start")) {
            fstart = func;
            fstart->used = 1;
        }
    }

    // If we are formating,
    //     the whole file must be analysed.this happens
    // regardless of whether start was found or not
    // if (parser->mode == PMTest || parser->mode == PMLint) {
    foreach (ast_expr_t*, stmt, mod->scope->stmts)
        analyse_expr(parser, stmt, mod->scope, mod, NULL, false);
    // foreach (ast_var_t*, var, mod->scope->locals)
    //     if (var->init)
    //         analyse_expr(parser, var->init, mod->scope, mod, false);
    foreach (ast_test_t*, test, mod->tests)
        analyse_test(parser, test, mod);
    foreach (ast_func_t*, func, mod->funcs)
        analyse_func(parser, func, mod);
    foreach (ast_type_t*, type, mod->types)
        analyse_type(parser, type, mod);
    foreach (ast_type_t*, en, mod->enums)
        analyse_type(parser, en, mod);

    // } else if (fstart) {
    /* TODO: what if you have tests and a start()? Now you will have to
     analyse the tests anyway */
    // foreach (ast_expr_t*, stmt, mod->scope->stmts)
    // analyse_expr(parser, stmt, mod->scope, mod, NULL, false);
    // foreach (ast_var_t*, var, mod->scope->locals)
    //     if (var->init)
    //         analyse_expr(parser, var->init, mod->scope, mod, false);

    // analyse_func(parser, fstart, mod);

    // Check dead code -- unused funcs and types, and report warnings.

    if (!fstart) { // TODO: new error, unless you want to get rid of start
        eputs("\n\e[31m*** error:\e[0m cannot find function "
              "\e[33mstart\e[0m.\n");
        parser->issues.errCount++;
    }
    foreach (ast_func_t*, func, mod->funcs)
        if (!func->intrinsic
            && (!func->used || (!func->analysed && !func->isDefCtor)))
            Parser_warnUnusedFunc(parser, func);
    foreach (ast_type_t*, type, mod->types)
        if (!type->used || !type->analysed) Parser_warnUnusedType(parser, type);

    foreach (ast_func_t*, func, mod->funcs)
        checkRecursion_func(func);
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
    // supers. foreach (ast_type_t*, type, mod->types) {
    //     if (! type->analysed or not type->super) continue;
    //     assert(type->super->typeType == ty_object);

    //     // traverse the type hierarchy for this type and see if you
    //     revisit any ast_type_t* superType = type->super->type; while
    //     (superType) {
    //         if (superType->visited) {
    //             Parser_errorInheritanceCycle(self, type);
    //             break;
    //         }
    //         superType->visited = true;
    //         if (! superType->super) break;
    //         assert(superType->super->typeType == ty_object);
    //         superType = superType->super->type;
    //     }

    //     // reset the cycle check flag on all types
    //     foreach (ast_type_t*, etype, mod->types)
    //         if (type->analysed) etype->visited = false;
    // }

    // check each stmt in each type to find cycles.
    foreach (ast_type_t*, type, mod->types)
        if (type->analysed && type->body && !type->visited) {
            if (checkCycles_type(parser, type)) {
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
                unmarkTypesVisited_module(mod);
        }
}

// return 0 on no cycle found, -1 on cycle found
static int checkCycles_type(parser_t* parser, ast_type_t* type) {
    foreach (ast_expr_t*, stmt, type->body->stmts)
        if (markTypesVisited_expr(parser, stmt)) {
            eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
                type->name, parser->filename, stmt->line, stmt->col);
            return -1;
        }
    return 0;
}

static int markTypesVisited_expr(parser_t* parser, ast_expr_t* expr) {
    ast_type_t* type = NULL;
    if (!expr) return 0;
    switch (expr->kind) {
    case tk_varAssign: return markTypesVisited_expr(parser, expr->var->init);
    case tk_functionCall: return markTypesVisited_expr(parser, expr->left);
    case tk_functionCallResolved:
        if (markTypesVisited_expr(parser, expr->left)) return -1;
        // if (expr->func->isDefCtor) type =
        // expr->func->returnSpec->type;
        if (expr->func->returnSpec->typeType == ty_object
            && expr->func->returnsNewObjectAlways)
            type = expr->func->returnSpec->type;
        break;
    case tk_subscript:
    case tk_subscriptResolved:
        return markTypesVisited_expr(parser, expr->left);
        // case tk_keyword_if:
        // case tk_keyword_elif:
        // case tk_keyword_case:
        // case tk_keyword_match:

    case tk_identifierResolved:
    case tk_string:
    case tk_identifier:
    case tk_keyword_no:
    case tk_keyword_yes:
    case tk_keyword_nil:
    case tk_number:
    case tk_rawString:
    case tk_lineComment: return 0;
    default:
        if (expr->prec) {
            int ret = 0;
            if (!expr->unary) ret += markTypesVisited_expr(parser, expr->left);
            ret += markTypesVisited_expr(parser, expr->right);
            if (ret) return ret;
        } else
            unreachable("unknown expr kind: %s at %d:%d\n",
                tokenkind_e_names[expr->kind], expr->line, expr->col);
    }
    if (!type) return 0;
    if (type->visited) {
        Parser_errorConstructorHasCycle(parser, type);
        eprintf("%s", "\e[;2m"); // Backtrace (innermost first):\n");
        return -1;
    }
    type->visited = true;
    return checkCycles_type(parser, type);
}

static void unmarkTypesVisited_module(ast_module_t* mod) {
    // reset the cycle check flag on all types
    foreach (ast_type_t*, type, mod->types)
        type->visited = false;
}

#include "cse.h"
