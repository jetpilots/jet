
static bool isSelfMutOp(ast_expr_t* expr) {
    return expr->kind > __tk__selfMutOps__begin
        && expr->kind < __tk__selfMutOps__end;
    //  expr->kind == tk_plusEq //
    //     || expr->kind == tk_minusEq //
    //     || expr->kind == tk_slashEq //
    //     || expr->kind == tk_timesEq //
    //     || expr->kind == tk_powerEq //
    //     || expr->kind == tk_opModEq //
    //     || expr->kind == tk_opAssign;
}

static bool isArithOp(ast_expr_t* expr) {
    return expr->kind > __tk__arithOps__begin
        && expr->kind < __tk__arithOps__end;
    // == tk_plusEq //
    //     || expr->kind == tk_minusEq //
    //     || expr->kind == tk_slashEq //
    //     || expr->kind == tk_timesEq //
    //     || expr->kind == tk_powerEq //
    //     || expr->kind == tk_opModEq //
    //     || expr->kind == tk_plus || expr->kind == tk_minus
    //     || expr->kind == tk_slash || expr->kind == tk_times
    //     || expr->kind == tk_power || expr->kind == tk_opMod;
}

// isSelfMutOp(expr as ast_expr_t) := expr.kind in [
//     .plusEq, .minusEq, .slashEq, .timesEq, .opModEq, .opAssign
// ]

// function resolve(typespec as ast_typespec_t, mod as ast_module_t, par as
// parser_t)
//     if typespec.typeType != .unresolved then return
//     if typespec.name == "" then return
//     var tyty = typeTypeByName(typespec.name)
//     if tyty != .unresolved
//         typespec.typeType = tyty
//     else
//         type = lookupType(typespec.name, module = mod)
//         typespec.typeType = .object
//         typespec.type = type
//     end if
//     on error . itemNotFound
//         errorUnrecognized(typespec, parser = parser)
// end function

static void resolveTypeSpec(
    parser_t* parser, ast_typespec_t* typespec, ast_module_t* mod) {
    // TODO: disallow a type that derives from itself!
    if (typespec->typeType != ty_unresolved) return;
    if (!*typespec->name) return;

    // TODO: DO THIS IN PARSE... stuff!!

    typetype_e tyty = typetype_e_byName(typespec->name);
    if (tyty) { // can be member of ast_typespec_t!
        typespec->typeType = tyty;
    } else {
        ast_type_t* type = getType_module(mod, typespec->name);
        if (type) {
            typespec->typeType = ty_object;
            typespec->type = type;
            type->used++;
            return;
        }
        Parser_errorUnrecognizedType(parser, typespec);
        return;
    }
    if (typespec->dims) {
        // set collection type, etc.
        // for this we will need info about the var and its usage
        // patterns. so this will probably be a separate func that is
        // called during such analysis.
    }
}
// function checkUnusedVars(scope as ast_scope_t, parser as parser_t)
//     for var = scope.locals
//
//     end for
// end function

static void checkUnusedVars_scope(parser_t* parser, ast_scope_t* scope) {
    foreach (ast_var_t*, var, scope->locals)
        if (!var->used) Parser_warnUnusedVar(parser, var);

    foreach (ast_expr_t*, stmt, scope->stmts)
        if (isCtrlExpr(stmt) && stmt->body)
            checkUnusedVars_scope(parser, stmt->body);
}

static void checkUnusedVars_func(parser_t* parser, ast_func_t* func) {
    foreach (ast_var_t*, arg, func->args)
        if (!arg->used) Parser_warnUnusedArg(parser, arg);

    checkUnusedVars_scope(parser, func->body);
}

static void checkUnusedVars_test(parser_t* parser, ast_test_t* test) {
    checkUnusedVars_scope(parser, test->body);
}

// TODO: Btw there should be a module level scope to hold lets (and
// comments). That will be the root scope which has parent==NULL.

static void resolveMember(
    parser_t* parser, ast_expr_t* expr, ast_type_t* type) {
    assert(expr->kind == tk_identifier || expr->kind == tk_subscript);
    tokenkind_e ret = (expr->kind == tk_identifier) ? tk_identifierResolved
                                                    : tk_subscriptResolved;
    ast_var_t* found = NULL;
    if (type->body) found = getVar_scope(type->body, expr->string);
    if (found) {
        expr->kind = ret;
        expr->var = found;
        expr->var->used++;
    } else {
        Parser_errorUnrecognizedMember(parser, type, expr);
    }
}

// This function is called in one pass, during the line-by-line parsing.
// (since variables cannot be "forward-declared").
static void resolveVars(parser_t* parser, ast_expr_t* expr, ast_scope_t* scope,
    bool inFuncCall) { // TODO: this could be done on rpn in parseExpr, making
                       // it iterative instead of recursive = behaves
                       // differently inside a func call: the ->left is not
                       // resolved to a var, but to an argument label of the
                       // called func. it would make sense to just skip checking
                       // it here for now, and let resolveFuncs use it to
                       // construct the func selector for lookup. At some point
                       // though it would be nice if the compiler could tell the
                       // user 'you missed the arg label "xyz"' for which the
                       // basename of the func could be used to get a list of
                       // all selectors having that basename as a prefix.

    if (!expr) return;
    switch (expr->kind) {
    case tk_identifierResolved:
    case tk_subscriptResolved:
        expr->var->last_usage = expr->line;
        // ^ TODO: actually the line to set is not expr->line, but the line
        // number of the toplevel expr. In the new code you pass parent expr
        // into analyse & resolve, use that to walk up to the toplevel and find
        // its line number. This matters for statements like:
        // var g =
        //    1 + call(2,
        //             4,
        //             h)
        // when you later want to insert drop calls, you have to find the expr
        // with the corresponding line (==, not >= since order can be changed)
        // The stmt list will have the line number corresponding to the varDecl
        // line and not the actual line on which h appears.
        break;

    case tk_identifier:
    case tk_subscript: {
        tokenkind_e ret = (expr->kind == tk_identifier) ? tk_identifierResolved
                                                        : tk_subscriptResolved;
        ast_var_t* found = getVar_scope(scope, expr->string);
        if (found) {
            expr->kind = ret;
            expr->var = found;
            expr->var->used++;
            expr->var->last_usage = expr->line; // see above for a TODO and info
        } else {
            // ast_import_t* import = getImportByAlias_module(mod,
            // expr->string); if (import) {
            //     expr->kind = tk_keyword_import;
            //     expr->import = import;
            //     import->used = true;
            // } else {
            Parser_errorUnrecognizedVar(parser, expr);
            // }
        }
        if (expr->kind == tk_subscriptResolved || expr->kind == tk_subscript) {
            resolveVars(parser, expr->left, scope, inFuncCall);
            // check subscript argument count
            // recheck kind since the var may have failed resolution
            // TODO: handle dims 0 as dims 1 because arr[] is the same as arr[:]
            //            if (expr->kind == tk_subscriptResolved
            //                and countCommaList_expr(expr->left)
            //                    != expr->var->typespec->dims)
            //                Parser_errorIndexDimsMismatch(parser, expr);
            // do it in analysis after type/dims inference
        }
        break;
    }
    case tk_functionCall:
        if (expr->left) resolveVars(parser, expr->left, scope, true);
        break;

    case tk_period:
        if (expr->left) resolveVars(parser, expr->left, scope, inFuncCall);
        // expr->right is not to be resolved in the same scope, but in
        // the type body of the type of expr->left. So you cannot call
        // resolveVars on expr->right from here. Neither can you assume
        // that types have been resolved, because name resolution
        // happens as the file is read, while type resolution happens
        // only after the tree is fully built. The way to fix it is to
        // have analyseExpr call the name resolution (as it already does for
        // exprs like a.b but not a.b.c) for expr->right, AFTER the type
        // for expr->left has been resolved.
        //        if (expr->right->kind==tk_period) resolveVars(this,
        //        expr->right, scope, inFuncCall);
        // besides an ident, the ->right of a . can be either another
        // dot, a subscript, or a func call if we allow member funcs
        if (expr->right->kind == tk_subscript
            || expr->right->kind == tk_subscriptResolved)
            resolveVars(parser, expr->right->left, scope, inFuncCall);

        break;

    case tk_string: {
        // strings may have embedded variable names of the form $name or
        // $(name), so resolve them. by the time this func is called, the string
        // will be null-terminated
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
                    //            eprintf("lookup '%s'\n", buf);
                    ast_var_t* var = getVar_scope(scope, buf);
                    if (!var) {
                        char* orig = expr->string;
                        expr->string = buf;
                        expr->col += (pos - orig) + 1;
                        Parser_errorUnrecognizedVar(parser, expr);
                        expr->col -= (pos - orig) + 1;
                        expr->string = orig;
                    } else {
                        // we're not going to actually "resolve" the embedded
                        // var, just format the name here so it is in the
                        // correct case
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
                if (inFuncCall && expr->kind == tk_opAssign) {
                    // expr->left->kind = tk_argumentLabel;
                    // Here you only set the type of the expr to
                    // tk_argumentLabel. It will be resolved later in analyse(),
                    // when the functions have been resolved. Maybe this should
                    // also be moved there then.
                } else {
                    resolveVars(parser, expr->left, scope, inFuncCall);
                }
            }
            resolveVars(parser, expr->right, scope, inFuncCall);

            if (isSelfMutOp(expr)) {
                ast_var_t* var = NULL;
                ast_expr_t* varExpr = expr->left;
                if (varExpr) {
                    if (varExpr->kind == tk_identifierResolved || //
                        varExpr->kind == tk_subscriptResolved) {
                        var = varExpr->var;
                    } else if (varExpr->kind == tk_period && //
                        varExpr->left
                        && varExpr->left->kind == tk_identifierResolved) {
                        varExpr = varExpr->left;
                        var = varExpr->var;
                    }
                }
                if (var) {
                    // TODO: If you will allow changing the first arg of a
                    // function, using an & op or whatever, check for those
                    // mutations here BTW this marks entire arrays as used
                    // for a.x = ... it marks all of a as used. Instead should
                    // traverse the . sequence left to right and check which the
                    // first read-only variable in that sequence
                    var->changed++;
                    if (expr->kind == tk_opAssign) var->reassigned = true;
                    if (!var->isVar) Parser_errorReadOnlyVar(parser, varExpr);
                }
            }
        }
    }
}
