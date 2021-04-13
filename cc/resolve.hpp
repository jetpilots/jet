

// isSelfMutOp(expr as ASTExpr) := expr.kind in [
//     .plusEq, .minusEq, .slashEq, .timesEq, .opModEq, .opAssign
// ]

// function resolve(typeSpec as ASTTypeSpec, mod as ASTModule, par as Parser)
//     if typeSpec.typeType != .unresolved then return
//     if typeSpec.name == "" then return
//     var tyty = typeTypeByName(typeSpec.name)
//     if tyty != .unresolved
//         typeSpec.typeType = tyty
//     else
//         type = lookupType(typeSpec.name, module = mod)
//         typeSpec.typeType = .object
//         typeSpec.type = type
//     end if
//     on error .itemNotFound
//         errorUnrecognized(typeSpec, parser = parser)
// end function

static void resolveTypeSpec(
    Compiler& com, ASTTypeSpec* typeSpec, ASTModule* mod) {
    // TODO: disallow a type that derives from itself!
    if (typeSpec.typeType != TYUnresolved) return;
    if (!*typeSpec.name) return;

    // TODO: DO THIS IN PARSE... stuff!!

    TypeTypes tyty = TypeType_byName(typeSpec.name);
    if (tyty) { // can be member of ASTTypeSpec!
        typeSpec.typeType = tyty;
    } else {
        ASTType& type = getType(mod, typeSpec.name);
        if (type) {
            typeSpec.typeType = TYObject;
            typeSpec.type = type;
            return;
        }
        errorUnrecognizedType(parser, typeSpec);
        return;
    }
    if (typeSpec.dims) {
        // set collection type, etc.
        // for this we will need info about the var and its usage
        // patterns. so this will probably be a separate func that is
        // called during such analysis.
    }
}
// function checkUnusedVars(scope as ASTScope, parser as Parser)
//     for var = scope.locals
//
//     end for
// end function

static void checkUnusedVars(Compiler& com, ASTScope& scope) {
    for (ASTVar& var : scope.locals)
        if (!var.used) warnUnusedVar(parser, var);

    for (ASTExpr& stmt : scope.stmts)
        if (isCtrlExpr(stmt) && stmt.body) checkUnusedVars(parser, stmt.body);
}

static void checkUnusedVars(Compiler& com, ASTFunc& func) {
    for (ASTVar& arg : func.args)
        if (!arg.used) warnUnusedArg(parser, arg);

    checkUnusedVars(parser, func.body);
}

static void checkUnusedVars(Compiler& com, ASTTest& test) {
    checkUnusedVars(parser, test.body);
}

// TODO: Btw there should be a module level scope to hold lets (and
// comments). That will be the root scope which has parent==NULL.

static void resolveMember(Compiler& com, ASTExpr& expr, ASTType& type) {
    assert(expr.kind == tkIdentifier || expr.kind == tkSubscript);
    TokenKind ret = (expr.kind == tkIdentifier) ? tkIdentifierResolved
                                                : tkSubscriptResolved;
    ASTVar* found = NULL;
    if (type.body) found = getVar(type.body, expr.string);
    if (found) {
        expr.kind = ret;
        expr.var = found;
        expr.var.used = true;
    } else {
        errorUnrecognizedMember(parser, type, expr);
    }
}

// This function is called in one pass, during the line-by-line parsing.
// (since variables cannot be "forward-declared").
static void resolveVars(Compiler& com, ASTExpr& expr, ASTScope& scope,
    bool inFuncCall) { // TODO: this could be done on rpn in parseExpr, making
                       // it iterative instead of recursive = behaves
                       // differently inside a func call: the .left is not
                       // resolved to a var, but to an argument label of the
                       // called func. it would make sense to just skip checking
                       // it here for now, and let resolveFuncs use it to
                       // construct the func selector for lookup. At some point
                       // though it would be nice if the compiler could tell the
                       // user 'you missed the arg label "xyz"' for which the
                       // basename of the func could be used to get a list of
                       // all selectors having that basename as a prefix.

    if (!expr) return;
    switch (expr.kind) {
    case tkIdentifierResolved:
        break;

    case tkIdentifier:
    case tkSubscript: {
        TokenKind ret = (expr.kind == tkIdentifier) ? tkIdentifierResolved
                                                    : tkSubscriptResolved;
        ASTVar* found = getVar(scope, expr.string);
        if (found) {
            expr.kind = ret;
            expr.var = found;
            expr.var.used = true;
            // expr.var.lastUsed = topExpr;
        } else {
            // ASTImport* import = getImportByAlias(mod,
            // expr.string); if (import) {
            //     expr.kind = tkKeyword_import;
            //     expr.import = import;
            //     import.used = true;
            // } else {
            errorUnrecognizedVar(parser, expr);
            // }
        }
        if (expr.kind == tkSubscriptResolved || expr.kind == tkSubscript) {
            resolveVars(parser, *expr.left, scope, inFuncCall);
            // check subscript argument count
            // recheck kind since the var may have failed resolution
            // TODO: handle dims 0 as dims 1 because arr[] is the same as arr[:]
            //            if (expr.kind == tkSubscriptResolved
            //                and countCommaList(expr.left)
            //                    != expr.var.typeSpec.dims)
            //                errorIndexDimsMismatch(parser, expr);
            // do it in analysis after type/dims inference
        }
        break;
    }
    case tkFunctionCall:
        if (expr.left) resolveVars(parser, *expr.left, scope, true);
        break;

    case tkPeriod:
        if (expr.left) resolveVars(parser, *expr.left, scope, inFuncCall);
        // expr.right is not to be resolved in the same scope, but in
        // the type body of the type of expr.left. So you cannot call
        // resolveVars on expr.right from here. Neither can you assume
        // that types have been resolved, because name resolution
        // happens as the file is read, while type resolution happens
        // only after the tree is fully built. The way to fix it is to
        // have analyseExpr call the name resolution (as it already does for
        // exprs like a.b but not a.b.c) for expr.right, AFTER the type
        // for expr.left has been resolved.
        //        if (expr.right.kind==tkPeriod) resolveVars(this,
        //        expr.right, scope, inFuncCall);
        // besides an ident, the .right of a . can be either another
        // dot, a subscript, or a func call if we allow member funcs
        if (expr.right->kind == tkSubscript
            || expr.right->kind == tkSubscriptResolved)
            resolveVars(parser, *expr.right->left, scope, inFuncCall);

        break;

    case tkString: {
        // strings may have embedded variable names of the form $name or
        // $(name), so resolve them. by the time this func is called, the string
        // will be null-terminated
        char* pos = strchr(expr.string, '$');
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
                    ASTVar* var = getVar(scope, buf);
                    if (!var) {
                        char* orig = expr.string;
                        expr.string = buf;
                        expr.col += (pos - orig) + 1;
                        errorUnrecognizedVar(parser, expr);
                        expr.col -= (pos - orig) + 1;
                        expr.string = orig;
                    } else {
                        // we're not going to actually "resolve" the embedded
                        // var, just lint the name here so it is in the correct
                        // case
                        memcpy(pos + 1, var.name, len);
                    }

                } else {
                    errorStringInterp(parser, expr, pos);
                }
            }
            pos = strchr(pos + 1, '$');
        }
    } break;

    default:
        if (expr.prec) {
            if (!expr.unary) {
                if (inFuncCall && expr.kind == tkOpAssign) {
                    // expr.left.kind = tkArgumentLabel;
                    // Here you only set the type of the expr to
                    // tkArgumentLabel. It will be resolved later in analyse(),
                    // when the functions have been resolved. Maybe this should
                    // also be moved there then.
                } else {
                    resolveVars(parser, expr.left, scope, inFuncCall);
                }
            }
            resolveVars(parser, expr.right, scope, inFuncCall);

            if (isSelfMutOp(expr)) {
                ASTVar* var = NULL;
                ASTExpr* varExpr = expr.left;
                if (varExpr->kind == tkIdentifierResolved || //
                    varExpr->kind == tkSubscriptResolved) {
                    var = varExpr->var;
                } else if (varExpr->kind == tkPeriod && //
                    varExpr->left
                    && varExpr->left->kind == tkIdentifierResolved) {
                    varExpr = varExpr->left;
                    var = varExpr->var;
                }
                if (var) {
                    // TODO: If you will allow changing the first arg of a
                    // function, using an & op or whatever, check for those
                    // mutations here BTW this marks entire arrays as used
                    // for a.x = ... it marks all of a as used. Instead should
                    // traverse the . sequence left to right and check which the
                    // first read-only variable in that sequence
                    var->changed = true;
                    if (expr.kind == tkOpAssign) var->reassigned = true;
                    if (!var->isVar) errorReadOnlyVar(parser, varExpr);
                }
            }
        }
    }
}
