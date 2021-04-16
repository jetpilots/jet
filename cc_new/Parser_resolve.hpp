/// Resolves the type of the given variable.
void resolve(Var& var) {
    if (var.typeInfo.typeType != TYUnresolved) return;
    if (not *var.typeName()) return;
    TypeType tyty = TypeType_byName(var.typeName());
    if (tyty) {
        var.typeInfo.typeType = tyty;
    } else {
        Type* type = getType(var.typeName());
        if (type) {
            var.typeInfo.typeType = TYObject;
            var.type = type;
            return;
        }
        errs.unknownType(var);
        return;
    }
    if (var.typeInfo.dims) { }
}

/// Resolve a variable reference to its corresponding Var object.
void resolve(Expr& expr, Expr* parent, Scope& scope) { // if (!expr) return;
    switch (expr.kind) {
    case tkIdentifierResolved: break;
    case tkIdentifier:
    // TODO check parent here if tkFuncCall. Then you don't nee
    // arg
    case tkSubscript: {
        TokenKind ret = (expr.kind == tkIdentifier) ? tkIdentifierResolved
                                                    : tkSubscriptResolved;
        Var* found = scope.getVar(expr.string);
        if (found) {
            expr.kind = ret;
            expr.var = found;
            expr.var->used = true;
        } else {
            Import* import = scope.getImport(expr.string);
            if (import) {
                expr.kind = tkKeyword_import;
                expr.import = import;
                import->used = true;
            } else {
                errs.unknownVar(expr);
            }
        }
        if (expr.is(tkSubscriptResolved) or expr.is(tkSubscript))
            resolve(*expr.left, &expr, scope);
    } break;
    case tkFunctionCall:
        if (expr.left) resolve(expr.left, expr, scope, true);
        break;
    case tkPeriod:
        if (expr.left) resolve(expr.left, expr, scope);
        // expr.right itself will be resolved in analysis, because it needs
        // the type of expr.left to have been resolved (which can be forward
        // declared).
        if (expr.right->is(tkSubscript) or expr.right->is(tkSubscriptResolved))
            resolve(expr.right->left, expr.right, scope);
        break;
    case tkString: {
        // strings may have embedded variable names of the form $name or
        // $(name), so resolve them. by the time this func is called, the
        // string will be null-terminated.
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
                    Var* var = scope.getVar(buf);
                    if (!var) {
                        char* orig = expr.string;
                        expr.string = buf;
                        expr.loc.col += (pos - orig) + 1;
                        errs.unknownVar(expr);
                        expr.loc.col -= (pos - orig) + 1;
                        expr.string = orig;
                    } else {
                        // we're not yet going to actually "resolve" the
                        // embedded var, just lint the name here so it is in
                        // the correct case.
                        // TODO: resolve the var and add it to the vars list
                        // in the expr
                        memcpy(pos + 1, var->name, len);
                        // expr.vars.push()
                    }

                } else {
                    errs.stringInterp(expr, pos);
                }
            }
            pos = strchr(pos + 1, '$');
        }
    } break;

    default:
        if (expr.prec) {
            if (!expr.unary) {
                if (parent->in(tkFunctionCall, tkFunctionCallResolved)
                    and expr.kind == tkOpAssign) {
                    // expr.left.kind = tkArgumentLabel;
                    // Here you only set the type of the expr to
                    // tkArgumentLabel. It will be resolved later in
                    // analyse(), when the functions have been resolved.
                    // Maybe this should also be moved there then.
                } else {
                    resolve(*expr.left, &expr, scope);
                }
            }
            resolve(*expr.right, &expr, scope);

            if (expr.isSelfMutOp()) {
                Expr* target = expr.left;
                if (target->is(tkIdentifierResolved) or //
                    target->is(tkSubscriptResolved)) {
                } else if (target->is(tkPeriod) and target->left //
                    and target->left->is(tkIdentifierResolved)) {
                    target = target->left;
                } else {
                    unreachable("unknown kind: %d", target->kind);
                }
                Var* var = target->var;
                if (var) {
                    var->changed++;
                    if (expr.is(tkOpAssign)) var->reassigned = true;
                    if (!var->isMutable) errs.immutableVar(target);
                }
            }
        }
    }
}

/// Resolve a reference to a member variable within its type.
void resolve(Expr& expr, Expr* parent, Type& type) {
    assert(expr.in(tkIdentifier, tkSubscript));
    TokenKind ret = (expr.kind == tkIdentifier) ? tkIdentifierResolved
                                                : tkSubscriptResolved;
    Var* found = nullptr;
    if (type.body) found = type.body.getVar(expr.string);
    if (found) {
        expr.kind = ret;
        expr.var = found;
        expr.var->used++;
    } else {
        errs.unknownMember(expr, type);
    }
}