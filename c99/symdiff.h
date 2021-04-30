Expr* makeDerivative(Expr* expr, Var* wrt) {
    switch (expr->kind) {
    case tkOpPlus:
    case tkOpMinus:
        if (expr->left->isConst) // isConst, isLiteral, or canEval?
            return makeDerivative(expr->right, wrt);
        else if (expr->right->isConst)
            return makeDerivative(expr->left, wrt);
        else
            return NEWW(Expr, //
                .left = makeDerivative(expr->left, wrt),
                .right = makeDerivative(expr->right, wrt), //
                .kind = expr->kind);
        break;
    case tkOpTimes:
        if (expr->left->isConst) // isConst, isLiteral, or canEval?
            return makeDerivative(expr->right, wrt);
        else if (expr->right->isConst)
            return makeDerivative(expr->left, wrt);
        else
            return NEWW(Expr, //
                .left = makeDerivative(expr->left, wrt),
                .right = makeDerivative(expr->right, wrt), //
                .kind = expr->kind);
        break;
    default: break;
    }
}