
#include "jet/core/Dict.h"

static void ASTExpr_hash(
    /* Parser* parser, */ ASTExpr* expr, Dict(UInt32, Ptr) * cseDict) {
    if (!expr) unreachable("%s", "expr is NULL");
    switch (expr->kind) {
    case tkString:
    case tkRawString:
        expr->hash = CString_hash(expr->string);
        break;
    case tkNumber: {
        char* c = strpbrk(expr->string, "e.");
        if (c) {
            expr->hash = Real64_hash(atof(expr->string));
        } else {
            expr->hash = Int64_hash(atoll(expr->string));
        }
    } break;
    case tkVarAssign:
        ASTExpr_hash(expr->var->init, cseDict);

        break;
    case tkFunctionCallResolved:
    case tkIdentifierResolved:
    case tkSubscriptResolved: {
        union {

            UInt64 h64;
            UInt32 hash[2];
        } str = { .h64 = 0 };
        str.hash[0] = UInt64_hash((UInt64)expr->right);

        if (expr->left) {
            ASTExpr_hash(expr->left, cseDict);
            str.hash[1] = expr->left->hash;
        }
        expr->hash = UInt64_hash(str.h64);
    } break;
    default:
        if (expr->prec) {
            union {
                char buf[13];
                UInt64 h64;
                UInt32 hash[2];
            } str = { .h64 = 0 };

            str.hash[0] = UInt32_hash(expr->kind);
            str.hash[1] = 0;
            if (expr->right) {
                ASTExpr_hash(expr->right, cseDict);
                str.hash[1] = expr->right->hash;
            }
            UInt32 tmphash = UInt64_hash(str.h64);

            if (!expr->unary) {
                ASTExpr_hash(expr->left, cseDict);
                str.hash[0] = expr->left->hash;
                str.hash[1] = tmphash;
                tmphash = UInt64_hash(str.h64);
            }

            expr->hash = tmphash;
        }
    }

    if (expr->kind != tkNumber && expr->kind != tkIdentifierResolved
        && expr->kind != tkVarAssign) {

        int status = 0;
        UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, expr->hash, &status);
        if (status == 1) Dict_val(cseDict, idx) = expr;
    }
}

static void ASTExpr_checkHashes(
    /* Parser* parser, */ ASTExpr* expr, Dict(UInt32, Ptr) * cseDict) {

    UInt32 idx = Dict_get(UInt32, Ptr)(cseDict, expr->hash);

    if (idx < Dict_end(cseDict)) {
        ASTExpr* orig = Dict_val(cseDict, idx);
        if (orig != expr && orig->kind == expr->kind) {

            eprintf("\n-- found same exprs at\n%02d:%02d hash %d and\n",
                expr->line, expr->col, expr->hash);

            eprintf(
                "\n:%02d:%02d hash %d\n", orig->line, orig->col, orig->hash);

            eputs("");
            return;
        }
    }

    switch (expr->kind) {

    case tkVarAssign:
        ASTExpr_checkHashes(expr->var->init, cseDict);
        break;

    case tkFunctionCallResolved:
    case tkSubscriptResolved:
        if (expr->left) ASTExpr_checkHashes(expr->left, cseDict);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary) ASTExpr_checkHashes(expr->left, cseDict);
            if (expr->right) ASTExpr_checkHashes(expr->right, cseDict);
        }
    }
}

static void ASTFunc_hashExprs(/* Parser* parser,  */ ASTFunc* func) {
    static Dict(UInt32, Ptr)* cseDict = NULL;
    if (!cseDict) cseDict = Dict_init(UInt32, Ptr)();
    Dict_clear(UInt32, Ptr)(cseDict);

    foreach (ASTExpr*, stmt, func->body->stmts) {
        ASTExpr_hash(stmt, cseDict);
        ASTExpr_checkHashes(stmt, cseDict);
    }
}
