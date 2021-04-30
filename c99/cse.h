
#include "jet/core/Dict.h"

static void Expr_hash(Parser* parser, Expr* expr, Dict(UInt32, Ptr) * cseDict) {
    if (!expr) {
        unreachable("%s", "expr is NULL");
        return;
    }
    switch (expr->kind) {
    case tkString:
    case tkRawString: expr->hash = CString_hash(expr->string); break;
    case tkNumber: {
        char* c = strpbrk(expr->string, "e.");
        if (c) {
            expr->hash = Real64_hash(atof(expr->string));
        } else {
            expr->hash = Int64_hash(atoll(expr->string));
        }
    } break;
    case tkVarAssign: Expr_hash(parser, expr->var->init, cseDict); break;
    case tkFunctionCallResolved:
    case tkFunctionCall:
    case tkSubscript:
    case tkIdentifierResolved:
    case tkSubscriptResolved: {
        union {
            UInt64 h64;
            UInt32 hash[2];
        } str = { .h64 = 0 };
        str.hash[0] = UInt64_hash((UInt64)expr->right);
        // TODO: hash the right ptr instead
        // here we exploit the fact that name is at the same offset within
        // Func and Var, so the deref will get the right thing
        if (expr->left) {
            Expr_hash(parser, expr->left, cseDict);
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
                Expr_hash(parser, expr->right, cseDict);
                str.hash[1] = expr->right->hash;
            }
            UInt32 tmphash = UInt64_hash(str.h64);

            if (!expr->unary && expr->left) {
                Expr_hash(parser, expr->left, cseDict);
                str.hash[0] = expr->left->hash;
                str.hash[1] = tmphash;
                tmphash = UInt64_hash(str.h64);
            }

            expr->hash = tmphash;

            // just hashing the kind for now, maybe we need
            // the typeinfo as well
        }
    }
    if (!ISIN(3, expr->kind, tkNumber, tkIdentifierResolved, tkVarAssign)) {
        // Don't put really simple things like literals into the CSE dict.
        int status = 0;
        UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, expr->hash, &status);
        if (status == 1) Dict_val(cseDict, idx) = expr;
    }
}

// This function checks the hashes, going DOWN the tree. It happens after the
// hashes have been generated in Expr_hash (which is bottom-up, so checking
// cannot happen inline).
static void Expr_checkHashes(
    Parser* parser, Expr* expr, Dict(UInt32, Ptr) * cseDict) {
    if (!expr) return;
    UInt32 idx = Dict_get(UInt32, Ptr)(cseDict, expr->hash);

    if (idx < Dict_end(cseDict)) {
        Expr* orig = Dict_val(cseDict, idx);
        // unfortunately there ARE collisions, so check again
        if (orig != expr && orig->kind == expr->kind) {
            Parser_warnSameExpr(parser, expr, orig);
            return;
        }
    }
    // Otherwise this expr isn't a candidate for CSE. Check subexprs.
    switch (expr->kind) {
    case tkVarAssign: Expr_checkHashes(parser, expr->var->init, cseDict); break;
    case tkFunctionCallResolved:
    case tkFunctionCall:
    case tkSubscript:
    case tkSubscriptResolved:
        if (expr->left) Expr_checkHashes(parser, expr->left, cseDict);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary && expr->left)
                Expr_checkHashes(parser, expr->left, cseDict);
            if (expr->right) Expr_checkHashes(parser, expr->right, cseDict);
        }
    }
}

static void Func_hashExprs(Parser* parser, Func* func) {
    static Dict(UInt32, Ptr)* cseDict = NULL; // FIXME: will leak
    if (!cseDict) cseDict = Dict_init(UInt32, Ptr)();
    Dict_clear(UInt32, Ptr)(cseDict);

    foreach (Expr*, stmt, func->body->stmts) {
        Expr_hash(parser, stmt, cseDict);
        Expr_checkHashes(parser, stmt, cseDict);
    }
}
