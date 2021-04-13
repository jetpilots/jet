
#include "jet/core/Dict.h"

static void ASTExpr_hash(
    /* Compiler* com, */ ASTExpr* expr, Dict(UInt32, Ptr) * cseDict) {
    if (!expr) unreachable("%s", "expr is NULL");
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
    case tkVarAssign:
        ASTExpr_hash(/* parser, */ expr->var->init, cseDict);
        //        expr->hash = expr->var->init->hash;
        break;
    case tkFunctionCallResolved:
    case tkIdentifierResolved:
    case tkSubscriptResolved: {
        union {
            // char buf[9];
            UInt64 h64;
            UInt32 hash[2];
        } str = { .h64 = 0 }; //.buf = { 1, 1, 1, 1, 1, 1, 1, 1, 0 } };
        str.hash[0] = UInt64_hash((UInt64)expr->right); //->name);
        // TODO: hash the right ptr instead
        // here we exploit the fact that name is at the same offset within
        // ASTFunc and ASTVar, so the deref will get the right thing
        if (expr->left) {
            ASTExpr_hash(/* parser,  */ expr->left, cseDict);
            str.hash[1] = expr->left->hash;
        }
        expr->hash = UInt64_hash(str.h64); // CString_hash(str.buf);
    } break;
    default:
        if (expr->prec) {
            union {
                char buf[13];
                UInt64 h64;
                UInt32 hash[2];
            } str = { .h64 = 0 }; //.buf = {1,1,1,1,1,1,1,1,1,1,1,1,0} };

            //            UInt32 kind = expr->kind;
            str.hash[0] = UInt32_hash(expr->kind);
            str.hash[1] = 0;
            if (expr->right) {
                ASTExpr_hash(/* parser, */ expr->right, cseDict);
                str.hash[1] = expr->right->hash;
            }
            UInt32 tmphash = UInt64_hash(str.h64);

            if (!expr->unary) {
                ASTExpr_hash(/* parser, */ expr->left, cseDict);
                str.hash[0] = expr->left->hash;
                str.hash[1] = tmphash;
                tmphash = UInt64_hash(str.h64);
            }

            expr->hash = tmphash; // CString_hash(str.buf);

            // just hashing the kind for now, maybe we need
            // the typeinfo as well
        }
    }
    //    printf("./%s:%02d:%02d: %-24s hash: %u\n",com->filename,
    //    expr->line, expr->col,
    //        TokenKind_str[expr->kind], expr->hash);

    if (expr->kind != tkNumber && expr->kind != tkIdentifierResolved
        && expr->kind != tkVarAssign) {
        // we don't want to put every small thing in the table, esp not literals
        // and idents. We'll put strings, so that they are effectively uniq'd
        // within the func. Also regexes. What we really want are binops, calls,
        // subscripts, etc.
        int status = 0;
        UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, expr->hash, &status);
        if (status == 1) Dict_val(cseDict, idx) = expr;
    }
}

// This function checks the hashes, going DOWN the tree. It happens after the
// hashes have been generated in ASTExpr_hash (which is bottom-up, so checking
// cannot happen inline).
static void ASTExpr_checkHashes(
    /* Compiler* com, */ ASTExpr* expr, Dict(UInt32, Ptr) * cseDict) {

    UInt32 idx = Dict_get(UInt32, Ptr)(cseDict, expr->hash);

    if (idx < Dict_end(cseDict)) {
        ASTExpr* orig = Dict_val(cseDict, idx);
        if (orig != expr && orig->kind == expr->kind) {
            // unfortunately there ARE collisions, so check again
            eprintf("\n-- found same exprs at\n%02d:%02d hash %d and\n",
                /* com->filename, */ expr->line, expr->col, expr->hash);
            // TODO: make gen print to stderr
            // ASTExpr_gen(expr, 4, true, false);
            eprintf("\n:%02d:%02d hash %d\n",
                /* com->filename, */ orig->line, orig->col, orig->hash);
            // ASTExpr_gen(orig, 4, true, false);
            eputs("");
            return;
        }
    }
    // this expr isn't a candidate for CSE. Check subexprs.
    switch (expr->kind) {
        //        case tkKeyword_if: ...
    case tkVarAssign:
        ASTExpr_checkHashes(/* parser, */ expr->var->init, cseDict);
        break;

    case tkFunctionCallResolved:
    case tkSubscriptResolved:
        if (expr->left) ASTExpr_checkHashes(/* parser, */ expr->left, cseDict);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary)
                ASTExpr_checkHashes(/* parser, */ expr->left, cseDict);
            if (expr->right)
                ASTExpr_checkHashes(/* parser, */ expr->right, cseDict);
        }
    }
}

static void ASTFunc_hashExprs(/* Compiler* com,  */ ASTFunc* func) {
    static Dict(UInt32, Ptr)* cseDict = NULL; // FIXME: will leak
    if (!cseDict) cseDict = Dict_init(UInt32, Ptr)();
    Dict_clear(UInt32, Ptr)(cseDict);

    foreach (ASTExpr*, stmt, func->body->stmts) {
        ASTExpr_hash(/* parser, */ stmt, cseDict);
        ASTExpr_checkHashes(/* parser, */ stmt, cseDict);

        //        int status = 0;
        //        UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, stmt->hash,
        //        &status);
        //        // Dict_get(UInt32, Ptr)(cseDict, stmt->hash);
        //        if (status != 1) { // idx < Dict_end(cseDict)) {
        //            ASTExpr* orig = Dict_val(cseDict, idx);
        //            printf("found same exprs at %02d:%02d (%d) and %d:%d
        //            (%d)\n",
        //                stmt->line, stmt->col, stmt->hash, orig->line,
        //                orig->col, orig->hash);
        //            ;
        //        } else
        //            //        idx =
        //            Dict_val(cseDict, idx) = stmt;
    }
    //    foreach (ASTExpr*, stmt, func->body->stmts) {
    //        int status = 0;
    //        UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, stmt->hash, &status);
    //        // Dict_get(UInt32, Ptr)(cseDict, stmt->hash);
    //        if (status != 1) { // idx < Dict_end(cseDict)) {
    //            ASTExpr* orig = Dict_val(cseDict, idx);
    //            printf("found same exprs at %02d:%02d (%d) and %d:%d (%d)\n",
    //                stmt->line, stmt->col, stmt->hash, orig->line, orig->col,
    //                orig->hash);
    //            ;
    //        } else
    //            //        idx =
    //            Dict_val(cseDict, idx) = stmt;
    //    }

    // exit(8);
}
