
#include "jet/core/Dict.h"

void hash(ASTExpr* expr, Dict<UInt32, Ptr>* cseDict) {
    if (!expr) unreachable("%s", "expr is NULL");
    switch (expr->kind) {
    case tkString:
    case tkRawString:
        expr->hash = hash(expr->string);
        break;
    case tkNumber: {
        char* c = strpbrk(expr->string, "e.");
        if (c) {
            expr->hash = hash(atof(expr->string));
        } else {
            expr->hash = hash(atoll(expr->string));
        }
    } break;
    case tkVarAssign:
        hash(expr->var->init, cseDict);
        break;
    case tkFunctionCallResolved:
    case tkIdentifierResolved:
    case tkSubscriptResolved: {
        union {
            UInt64 h64;
            UInt32 hash[2];
        } str = { .h64 = 0 };
        str.hash[0] = UInt64_hash((UInt64)expr->right);
        // TODO: hash the right ptr instead
        // here we exploit the fact that name is at the same offset within
        // ASTFunc and ASTVar, so the deref will get the right thing
        if (expr->left) {
            hash(expr->left, cseDict);
            str.hash[1] = expr->left->hash;
        }
        expr->hash = hash(str.h64); // CString_hash(str.buf);
    } break;
    default:
        if (expr->prec) {
            union {
                char buf[13];
                UInt64 h64;
                UInt32 hash[2];
            } str = { .h64 = 0 }; //.buf = {1,1,1,1,1,1,1,1,1,1,1,1,0} };

            //            UInt32 kind = expr->kind;
            str.hash[0] = hash(expr->kind);
            str.hash[1] = 0;
            if (expr->right) {
                hash(expr->right, cseDict);
                str.hash[1] = expr->right->hash;
            }
            UInt32 tmphash = hash(str.h64);

            if (!expr->unary) {
                hash(expr->left, cseDict);
                str.hash[0] = expr->left->hash;
                str.hash[1] = tmphash;
                tmphash = hash(str.h64);
            }

            expr->hash = tmphash; // CString_hash(str.buf);

            // just hashing the kind for now, maybe we need
            // the typeinfo as well
        }
    }
    //    printf("./%s:%02d:%02d: %-24s hash: %u\n",com->filename,
    //    expr->line, expr->col,
    //        TokenKind_str[expr->kind], expr->hash);

    if (not(expr->is(tkNumber) //
            or expr->is(tkIdentifierResolved) //
            or expr->is(tkVarAssign))) {
        cseDict[expr->hash] = expr;
        // we don't want to put every small thing in the table, esp not literals
        // and idents. We'll put strings, so that they are effectively uniq'd
        // within the func. Also regexes. What we really want are binops, calls,
        // subscripts, etc.
        // int status = 0;
        // UInt32 idx = Dict_put(UInt32, Ptr)(cseDict, expr->hash, &status);
        // if (status == 1) Dict_val(cseDict, idx) = expr;
    }
}

// This function checks the hashes, going DOWN the tree. It happens after the
// hashes have been generated in  hash (which is bottom-up, so checking
// cannot happen inline).
static void checkHashes(ASTExpr* expr, Dict(UInt32, Ptr) * cseDict) {

    UInt32 idx = Dict_get(UInt32, Ptr)(cseDict, expr->hash);

    if (idx < Dict_end(cseDict)) {
        ASTExpr* orig = Dict_val(cseDict, idx);
        if (orig != expr && orig->is(expr)->kind) {
            // unfortunately there ARE collisions, so check again
            eprintf("\n-- found same exprs at\n%02d:%02d hash %d and\n",
                /* com->filename, */ expr->line, expr->col, expr->hash);
            // TODO: make gen print to stderr
            //  gen(expr, 4, true, false);
            eprintf("\n:%02d:%02d hash %d\n",
                /* com->filename, */ orig->line, orig->col, orig->hash);
            //  gen(orig, 4, true, false);
            eputs("");
            return;
        }
    }
    // this expr isn't a candidate for CSE. Check subexprs.
    switch (expr->kind) {
        //        case tkKeyword_if: ...
    case tkVarAssign:
        checkHashes(expr->var->init, cseDict);
        break;

    case tkFunctionCallResolved:
    case tkSubscriptResolved:
        if (expr->left) checkHashes(expr->left, cseDict);
        break;
    default:
        if (expr->prec) {
            if (!expr->unary) checkHashes(expr->left, cseDict);
            if (expr->right) checkHashes(expr->right, cseDict);
        }
    }
}

void dohash(ASTFunc* func) {
    static Dict<UInt32, Ptr>* cseDict = NULL; // FIXME: will leak
    if (!cseDict) cseDict = Dict<UInt32, Ptr>();
    clear(cseDict);

    for (ASTExpr* stmt : func->body->stmts) {
        hash(stmt, cseDict);
        checkHashes(stmt, cseDict);

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
