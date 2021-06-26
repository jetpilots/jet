
#include "jet/core/Dict.h"
static uint32_t func_hash(Func* func) {
  return UInt64_hash((uint64_t)func); // for now just the pointer hash
}

static uint32_t var_hash(Var* var) {
  return UInt64_hash((uint64_t)var) + var->changed;
}

static void expr_dohash(
    Parser* parser, Expr* expr, Dict(UInt32, VPtr) * cseDict) {
  if (!expr) {
    unreachable("%s", "expr is NULL");
    return;
  }
  switch (expr->kind) {
  case tkString:
  case tkIdent:
  case tkRawString: expr->hash = CString_hash(expr->str); break;
  case tkNumber:
    expr->hash = strpbrk(expr->str, "e.") ? Real64_hash(atof(expr->str))
                                          : Int64_hash(atoll(expr->str));
    break;
  case tkVarDefn: expr_dohash(parser, expr->var->init, cseDict); break;

  case tkFuncCallR:
  case tkFuncCall: {
    union {
      UInt64 h64;
      UInt32 hash[2];
    } str = { .h64 = 0 };
    str.hash[0] = func_hash(expr->func);
    if (expr->left) {
      expr_dohash(parser, expr->left, cseDict);
      str.hash[1] = expr->left->hash;
    }
    expr->hash = UInt64_hash(str.h64);
  } break;

  case tkSubscript:
  case tkIdentR:
  case tkSubscriptR: {
    // TODO: when you have expr kind in self-mut ops you should increment a
    // field in the targetvar. changed is not a suitable field because it
    // would require cse to be done in tandem with analyse (among other
    // issues cseDict cannot then be static).
    // TODO: if you keep one large cseDict of hashes across all funcs, you
    // can do module-wide CSE by storing into static threadprivate globals.
    union {
      UInt64 h64;
      UInt32 hash[2];
    } str = { .h64 = 0 };
    str.hash[0] = var_hash(expr->var);
    if (expr->left) {
      expr_dohash(parser, expr->left, cseDict);
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
        expr_dohash(parser, expr->right, cseDict);
        str.hash[1] = expr->right->hash;
      }
      UInt32 tmphash = UInt64_hash(str.h64);

      if (!expr->unary && expr->left) {
        expr_dohash(parser, expr->left, cseDict);
        str.hash[0] = expr->left->hash;
        str.hash[1] = tmphash;
        tmphash = UInt64_hash(str.h64);
      }

      expr->hash = tmphash;

      // just hashing the kind for now, maybe we need
      // the typeinfo as well
    }
  }
  if (!ISIN(6, expr->kind, tkNumber, tkString, tkRawString, tkIdentR,
          tkVarDefn, tkComment)
      && !isCtrlExpr(expr)) {
    if (expr->kind != tkPeriod
        || (expr->right && expr->right->kind == tkFuncCallR)) {
      // Don't put really simple things like literals into the CSE dict.
      Dict_putk(UInt32, VPtr)(cseDict, expr->hash, expr);
      // int status = 0;
      // UInt32 idx = Dict_put(UInt32, VPtr)(cseDict, expr->hash, &status);
      // if (status == 1) Dict_val(cseDict, idx) = expr;
    }
  }
}

// This function checks the hashes, going DOWN the tree. It happens after
// the hashes have been generated in expr_dohash (which is bottom-up, so
// checking cannot happen inline).
static void expr_checkHashes(
    Parser* parser, Expr* expr, Dict(UInt32, VPtr) * cseDict) {
  if (!expr) return;

  // UInt32 idx = Dict_get(UInt32, VPtr)(cseDict, expr->hash);
  Expr* orig = Dict_getk(UInt32, VPtr)(cseDict, expr->hash);
  if (orig /*idx < Dict_end(cseDict)*/) {
    // Expr* orig = Dict_val(cseDict, idx);
    // unfortunately there ARE collisions, so check again
    if (orig != expr && orig->kind == expr->kind) {
      warn_sameExpr(parser, expr, orig);
      return;
    }
  }
  // Otherwise this expr isn't a candidate for CSE. Check subexprs.
  switch (expr->kind) {
  case tkVarDefn: expr_checkHashes(parser, expr->var->init, cseDict); break;
  case tkFuncCallR:
  case tkFuncCall:
  case tkSubscript:
  case tkSubscriptR:
    if (expr->left) expr_checkHashes(parser, expr->left, cseDict);
    break;
  default:
    if (expr->prec) {
      if (!expr->unary && expr->left)
        expr_checkHashes(parser, expr->left, cseDict);
      if (expr->right) expr_checkHashes(parser, expr->right, cseDict);
    }
  }
}

static void func_hashExprs(Parser* parser, Func* func) {
  static Dict(UInt32, VPtr)* cseDict = NULL; // FIXME: will leak
  if (!cseDict) cseDict = Dict_new(UInt32, VPtr)();
  Dict_clear(UInt32, VPtr)(cseDict);

  foreach (Expr*, stmt, func->body->stmts) {
    expr_dohash(parser, stmt, cseDict);
    expr_checkHashes(parser, stmt, cseDict);
  }
}
