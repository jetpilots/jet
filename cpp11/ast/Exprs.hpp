struct Expr {
  int line, col, len, kind;
  virtual void resolve(Scope& scope, Module& mod) { }
  virtual void analyse(Scope& scope, Func& func, Module& mod) { }
  virtual void write(Scope& scope, Func& func, Module& mod) { }
  virtual void dumpc(Scope& scope, Func& func, Module& mod) { }
  virtual void emitC(Scope& scope, Func& func, Module& mod) { }
  virtual void emitH(Scope& scope, Func& func, Module& mod) { }
  virtual Expr* clone() { return new Expr(*this); }
};
struct UnOpExpr : Expr {
  Expr* right;
  virtual Expr* clone() {
    auto n = new UnOpExpr;
    *n = *this;
    if (right) right = right->clone();
    return n;
  }
};
struct BinOpExpr : UnOpExpr {
  Expr* left;
  virtual Expr* clone() {
    auto n = new BinOpExpr;
    *n = *this;
    if (right) right = right->clone();
    if (left) left = left->clone();
    return n;
  }
};
struct MutOpExpr : BinOpExpr { };
struct CallExpr : Expr {
  Expr* args;
  Func* func;
};
struct IndexExpr : Expr {
  Expr* args;
  Var* var;
};
struct LitExpr : Expr {
  union {
    char* string;
    double number;
  };
};
struct NameExpr : LitExpr { };
struct StrExpr : LitExpr { };
struct RegxExpr : LitExpr { };
struct NumExpr : LitExpr { };
struct KwordExpr : LitExpr { };
struct EnumExpr : LitExpr {
  Type* base;
};
struct CondExpr : Expr {
  Expr* cond;
};
struct MatchExpr : CondExpr {
  List<CaseExpr&>* cases;
};
struct CaseExpr : CondExpr {
  Scope& body;
};
struct IfExpr : CondExpr { };
struct ElseExpr : CondExpr { };
struct ForExpr : Expr { };
struct WhileExpr : CondExpr { };
struct VarExpr : Expr { };
struct CheckExpr : CondExpr { };
