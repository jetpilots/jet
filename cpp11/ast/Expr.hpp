struct Var;
struct Func;
struct Import;

struct Expr {
  SourceLoc loc;
  TokenKind kind : 8;
  uint8_t prec : 6, rassoc : 1, unary : 1;
  bool elemental, throws, extract;
  union {
    Expr* left;
    List<Var&>* vars;
    Type* type; // for array elements etc.
    // FIXME rename type to something else! It feels like it indicates the
    // type of the expr which IT DOES NOT. it is a reference to a Type
    // object for tkKeyword_type exprs and for arrays it is a ref to the
    // type of the elements. It can't hold type info for exprs generally
    // because it will clobber left.
  };
  union {
    Expr* right;
    const char* string;
    Var* var;
    Func* func;
    Import* import;
    Scope* body;
  };
  union {
    TypeInfo spec;
    long hash;
  };
  Expr(TokenKind k)
      : kind(k) { }
  Expr(TokenKind k, SourceLoc l)
      : kind(k)
      , loc(l) { }
  Expr(TokenKind k, const char* s)
      : kind(k)
      , string(s) { }

  bool isSelfMutOp() {
    return kind > __tk__selfMutOps__begin and kind < __tk__selfMutOps__end;
  }
  bool isCtrlExpr() {
    return kind > __tk__ctrlflow__begin and kind < __tk__ctrlflow__end;
  }
  bool isBoolOp() {
    return kind > __tk__logicals__begin and kind < __tk__logicals__end;
  }
  bool isCmpOp() {
    return kind > __tk__cmpOps__begin and kind < __tk__cmpOps__end;
  }
  static Expr* Defn(Func& fn) {
    Expr* ret = new Expr(tkKeyword_function, fn.loc);
    ret->func = &fn;
    ret->elemental = fn.elemental;
    ret->throws = fn.throws;
    return ret;
  }
  static Expr* Defn(Type& type) {
    Expr* ret = new Expr(tkKeyword_type, type.loc);
    ret->type = &type;
    return ret;
  }
  static Expr* Defn(Var& var) {
    Expr* ret = new Expr(tkKeyword_type, var.loc);
    ret->var = &var;
    return ret;
  }
  bool is(TokenKind k) { return k == kind; }
  bool in(TokenKind k1, TokenKind k2, TokenKind k3) {
    return k1 == kind or in(k2, k3);
  }
  bool in(TokenKind k1, TokenKind k2, TokenKind k3, TokenKind k4) {
    return k1 == kind or in(k2, k3, k4);
  }
  bool in(TokenKind k1, TokenKind k2) { return k1 == kind or k2 == kind; }

  // static  TypeSpec*  Expr_getObjectTypeSpec(const  Expr* const self) {
  //     if (!self) return nullptr;

  //     // all that is left is object
  //     switch ( kind) {
  //     case tkFunctionCallResolved:
  //         // case tkFunctionCall:
  //         return  func->returnSpec;
  //     case tkIdentifierResolved:
  //     case tkSubscriptResolved:
  //         // case tkIdentifier:
  //         // case tkSubscript:
  //         return  var->spec;
  //         //        }
  //         // TODO: tkOpColon should be handled separately in the semantic
  //         // pass, and should be assigned either TYObject or make a
  //         dedicated
  //         // TYRange
  //         //     case tkOpColon:
  //         //        return "Range";
  //         // TODO: what else???
  //     case tkPeriod:
  //     case tkOpComma: return  Expr_getObjectTypeSpec( right);
  //     default: break;
  //     }
  //     return nullptr;
  // }
  int countCommaList() {
    Expr* e = this;
    int i = 0;
    for (i = 1; right && kind == tkOpComma; i++) e = e->right;
    return i;
  }

  Type* getEnumType() {
    Type* ret = nullptr;
    if (spec.typeType != TYObject) return ret;
    // all that is left is object
    switch (kind) {
    case tkFunctionCallResolved: ret = func->ret->type; break;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
      ret = var->type;
      break; //        }
             // TODO: tkOpColon should be handled separately in the
             // semantic pass, and should be assigned either TYObject or
             // make a dedicated TYRange
             //     case tkOpColon:
             //        return "Range";
             // TODO: what else???
    case tkPeriod:
    case tkOpComma: ret = left->getEnumType();
    default: break;
    }
    if (ret && !ret->isEnum()) ret = nullptr;
    return ret;
  }

  Type* getObjectType() {
    if (spec.typeType != TYObject) return nullptr;

    // all that is left is object
    switch (kind) {
    case tkFunctionCallResolved: return func->ret->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved: return var->type;
    //        }
    // TODO: tkOpColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkOpColon:
    //        return "Range";
    // TODO: what else???
    case tkPeriod:
    case tkOpComma: return right->getObjectType();
    default: break;
    }
    return nullptr;
  }

  Type* getTypeOrEnum() {
    if (spec.typeType != TYObject) return nullptr;

    // all that is left is object
    switch (kind) {
    case tkFunctionCallResolved: return func->ret->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved: return var->type;
    //        }
    // TODO: tkOpColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkOpColon:
    //        return "Range";
    // TODO: what else???
    case tkPeriod: {
      Type* type = left->getObjectType();
      if (!type->isEnum()) type = right->getObjectType();
      return type;
    }
    case tkOpComma: return left->getTypeOrEnum();
    default: break;
    }
    return nullptr;
  }
  // static CString*  Expr_getTypeOrEnumName(const  Expr* const self) {
  //      Type* type =  Expr_getTypeOrEnum(self);
  //     return type ? type->name : "";
  // }

  const char* typeName() {
    const char* ret = spec.name();
    if (!ret) return "<unknown>"; // unresolved
    if (*ret) return ret; // primitive type

    // all that is left is object
    switch (kind) {
    case tkFunctionCallResolved: return func->ret->typeName();
    case tkIdentifierResolved:
    case tkSubscriptResolved: return var->typeName();
    //        }
    // TODO: tkOpColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkOpColon:
    //        return "Range";
    // TODO: what else???
    // case tkPeriod:
    //     return  Expr_typeName( right);
    case tkPeriod: {
      Type* type = left->getObjectType();
      return (type->isEnum()) ? type->name : right->typeName();
    }
    case tkOpComma: return left->typeName();
    default: break;
    }
    return "<invalid>";
  }
};
static_assert(sizeof(Expr) <= 32);
