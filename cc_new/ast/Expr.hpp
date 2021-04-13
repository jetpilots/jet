struct Var;
struct Func;
struct Import;

struct Expr {
    SourceLoc loc;
    TokenKind kind : 8;
    uint8_t prec : 6, rassoc : 1, unary : 1;
    union {
        Expr* left;
        List<Var&>* vars;
    };
    union {
        Expr* right;
        SmallString string;
        Var* var;
        Func* func;
        Import* import;
        Scope* body;
    };
    union {
        TypeInfo typeInfo;
        long hash;
    };

    bool is(TokenKind k) { return k == kind; }
    bool in(TokenKind k1, TokenKind k2, TokenKind k3) {
        return k1 == kind or in(k2, k3);
    }
    bool in(TokenKind k1, TokenKind k2, TokenKind k3, TokenKind k4) {
        return k1 == kind or in(k2, k3, k4);
    }
    bool in(TokenKind k1, TokenKind k2) { return k1 == kind or k2 == kind; }
};

static_assert(sizeof(Expr) == 32);
