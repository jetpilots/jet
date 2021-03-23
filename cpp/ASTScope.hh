struct ASTScope {
    List<ASTExpr&> stmts;
    List<ASTVar&> locals;
    struct ASTScope* parent;

    void lowerElementalOps();
    void promoteCandidates();
    void emit(int level);
    void lint(int level);
    size_t size();
    ASTVar* var(const char* name);
};