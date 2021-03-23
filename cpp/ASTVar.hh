struct ASTVar : public ASTNode {
    char* name;
    ASTTypeSpec* typeSpec;
    ASTExpr* init;
    // Interval* refinement;
    // ^ For strings or arrays, the refinement is the guessed length bound. This
    // is collected based on checks or other expressions. It is updated while
    // stepping through statements in analysis. For numbers, it is the guessed
    // value bound. The guessing should be based on sound analysis.

    bool used : 1, changed : 1, isLet : 1, isVar : 1, isArg : 1, stackAlloc : 1,
        isTarget : 1, visited : 1, escapes : 1, canInplace : 1, isPromise : 1,
        hasRefs : 1, obtainedBySerialization : 1, usedAsIndex : 1,
        reassigned : 1, resized : 1, returned : 1, elementsReassigned;

    void genh(int level);
    void lint(int level);
    void emit(int level, bool isconst);
};