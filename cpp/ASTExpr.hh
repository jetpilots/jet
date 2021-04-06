struct ASTExpr : public ASTNode {

    unsigned typeType : 8;
    unsigned collectionType : 8;
    unsigned prec : 6, unary : 1, rassoc : 1;
    unsigned nullable : 1, elemental : 1, throws : 1, impure : 1, promote : 1,
        canEval : 1, didEval : 1;

    unsigned hash, dims;

    union {
        ASTExpr* left;
        List<ASTExpr&>* interpExprs;
        ASTType* elementType;
    };
    union {
        ASTExpr* right;
        CString string;
        Real64 number;
        Integer integer;
        ASTVar* var;
        ASTFunc* func;
        ASTImport* import;
        ASTScope* body;
    };

    bool isCtrlExpr();
    bool isSelfMutOp();
    bool isArithOp();
    bool isLiteralExpr();
    bool isComparatorExpr();
    bool isCmpOp();
    bool isBoolOp();
    void emit_tkSubscriptResolved(int level);
    void emit_tkFunctionCallResolved(int level);
    char* strchrnul(char* str, char ch);
    void lineupmultilinestring(int indent);
    void printmultilstr(char* pos);
    void emit_tkString(int level);
    void emit_tkNumber(int level);
    void emit_tkCheck(int level);
    void emit(int level);
    bool mustPromote(const char* name);
    void unmarkVisited();
    void genPrintVars(int level);
    ASTExpr* findPromotionCandidate();
    char* newTmpVarName(int num, char c);
    void lint(int level, bool spacing, bool escapeStrings);
    static ASTExpr* fromToken(const Token* this);
    bool throws();
    ASTTypeSpec* getObjectTypeSpec();
    ASTType* getEnumType();
    ASTType* getObjectType();
    ASTType* getTypeOrEnum();
    const char* typeName();
    void catarglabels();
    int strarglabels(char* buf, int bufsize);
    int countCommaList();
};