struct ASTFunc : public ASTNode {
    char* name;
    ASTScope* body;
    List<ASTVar&> args;
    List<ASTVar&> callers, callees;
    ASTTypeSpec* returnSpec;
    char *selector, *prettySelector;
    struct {
        struct {
            uint16_t throws : 1, isRecursive : 1, usesSerialisation : 1,
                isExported : 1, usesReflection : 1, nodispatch : 1, isStmt : 1,
                isDeclare : 1, isCalledFromWithinLoop : 1, elemental : 1,
                isDefCtor : 1, intrinsic : 1,

                analysed : 1, isCalledAsync : 1, returnsNewObjectSometimes : 1,
                returnsNewObjectAlways : 1;
        };
        //  char argCount, nameLen;
    };

    void genh(int level);
    void emit(int level);
    void printStackUsageDef(size_t stackUsage);
    void lint(int level);
    ASTFunc* createDeclWithArg(char* name, char* retType, char* arg1Type);
    size_t size();
    void getSelector();
};
