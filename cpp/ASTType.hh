struct ASTType : public ASTNode {
    char* name;
    ASTTypeSpec* super;
    List<ASTType&> usedTypes;
    List<ASTType&> usedByTypes;
    ASTScope* body;
    bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
        isValueType : 1, isEnum : 1, isDeclare : 1;

    void genNameAccessors();
    void genTypeInfoDecls();
    void emit(int level);
    void genh(int level);
    void genhEnum(int level);
    void genJson();
    void genJsonReader();
    void lint(int level);
    void lintType(int level);
    void lintEnum(int level);
    const char* getDefaultValueForType(ASTTypeSpec* type);
    ASTVar* var(const char* name);
    size_t size();
};