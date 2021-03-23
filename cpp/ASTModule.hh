class ASTModule {
    char* name;
    List<ASTFunc&> funcs;
    List<ASTTest&> tests;

    List<ASTType&> types;
    ASTScope scope[1];

    List<ASTImport&> imports;
    List<ASTType&> enums;
    List<ASTModule&> importedBy;

    char* moduleName;

    void lint();
    void emit();
    void genTests();

    ASTImport* import(const char* alias);
    ASTFunc* func(const char* selector);
    ASTVar* var(const char* name);
    ASTType* type(const char* name);
};