struct ASTImport {
    char *name, *alias;
    ASTModule* module;
    void emit(int level);
    void undefc();
    void lint(int level);
};