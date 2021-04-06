struct ASTTypeSpec {
    union {
        ASTType* type;
        char* _name;
        Interval* intv;

        ASTUnits* units;
    };
    struct {

        uint16_t dims;

        CollectionTypes collectionType : 6;
        bool hasRange : 1, hasUnits : 1;
        TypeTypes typeType : 7;
        bool nullable : 1;
    };
    ASTLocation loc[0];
    uint32_t line : 24, col, : 8;

    ASTTypeSpec* ASTTypeSpec::neew(TypeTypes tt, CollectionTypes ct);
    const char* ASTTypeSpec::name();
    const char* ASTTypeSpec::cname();
    void emit(int level, bool isconst);
    void lint(int level);
    void resolveTypeSpec(Parser* parser, ASTModule* mod);
};