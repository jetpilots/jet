struct ASTTest {
    char* name;
    ASTScope* body;
    char* selector;
    struct {
        uint16_t line;
        struct {
            uint16_t analysed : 1;
        } flags;
    };
    void emit();
    void lint(int level);
};