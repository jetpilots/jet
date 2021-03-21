class ASTTest {
    char* name;
    ASTScope* body;
    char* selector;
    struct {
        uint16_t line;
        struct {
            uint16_t analysed : 1;
        } flags;
    };

private:
    void emit() {
        if (!this->body) return;
        printf("\nstatic void test_%s() {\n", this->name);
        this->body.emit(STEP);
        puts("}");
    }

    void lint(int level) {
        printf("test '%s'\n", this->name);
        this->body.lint(level + STEP);
        puts("end\n");
    }
};