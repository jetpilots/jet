class ASTImport {
    char *name, *alias;
    ASTModule* module;

private:
    void emit(int level) {
        this->name.tr_ip('.', '_', 0);
        printf("\n#include \"%s.h\"\n", this->name);
        if (this->alias) printf("#define %s %s\n", this->alias, this->name);

        this->name.tr_ip('_', '.', 0);
    }

    void undefc() {
        if (this->alias) printf("#undef %s\n", this->alias);
    }

    void lint(int level) {
        printf("this %s%s%s\n", this->name, this->alias ? " as " : "",
            this->alias ? this->alias : "");
    }
};