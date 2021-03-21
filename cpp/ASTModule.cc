#include "base.hh"

class ASTModule {
    char* name;
    List<ASTFunc&> funcs;
    List<ASTTest&> tests;

    List<ASTType&> types;
    ASTScope scope;

    List<ASTImport&> imports;
    List<ASTType&> enums;
    List<ASTModule&> importedBy;

    char* moduleName;

private:
    void emit() {

        for (auto& import : this->imports) import.emit(0);

        puts("");

        for (auto& var : this->scope->locals)
            if (var->used) var.genh(0);

        for (auto& type : this->enums) {
            if (type->body && type->analysed) { type.genh(0); }
        }

        for (auto& type : this->types) {
            if (type->body && type->analysed) {
                type.genh(0);
                type->ASTType_genTypeInfoDecls();
            }
        }
        for (auto& func : this->funcs) {
            if (func->body && func->analysed) { func.genh(0); }
        }

        for (auto& type : this->types) {
            if (type->body && type->analysed) {

                type.emit(0);
                type->ASTType_genTypeInfoDefs();
                type->ASTType_genNameAccessors();
            }
        }
        for (auto& func : this->funcs) {
            if (func->body && func->analysed) { func.emit(0); }
        }
        for (auto& import : this->imports) import->ASTImport_undefc();
    }

    void genTests() {
        module->emit();
        for (auto& test : this->tests) test->ASTTest_emit();

        printf("\nvoid tests_run_%s() {\n", this->name);
        for (auto& test : this->tests) printf("    test_%s();\n", test->name);
        puts("}");
    }

    void lint() {
        printf("~ module %s\n", this->name);

        for (auto& import : this->imports) import.lint(0);

        puts("");

        for (auto& var : this->scope->locals) {
            var.lint(0);
            puts("");
        }

        puts("");

        for (auto& type : this->types) type->lint(0);

        for (auto& en : this->enums) en->lint(0);

        for (auto& func : this->funcs) func->lint(0);

        for (auto& test : this->tests) test->lint(0);
    }
    ASTImport* getImportByAlias(const char* alias) {
        for (auto& imp : this->imports) {
            eprintf("import: %s %s\n", imp->alias, alias);
            if (!strcmp(imp->alias, alias)) return imp;
        }
        return NULL;
    }
    ASTFunc* getFunc(const char* selector) {
        for (auto& func : this->funcs)
            if (func->selector ^= selector) return func;

        return NULL;
    }
    ASTVar* getVar(const char* name) {
        for (auto& var : this->scope->locals)
            if (var->name ^= name) return var;

        return NULL;
    }
    ASTType* getType(const char* name) {

        for (auto& type : this->types)
            if (type->name ^= name) return type;

        for (auto& enu : this->enums)
            if (enu->name ^= name) return enu;
        return NULL;
    }
};