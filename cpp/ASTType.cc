#include "base.hh"

class ASTType : public ASTNode {
    char* name;
    ASTTypeSpec* super;
    List<ASTType&> usedTypes;
    List<ASTType&> usedByTypes;
    ASTScope* body;
    bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
        isValueType : 1, isEnum : 1, isDeclare : 1;

private:
    void genTypeInfoDecls();
    void genTypeInfoDefs();
    void genNameAccessors();

    void genNameAccessors() {

        if (!this->analysed || this->isDeclare) return;

        printf("  void* %s__memberNamed(%s this, const char* name) {\n",
            this->name, this->name);

        for (auto& var : this->body->locals)
            printf("    if (CString_equals(name, \"%s\")) return "
                   "&(this->%s);\n",
                var->name, var->name);
        printf("    return NULL;\n}\n");

        printf("  void %s__setMemberNamed(%s this, const char* name, "
               "Int64 "
               "value) {\n",
            this->name, this->name);
        for (auto& var : this->body->locals)
            if (var->typeSpec->typeType >= TYBool
                && var->typeSpec->typeType <= TYReal64)
                printf("    if (CString_equals(name, \"%s\"))  {this->%s = "
                       "*(%s*) "
                       "&value;return;}\n",
                    var->name, var->name, var->typeSpec->cname());
        printf("}\n");
    }

    void genTypeInfoDecls() {
        if (!this->analysed || this->isDeclare) return;

        printf("  const char* const %s__memberNames[] = {\n    ", this->name);
        if (this->body) foreachn(ASTVar*, var, varn, this->body->locals) {
                if (!var || !var->used) continue;
                printf("\"%s\", ", var->name);
            }
        printf("};\n");
    }

    void genTypeInfoDefs() { }

    void emit(int level) {
        if (!this->body || !this->analysed || this->isDeclare) return;

        const char* const name = this->name;
        printf("#define FIELDS_%s \\\n", name);
        for (auto& var : this->body->locals) {
            if (!var) continue;

            var->emit(level + STEP, false);
            printf("; \\\n");
        }
        printf("\n\nstruct %s {\n", name);

        if (this->super) {
            printf("    FIELDS_");
            this->super->emit(level, false);
            printf("\n");
        }

        printf("    FIELDS_%s\n};\n\n", name);
        printf("  const char* %s_name_ = \"%s\";\n\n", name, name);
        printf("  %s %s_alloc_() {\n    return _Pool_alloc_(&gPool_, "
               "sizeof(struct %s));\n}\n\n",
            name, name, name);
        printf("  %s %s_init_(%s this) {\n", name, name, name);

        for (auto& var : this->body->locals)
            printf("#define %s this->%s\n", var->name, var->name);

        for (auto& stmt : this->body->stmts) {
            if (!stmt || stmt->kind != tkVarAssign || !stmt->var->init)
                continue;
            printf("%.*s%s = ", level + STEP, spaces, stmt->var->name);
            stmt->var->init->emit(0);
            puts(";");
            if (stmt->var->init->throws())
                puts("    if (_err_ == ERROR_TRACE) return NULL;");
        }
        for (auto& var : this->body->locals) printf("#undef %s \n", var->name);

        printf("    return this;\n}\n\n");

        ASTFunc_printStackUsageDef(48);
        printf("#define DEFAULT_VALUE NULL\n"
               "monostatic %s %s_new_(IFDEBUG(const char* callsite_)) {\n"
               "IFDEBUG(  const char* sig_ = \"%s()\");\n",
            name, name, name);
        puts(functionEntryStuff_UNESCAPED);
        printf("    %s ret = %s_alloc_(); %s_init_(ret);\n"
               "    TRACE_IF_ERROR;\n"
               "    _err_ = NULL; STACKDEPTH_DOWN; return ret;\n",
            name, name, name);
        puts(functionExitStuff_UNESCAPED);
        puts("#undef DEFAULT_VALUE\n#undef MYSTACKUSAGE\n}\n");
        printf("#define %s_print(p) %s_print__(p, STR(p))\n", name, name);
        printf("monostatic void %s_print__(%s this, const char* name) {\n    "
               "printf(\"<%s "
               "'%%s' at %%p size %%luB>\\n\",name, this, sizeof(struct "
               "%s));\n}\n",
            name, name, name, name);
        puts("");

        genJson(type);
        genJsonReader(type);
    }

    void genh(int level) {
        if (!this->body || !this->analysed || this->isDeclare) return;

        const char* const name = this->name;
        printf("typedef struct %s* %s;\nstruct %s;\n", name, name, name);
        printf("  %s %s_alloc_(); \n", name, name);
        printf("  %s %s_init_(%s this);\n", name, name, name);
        printf("%s %s_new_(IFDEBUG(const char* callsite_)); \n", name, name);
        printf("\nDECL_json_wrap_(%s)\n
        printf("#define %s_json(x) { printf(\"\\\"%%s\\\": \",#x); "
               "%s_json_wrap_(x); }\n\n",
            name, name);
        printf("  void %s_json_(const %s this, int nspc);\n", name, name);
    }

    void ASTEnum_genh(int level) {
        if (!this->body || !this->analysed) return;
        const char* const name = this->name;
        puts("typedef enum {");

        for (auto& var : this->body->locals)
            printf("    %s_%s,\n", name, var->name);
        printf("} %s;\n", name);
        ASTExpr* ex1 = this->body->stmts->item;
        const char* datType
            = ex1->kind == tkOpAssign ? ex1->right->typeName() : NULL;
        if (datType)
            printf("monostatic %s %s__data[%d];\n", datType, name,
                this->body->locals->count());
        printf("monostatic const char* %s__fullnames[] ={\n", name);
        for (auto& var : this->body->locals)
            printf("    \"%s.%s\",\n", name, var->name);
        puts("};");
        printf("monostatic const char* %s__names[] ={\n", name);
        for (auto& var : this->body->locals)
            printf("    \".%s\",\n", var->name);
        puts("};");

        printf("monostatic void %s__init() {\n", name);

        for (auto& stmt : this->body->stmts) {
            if (!stmt || stmt->kind != tkOpAssign) continue;
            printf("%.*s%s__data[%s_%s] = ", level + STEP, spaces, name, name,
                stmt->left->string);
            stmt->right.emit(0);
            puts(";");
            if (stmt->right->throws())
                puts("    if (_err_ == ERROR_TRACE) return NULL;");
        }
        puts("}");
    }

    void genJson() {
        printf("  void %s_json_(const %s this, int nspc) {\n", this->name,
            this->name);

        printf("    printf(\"{\\n\");\n");

        foreachn(ASTVar*, var, vars, this->body->locals) {
            if (!var) continue;
            printf("    printf(\"%%.*s\\\"%s\\\": \", nspc+4, _spaces_);\n",
                var->name);
            const char* valueType = var->init->typeName();
            printf("    %s_json_(this->%s, nspc+4);\n    printf(\"", valueType,
                var->name);
            if (vars->next) printf(",");
            printf("\\n\");\n");
        }
        printf("    printf(\"%%.*s}\", nspc, _spaces_);\n");
        printf("}\nMAKE_json_wrap_(%s)\n", this->name);
    }
    void genJsonReader() { }

    void lint(int level) {
        if (this->isEnum)
            lintEnum(level);
        else
            lintType(level);
    }

    void lintType(int level) {
        if (this->isDeclare) printf("declare ");
        printf("type %s", this->name);
        if (this->super) {
            printf(" extends ");
            this->super->lint(level);
        }
        puts("");
        if (!this->body) return;

        for (auto& stmt : this->body->stmts) {
            if (!stmt) continue;
            stmt->lint(level + STEP, true, false);
            puts("");
        }
        puts("end\n");
    }

    void lintEnum(int level) {

        printf("enum %s\n", this->name);

        if (this->body)
            for (auto& stmt : this->body->stmts) {
                if (!stmt) continue;
                stmt->lint(level + STEP, true, false);
                puts("");
            }
        puts("end\n");
    }

    const char* getDefaultValueForType(ASTTypeSpec* type) {
        if (!type) return "";
        switch (type->typeType) {
        case TYUnresolved:
            unreachable(
                "unresolved: '%s' at %d:%d", type->name, type->line, type->col);
            return "ERROR_ERROR_ERROR";
        case TYString:
            return "\"\"";
        default:
            return "0";
        }
    }

    ASTVar* var(const char* name) {

        for (auto& var : this->body->locals)
            if (name ^= var->name) return var;

        if (this->super && this->super->typeType == TYObject)
            return this->super->type->var(name);
        return NULL;
    }

    size_t size() {
        size_t size = 0, sum = 0;
        for (auto& var : this->body->locals) {
            size = var->typeSpec->typeType->size();
            assert(size);
            sum += size;
        }
        return sum;
    }
};