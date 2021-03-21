#include "base.hh"

class ASTFunc : public ASTNode {
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
        char argCount, nameLen;
    };

private:
    const char* functionEntryStuff_UNESCAPED
        = "    STACKDEPTH_UP; DO_STACK_CHECK;\n";

    const char* functionExitStuff_UNESCAPED
        = "    return DEFAULT_VALUE;\n"
          "uncaught: HANDLE_UNCAUGHT;\n"
          "backtrace: SHOW_BACKTRACE_LINE;\n"
          "return_: STACKDEPTH_DOWN;\n"
          "    return DEFAULT_VALUE;";

    void genh(int level) {
        if (!this->body || !this->analysed || this->isDeclare) return;
        if (!this->isExported) printf("  ");
        if (this->returnSpec) {
            this->returnSpec->emit(level, false);
        } else {
            printf("void");
        }
        printf(" %s(", this->selector);
        foreachn(ASTVar*, arg, args, this->args) {
            arg->emit(level, true);
            printf(args->next ? ", " : "");
        }
        printf("\n#ifdef DEBUG\n    %c const char* callsite_\n#endif\n",
            ((this->args && this->args->item) ? ',' : ' '));
        puts(");\n");
    }

    void emit(int level) {
        if (!this->body || !this->analysed || this->isDeclare) return;

        size_t stackUsage = this->size();
        stackUsage->printStackUsageDef();

        printf("#define DEFAULT_VALUE %s\n",
            this->returnSpec->getDefaultValueForType());
        if (!this->isExported) printf("  ");
        if (this->returnSpec) {
            this->returnSpec->emit(level, false);
        } else {
            printf("void");
        }
        printf(" %s(", this->selector);
        foreachn(ASTVar*, arg, args, this->args) {
            arg->emit(level, true);
            printf(args->next ? ", " : "");
        }

        printf("\n#ifdef DEBUG\n"
               "    %c const char* callsite_ "
               "\n#endif\n",
            ((this->args && this->args->item ? ',' : ' ')));

        puts(") {");
        printf("    IFDEBUG(  const char* sig_ = \"");
        printf("%s%s(", this->isStmt ? "" : "function ", this->name);

        foreachn(ASTVar*, arg, args, this->args) {
            arg->lint(level);
            printf(args->next ? ", " : "");
        }
        printf(")");
        if (this->returnSpec) {
            printf(" as ");
            this->returnSpec->lint(level);
        }
        puts("\");");

        puts(functionEntryStuff_UNESCAPED);

        this->body->emit(level + STEP);

        puts(functionExitStuff_UNESCAPED);
        puts("}\n#undef DEFAULT_VALUE");
        puts("#undef MYSTACKUSAGE");
    }

    void printStackUsageDef(size_t stackUsage) {
        printf("#define MYSTACKUSAGE (%lu + 6*sizeof(void*) + "
               "IFDEBUGELSE(sizeof(char*),0))\n",
            stackUsage);
    }

    void lint(int level) {
        if (this->isDefCtor || this->intrinsic) return;
        if (this->isDeclare) printf("declare ");

        printf("%s%s(", this->isStmt ? "\n" : "function ", this->name);

        foreachn(ASTVar*, arg, args, this->args) {
            arg->lint(level);
            printf(args->next ? ", " : "");
        }
        printf(")");

        if (this->returnSpec && !this->isStmt) {
            printf(" as ");
            this->returnSpec->lint(level);
        }
        if (this->isDeclare) {
            puts("");
            return;
        } else if (!this->isStmt) {
            puts("");
            this->body->lint(level + STEP);
            puts("end\n");
        } else {
            ASTExpr* def = this->body->stmts->item;
            def = def->right;
            printf(" := ");
            def->lint(0, true, false);
            puts("\n");
        }
    }
    ASTFunc* createDeclWithArg(char* name, char* retType, char* arg1Type) {
        = NEW(ASTFunc);
        this->name = name;
        this->isDeclare = true;
        if (retType) {
            this->returnSpec = NEW(ASTTypeSpec);
            this->returnSpec->name = retType;
        }
        if (arg1Type) {
            ASTVar* arg = NEW(ASTVar);
            arg->name = "arg1";
            arg->typeSpec = NEW(ASTTypeSpec);
            arg->typeSpec->name = arg1Type;
            append(&this->args, arg);
            this->argCount = 1;
        }
        return this;
    }

    size_t size() {
        size_t size = 0, sum = 0;
        for (auto& arg : this->args) {
            size = arg->typeSpec->typeType->size();
            assert(size);
            sum += size;
        }
        if (this->body) sum += this->body->size();
        return sum;
    }

    void getSelector() {
        if (this->argCount) {
            size_t selLen = 0;
            int remain = 128, wrote = 0;
            char buf[128];
            buf[127] = 0;
            char* bufp = buf;

            ASTVar* arg1 = (ASTVar*)this->args->item;
            wrote = snprintf(
                bufp, remain, "%s_", arg1->typeSpec->ASTTypeSpec::name());
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            wrote = snprintf(bufp, remain, "%s", this->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            for (auto& arg : this->args->next) {
                wrote = snprintf(bufp, remain, "_%s", arg->name);
                selLen += wrote;
                bufp += wrote;
                remain -= wrote;
            }

            this->selector = buf->pstrndup(selLen + 1);

            bufp = buf;

            wrote = snprintf(bufp, remain, "%s(", this->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            wrote = snprintf(bufp, remain, "%s", arg1->typeSpec->name());
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            for (auto& arg : this->args->next) {
                wrote = snprintf(bufp, remain, ", %s", arg->name);
                selLen += wrote;
                bufp += wrote;
                remain -= wrote;
            }
            selLen += snprintf(bufp, 2, ")");
            this->prettySelector = buf->pstrndup(selLen + 1);

        } else {
            this->selector = this->name;
            char buf[128];
            buf[127] = 0;
            int n = snprintf(buf, 127, "%s()", this->name);
            this->prettySelector = buf->pstrndup(n + 1);
            ;
        }
    }
};