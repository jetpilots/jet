#include "base.hh"

class ASTVar : public ASTNode {
    char* name;
    ASTTypeSpec* typeSpec;
    ASTExpr* init;

    bool used : 1, changed : 1, isLet : 1, isVar : 1, isArg : 1, stackAlloc : 1,
        isTarget : 1, visited : 1, escapes : 1, canInplace : 1, isPromise : 1,
        hasRefs : 1, obtainedBySerialization : 1, usedAsIndex : 1,
        reassigned : 1, resized : 1, returned : 1;

private:
    void genh(int level) {

        if (!this->init) return;

        this->typeSpec->emit(level, false);

        printf(" %s = ", this->name);
        this->init->emit(0);

        puts("");
    }

    void lint(int level) {
        printf("%.*s%s%s", level, spaces,
            this->isVar       ? "var "
                : this->isLet ? "let "
                              : "",
            this->name);
        TokenKind kind = this->init ? this->init->kind : tkUnknown;

        bool genType = true;

        if (kind == tkFunctionCall
            && !strcmp(this->init->string, this->typeSpec->name)) {
        } else if (kind == tkFunctionCallResolved
            && this->typeSpec->typeType == TYObject
            && !strcmp(this->init->func->name, this->typeSpec->type->name)) {

        } else if ((kind == tkNumber || kind == tkRegexp || kind == tkKeyword_no
                       || kind == tkKeyword_yes || kind == tkString
                       || kind == tkRawString)) {
        } else if (this->typeSpec->typeType == TYErrorType
            || this->typeSpec->typeType == TYNoType
            || (this->typeSpec->typeType == TYUnresolved
                && *this->typeSpec->name == '\0')) {
            genType = false;
        } else {
            genType = true;
        }

        if (genType) {
            printf(" ");
            this->typeSpec->lint(level + STEP);
        }

        if (this->init) {
            printf(" = ");
            this->init->lint(0, true, false);
        }
    }

    void emit(int level, bool isconst) {

        printf("%.*s", level, spaces);
        if (this->typeSpec) this->typeSpec->emit(level + STEP, isconst);
        printf(" %s", this->name);
    }
};