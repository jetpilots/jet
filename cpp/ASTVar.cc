#include "base.hh"

void ASTVar::genh(int level) {

    if (!this->init) return;

    this->typeSpec->emit(level, false);

    printf(" %s = ", this->name);
    this->init->emit(0);

    puts("");
}

void ASTVar::lint(int level) {
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

void ASTVar::emit(int level, bool isconst) {
    printf("%.*s", level, spaces);
    if (this->typeSpec) this->typeSpec->emit(level + STEP, isconst);
    printf(" %s", this->name);
}
