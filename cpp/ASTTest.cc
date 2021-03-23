
#include "base.hh"

void ASTTest::emit() {
    if (!this->body) return;
    printf("\nstatic void test_%s() {\n", this->name);
    this->body->emit(STEP);
    puts("}");
}

void ASTTest::lint(int level) {
    printf("test '%s'\n", this->name);
    this->body->lint(level + STEP);
    puts("end\n");
}