#define eputs(s) fputs(s, stderr)
#define eprintf(s, ...) fprintf(stderr, __VA_ARGS__)
#define unreachable(...) fprintf(stderr, __VA_ARGS__)

#include "List.hpp"

#include "ast/Var.hpp"
#include "ast/Expr.hpp"
#include "ast/Scope.hpp"
#include "ast/Type.hpp"
#include "ast/Func.hpp"
#include "ast/Import.hpp"
#include "ast/Module.hpp"

#include "Diagnostics.hpp"
#include "Parser.hpp"

#include "pass/Analyzer.hpp"
#include "pass/Optimizer.hpp"
#include "pass/CodeGenC99.hpp"
#include "pass/CodeGenMIR.hpp"
#include "pass/Writer.hpp"

#include "Compiler.hpp"

int main(int argc, char* argv[]) {
    Compiler com(argc, argv);
    com.start();
}
