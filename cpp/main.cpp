

#include "jet/base.h"

#define STEP 4

#include "types.h"
#include "tokenKind.h"
#include "token.h"

#define JOIN(x, y) x##y

#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;

static const char* const spaces
    = "                                                                     ";

#pragma mark - AST TYPE DEFINITIONS

struct ASTLocation {
    uint32_t line : 24, col : 8;
};

#pragma mark - AST IMPORT IMPL.

#pragma mark - AST UNITS IMPL.

static uint32_t exprsAllocHistogram[128];

#pragma mark - AST FUNC IMPL.

#pragma mark - AST EXPR IMPL.
#pragma mark - AST MODULE IMPL.

#include "lint.h"
#include "emit.h"

#pragma mark - PARSER

typedef enum ParserMode { PMLint, PMEmitC, PMGenTests } ParserMode;
struct IssueMgr {
    uint16_t errCount, warnCount, errLimit;
    uint8_t lastError, warnUnusedVar : 1, warnUnusedFunc : 1,
        warnUnusedType : 1, warnUnusedArg : 1, hasParseErrors : 1;
} IssueMgr;

struct Parser {
} Parser;

static const int sgr = sizeof(Parser);

#define STR(x) STR_(x)
#define STR_(x) #x

static const char* const banner
    = "________     _____  _\n"
      "______(_)______  /_ _|  The next-gen language of computing\n"
      "_____  /_  _ \\  __/ _|  %s %s %4d-%02d-%02d\n"
      "____  / /  __/ /_  __|\n"
      "___  /  \\___/\\__/ ___|  https:
      "/___/ ______________________________________________________\n\n";
#define FILE_SIZE_MAX 1 << 24

#include "errors.h"
#include "stats.h"

#pragma mark - PARSING BASICS

#include "resolve.h"

static ASTExpr lparen[] = { { .kind = tkParenOpen } };
static ASTExpr rparen[] = { { .kind = tkParenClose } };
static ASTExpr expr_const_0[] = { { .kind = tkNumber, .string = "0" } };
static ASTExpr expr_const_yes[] = { { .kind = tkKeyword_yes } };
static ASTExpr expr_const_no[] = { { .kind = tkKeyword_no } };
static ASTExpr expr_const_nil[] = { { .kind = tkKeyword_nil } };
static ASTExpr expr_const_empty[] = { { .kind = tkString, .string = "" } };

#include "analyse.h"

#include "parse.h"

void Parser::emit_open(Parser* parser) {
    printf("#ifndef HAVE_%s\n#define HAVE_%s\n\n", parser->capsMangledName,
        parser->capsMangledName);
    printf("#define THISMODULE %s\n", parser->mangledName);
    printf("#define THISFILE \"%s\"\n", parser->filename);
    printf("#define NUMLINES %d\n", parser->token.line);
}

void Parser::emit_close(Parser* parser) {
    printf("#undef THISMODULE\n");
    printf("#undef THISFILE\n");
    printf("#endif
}

void alloc_stat() { }

#include "ptr2off.h"

#pragma mark - main
int main(int argc, char* argv[]) {

    if (argc == 1) {
        eputs("jet: no input files. What are you trying to do?\n");
        return 1;
    }
    bool printDiagnostics = (argc > 2 && *argv[2] == 'd') || false;

    clock_Time t0 = clock_getTime();
    List<ASTModule*> modules;
    Parser* parser;

    ParserMode mode = PMEmitC;
    if (argc > 3 && *argv[3] == 'l') mode = PMLint;
    if (argc > 2 && *argv[2] == 'l') mode = PMLint;
    if (argc > 3 && *argv[3] == 't') mode = PMGenTests;
    if (argc > 2 && *argv[2] == 't') mode = PMGenTests;
    parser = Parser::fromFile(argv[1], true, mode);
    if (!parser) return 2;

    ASTModule* root = parseModule(parser, &modules, NULL);

    if (parser->mode == PMLint) {
        if (parser->issues.hasParseErrors) {

        } else {
            foreach (ASTModule*, mod, modules)
                ASTModule::lint(mod);
        }
    } else if (!(parser->issues.errCount)) {
        switch (parser->mode) {

        case PMEmitC: {

            Parser_emit_open(parser);

            printf("#include \"jet/runtime.h\"\n");
            foreach (ASTModule*, mod, modules)
                ASTModule::emit(mod);
            Parser_emit_close(parser);
        } break;

        case PMGenTests: {
            printf("#include \"jet/tester.h\"\n");

            foreach (ASTModule*, mod, modules)
                ASTModule::genTests(mod);
        } break;

        default:
            break;
        }
    }

    double elap = clock_clockSpanMicro(t0) / 1.0e3;
    if (printDiagnostics) printstats(parser, elap);

    if (parser->issues.errCount) {
        eprintf("\n\e[31;1;4m THERE ARE %2d ERRORS.                        "
                "      "
                "                         \e[0m\n How about fixing them? "
                "Reading "
                "the code would be a good start.\n",
            parser->issues.errCount);
    }

    if (parser->issues.warnCount)
        eprintf("\n\e[33m*** %d warnings\e[0m\n", parser->issues.warnCount);

    if (!printDiagnostics) eprintf("*** cgen in %.2f ms\n", elap);

    int ret = (parser->issues.errCount || _InternalErrs);

    return ret;
}
