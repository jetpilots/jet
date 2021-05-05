#include "jet/base.h"
#include "signal.h"

#define STEP 4
#define JOIN(x, y) x##y
#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;
static const char* const spaces = //
    "                                                                     ";

#include "types.h"
#include "token.h"
#include "ast.h"
#include "write.h"
#include "emit.h"
#include "dumpc.h"

typedef enum CompilerMode {
    PMTokenize, // just tokenize (debug only)
    PMLint, // format on stdout, ALL errors on stderr
    PMEmitC, // parse, stop on first error, or emit C
    // PMBuildO, // generate the object file
    PMMake, // run make and build the target
    PMRun, // run the executable in debug mode (build it first)
    PMTest // generate test code and run it
} CompilerMode;

static const char* CompilerMode__str[] = { //
    [PMTokenize] = "PMTokenize",
    [PMLint] = "PMLint",
    [PMEmitC] = "PMEmitC",
    [PMMake] = "PMMake",
    [PMRun] = "PMRun",
    [PMTest] = "PMTest"
};

// typedef enum DiagnosticSeverity {
//     DiagError,
//     DiagWarning,
//     DiagHint
// } DiagnosticSeverity;

// typedef struct Diagnostic {
//     unsigned short line;
//     unsigned char col;
//     DiagnosticSeverity severity : 8;
//     // DiagnosticKind kind : 8;
//     // DiagnosticEntity entity : 8;
//     union {
//         Type* type;
//         Func* func;
//         Var* var;
//         Expr* expr;
//     };
// } Diagnostic;

typedef struct IssueMgr {
    char* filename;
    uint16_t errCount, warnCount, errLimit;
    uint8_t lastError /*enum type*/, warnUnusedVar : 1, warnUnusedFunc : 1,
        warnUnusedType : 1, warnUnusedArg : 1, hasParseErrors : 1;
} IssueMgr;

typedef struct Parser {
    char* filename; // mod/submod/xyz/mycode.ch
    // char* moduleName; // mod.submod.xyz.mycode
    // char* mangledName; // mod_submod_xyz_mycode
    // char* capsMangledName; // MOD_SUBMOD_XYZ_MYCODE
    char *data, *end;
    // char* noext;
    PtrArray orig; // holds lines of original source for error reports

    Token token; // current
    IssueMgr issues;
    List(Module) * modules;

    CompilerMode mode;
    // JetOpts opts;

    bool generateCommentExprs; // set to false when compiling, set to
                               // true when linting

    // set these whenever use is detected (e.g. during resolveTypes or parsing
    // literals)
    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, ui : 1;
    } requires;
} Parser;

// static const int sgr = sizeof(Compiler);

// #define STR(x) STR_(x)
// #define STR_(x) #x

// static const char* const banner = //
//     "________     _____  _\n"
//     "______(_)______  /_ _|  The next-gen language of computing\n"
//     "_____  /_  _ \\  __/ _|  %s %s %4d-%02d-%02d\n"
//     "____  / /  __/ /_  __|\n"
//     "___  /  \\___/\\__/ ___|  https://github.com/jetpilots/jet\n"
//     "/___/ ______________________________________________________\n\n";

static void Parser_fini(Parser* parser) {
    // free(parser->data);
    // free(parser->orig.ref[0]);
    // free(parser->noext);
    // free(parser->moduleName);
    // free(parser->mangledName);
    // free(parser->capsMangledName);
}
#define FILE_SIZE_MAX 1 << 24

long recordNewlines(Parser* parser) {
    // push a new entry to get hold of the current source line later
    // this is the pointer in the original (unmodified) buffer
    char* cptr = parser->orig.ref[0];
    char* cend = cptr + (parser->end - parser->data);
    long lines = 1;
    for (char* c = cptr; c < cend; c++) {
        if (*c == '\n') {
            *c = 0;
            lines++;
            PtrArray_push(&parser->orig, c + 1);
        }
    }
    return lines;
}

static Parser* Parser_fromFile(char* filename, bool skipws, CompilerMode mode) {
    size_t flen = CString_length(filename);

    // Error: the file might not end in .ch
    if (!CString_endsWith(filename, flen, ".jet", 4)) {
        eprintf("jet: file '%s' invalid: name must end in '.jet'.\n", filename);
        return NULL;
    }

    struct stat sb;
    if (stat(filename, &sb) != 0) { // the file might not exist
        eprintf("jet: file '%s' not found.\n", filename);
        return NULL;
    } else if (S_ISDIR(sb.st_mode)) { // might really be a folder
        eprintf("jet: '%s' is a folder; only files are accepted.\n", filename);
        return NULL;
    } else if (access(filename, R_OK) == -1) { // permissions for the file
        eprintf("jet: no permission to read file '%s'.\n", filename);
        return NULL;
    }

    FILE* file = fopen(filename, "r");
    assert(file);

    Parser* ret = NEW(Parser);

    ret->filename = filename;
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file) + 2;

    // 2 null chars, so we can always lookahead
    if (size < FILE_SIZE_MAX) {
        char* data = malloc(size);
        data[size - 1] = 0;
        data[size - 2] = 0;

        fseek(file, 0, SEEK_SET);
        if (fread(data, size - 2, 1, file) != 1) {
            eprintf("jet: error: file '%s' could not be read\n", filename);
            fclose(file);
            return NULL;
            // would leak if ret was malloc'd directly, but we have a pool
        }
        fclose(file);

        ret = NEW(Parser);

        ret->filename = filename;
        ret->data = data;
        ret->end = ret->data + size - 2;
        ret->orig = (PtrArray) {};
        PtrArray_push(&ret->orig, strndup(data, size));
        ret->token = (Token) { //
            .pos = ret->data,
            .skipWhiteSpace = skipws,
            .mergeArrayDims = false,
            .kind = tkUnknown,
            .line = 1,
            .col = 1
        };
        ret->issues = (IssueMgr) { .errLimit = 50000 };
        ret->mode = mode;
        ret->generateCommentExprs = (ret->mode == PMLint);

        // If you ar not linting, even a single error is enough to stop and tell
        // the user to LINT THE DAMN FILE FIRST.
        if (ret->mode != PMLint) ret->issues.errLimit = 1;
        recordNewlines(ret);

        if (ret->orig.used > 65535) {
            eprintf("%s: error: too many lines (%u); limit is 65000\n",
                filename, ret->orig.used);
            Parser_fini(ret);
            ret = NULL;
        }
    } else {
        eprintf("%s: error: file with %zu MB exceeds 16 MB limit\n", filename,
            (size - 2) / 1024 / 1024);
    }
    return ret;
}
static bool Parser_matches(Parser* parser, TokenKind expected);

#include "errors.h"
#include "stats.h"

#pragma mark - PARSING BASICS

static Expr* exprFromCurrentToken(Parser* parser) {
    Expr* expr = Expr_fromToken(&parser->token);
    Token_advance(&parser->token);
    return expr;
}

static Expr* next_token_node(
    Parser* parser, TokenKind expected, const bool ignore_error) {
    if (parser->token.kind == expected) {
        return exprFromCurrentToken(parser);
    } else {
        if (!ignore_error) Parser_errorExpectedToken(parser, expected);
        return NULL;
    }
}
// these should all be part of Token_ when converted back to C
// in the match case, self->token should be advanced on error
static Expr* Parser_match(Parser* parser, TokenKind expected) {
    return next_token_node(parser, expected, false);
}

// this returns the match node or null
static Expr* Parser_trymatch(Parser* parser, TokenKind expected) {
    return next_token_node(parser, expected, true);
}

// just yes or no, simple
static bool Parser_matches(Parser* parser, TokenKind expected) {
    return (parser->token.kind == expected);
}

static bool Parser_ignore(Parser* parser, TokenKind expected) {
    bool ret;
    if ((ret = Parser_matches(parser, expected))) Token_advance(&parser->token);
    return ret;
}

// this is same as match without return
static void Parser_consume(Parser* parser, TokenKind expected) {
    if (!Parser_ignore(parser, expected))
        Parser_errorExpectedToken(parser, expected);
}

static char* parseIdent(Parser* parser) {
    if (parser->token.kind != tkIdentifier)
        Parser_errorExpectedToken(parser, tkIdentifier);
    char* p = parser->token.pos;
    Token_advance(&parser->token);
    return p;
}

static void getSelector(Func* func) {
    if (func->argCount) {
        size_t selLen = 0;
        int remain = 128, wrote = 0;
        char buf[128];
        buf[127] = 0;
        char* bufp = buf;

        Var* arg1 = (Var*)func->args->item;
        wrote = snprintf(bufp, remain, "%s_", TypeSpec_name(arg1->spec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        wrote = snprintf(bufp, remain, "%s", func->name);
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (Var*, arg, func->args->next) {
            wrote = snprintf(bufp, remain, "_%s", arg->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;
        }
        // TODO: why not use pstrndup here?
        func->selector = CString_pndup(buf, selLen + 1);
        // func->selector = PoolB_alloc(strPool, selLen + 1);
        // memcpy(func->selector, buf, selLen + 1);

        bufp = buf;

        wrote = snprintf(bufp, remain, "%s(", func->name);
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        wrote = snprintf(bufp, remain, "%s", TypeSpec_name(arg1->spec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (Var*, arg, func->args->next) {
            wrote = snprintf(bufp, remain, ", %s", arg->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;
        }
        selLen += snprintf(bufp, 2, ")");
        func->prettySelector = CString_pndup(buf, selLen + 1);

    } else {
        func->selector = func->name;
        char buf[128];
        buf[127] = 0;
        int n = snprintf(buf, 127, "%s()", func->name);
        func->prettySelector = CString_pndup(buf, n + 1);
        ;
    }
}

#include "resolve.h"

// this is a global astexpr representing 0. it will be used when parsing e.g.
// the colon op with nothing on either side. : -> 0:0 means the same as 1:end
static Expr lparen[] = { { .kind = tkParenOpen } };
static Expr rparen[] = { { .kind = tkParenClose } };
static Expr expr_const_0[] = { { .kind = tkNumber, .string = "0" } };
static Expr expr_const_yes[] = { { .kind = tkKeyword_yes } };
static Expr expr_const_no[] = { { .kind = tkKeyword_no } };
static Expr expr_const_nil[] = { { .kind = tkKeyword_nil } };
static Expr expr_const_empty[] = { { .kind = tkString, .string = "" } };

#include "analyse.h"
#include "parse.h"

static void Parser_emit_open(Parser* parser) {
    printf("#define THISFILE \"%s\"\n", parser->filename);
    printf("#define NUMLINES %d\n", parser->token.line);
}

static void Parser_emit_close(Parser* parser) { printf("#undef THISFILE\n"); }

static void alloc_stat() { }

static void sighandler(int sig, siginfo_t* si, void* unused) {
    write(2,
        "file:1:1-1: error: internal error: this file caused a segmentation "
        "fault (unknown location)\n",
        92);
    _exit(1);
}

int main(int argc, char* argv[]) {
    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = sighandler;
    sigaction(SIGSEGV, &sa, NULL);

    if (argc == 1) {
        eputs("jet: no input files. What are you trying to do?\n");
        return 1;
    }
    clock_Time t0 = clock_getTime();

    // JetOpts opts[1];
    // if (!getOpts(argc, argv, opts)) return 3;

    // todo: mode 'x' must be replaced with 'd', 'r', 'z' for build modes
    // TODO: these short circuits should not be here but in the mode switch
    // below. When you are in compile or build mode check these to save some
    // repeated generation. When linting this has no effect, unless of course
    // you have dumped the Jet into a binary format and want to check for that.
    // if (!(needsBuild(opts->srcfile, 'x') || opts->forceBuild)) return 0;
    // if (!(needsBuild(opts->srcfile, 'o') || opts->forceBuild)) return 0;
    // if (!(needsBuild(opts->srcfile, 'c') || opts->forceBuild)) return 0;

    CompilerMode mode = PMEmitC;
    bool stats = false;

    char* filename = argv[1];
    if (argc > 2) {
        if (*argv[2] == 'c' || *argv[2] == 'C')
            mode = PMEmitC, stats = (*argv[2] == 'C');
        if (*argv[2] == 'l' || *argv[2] == 'L')
            mode = PMLint, stats = (*argv[2] == 'L');
        if (*argv[2] == 't' || *argv[2] == 'T')
            mode = PMTest, stats = (*argv[2] == 'T');
    }
    Parser* parser = Parser_fromFile(filename, true, mode);
    if (!parser) return 2;

    parser->issues.warnUnusedArg = //
        parser->issues.warnUnusedFunc = //
        parser->issues.warnUnusedType = //
        parser->issues.warnUnusedVar = 1;

    List(Module)* modules = NULL;

    Module* root = parseModule(parser, &modules, NULL);

    if (parser->mode == PMLint) {
        if (parser->issues.hasParseErrors) {
            /* TODO: fallback to token-based linter (formatter)*/
        } else {
            foreach (Module*, mod, modules)
                Module_write(mod);
            foreach (Module*, mod, modules)
                Module_dumpc(mod);
        }
    } else if (!(parser->issues.errCount)) {
        switch (parser->mode) {

        case PMEmitC: {
            // TODO: if (monolithic) printf("#define function static\n");
            Parser_emit_open(parser);
            // ^ This is called before including the runtime, so that the
            // runtime can know THISFILE NUMLINES etc.
            printf("#include \"jet/runtime.h\"\n");
            foreach (Module*, mod, modules)
                Module_emit(mod);
            Parser_emit_close(parser);

        } break;

        case PMTest: {
            printf("#include \"jet/tester.h\"\n");
            // TODO : THISFILE must be defined since function callsites need
            // it, but the other stuff in Parser_emit_open isn't required.
            // Besides, THISFILE should be the actual module's file not the
            // test file

            foreach (Module*, mod, modules)
                Module_genTests(mod);
        } break;

        default: break;
        }
    }

    double elap = clock_clockSpanMicro(t0) / 1.0e3;
    if (stats) printstats(parser, elap);
    if (parser->issues.errCount)
        eprintf("\n\e[31m*** errors: %d\e[0m\n", parser->issues.errCount);
    if (parser->issues.warnCount)
        eprintf("\n\e[33m*** warnings: %d\e[0m\n", parser->issues.warnCount);

    return parser->issues.errCount || _InternalErrs;
}
