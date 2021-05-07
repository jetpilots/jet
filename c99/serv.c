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
#include "clone.h"
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

// static const char* CompilerMode__str[] = { //
//     [PMTokenize] = "PMTokenize",
//     [PMLint] = "PMLint",
//     [PMEmitC] = "PMEmitC",
//     [PMMake] = "PMMake",
//     [PMRun] = "PMRun",
//     [PMTest] = "PMTest"
// };

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
#include "parser.h"
#include "errors.h"
#include "stats.h"

#pragma mark - PARSING BASICS

#include "resolve.h"

// this is a global astexpr representing 0. it will be used when parsing
// e.g. the colon op with nothing on either side. : -> 0:0 means the same as
// 1:end
static Expr lparen[] = { { .kind = tkParenOpen } };
static Expr rparen[] = { { .kind = tkParenClose } };
static Expr expr_const_0[] = { { .kind = tkNumber, .str = "0" } };
static Expr expr_const_yes[] = { { .kind = tkYes } };
static Expr expr_const_no[] = { { .kind = tkNo } };
static Expr expr_const_nil[] = { { .kind = tkNil } };
static Expr expr_const_empty[] = { { .kind = tkString, .str = "" } };

#include "analyse.h"
#include "parse.h"

static void par_emit_open(Parser* parser) {
  printf("#define THISFILE \"%s\"\n", parser->filename);
  printf("#define NUMLINES %d\n", parser->token.line);
}

static void par_emit_close(Parser* parser) { printf("#undef THISFILE\n"); }

static void alloc_stat() { }

static void sighandler(int sig, siginfo_t* si, void* unused) {
  write(2,
      "file:1:1-1: error: internal error: this file caused a segmentation "
      "fault (unknown location)\n",
      92);
  _exit(1);
}
#define cmd(s) else if (!strncasecmp(line, s "", sizeof(s "") - 1))

#define LINESIZE 1024

#include "linenoise.c"

int main(int argc, char* argv[]) {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sighandler;
  sigaction(SIGSEGV, &sa, NULL);

  CompilerMode mode = PMLint;
  List(Module)* modules = NULL;
  Parser* parser;
  Module* root;

  // static char linebuf[LINESIZE];
  char* line; //= linebuf;
  long len;
  while ((line = linenoise("jet> "))) {
    // printf("jet> ");
    // fgets(linebuf, LINESIZE, stdin);
    // line = linenoise("jet> ");//linebuf;
    while (*line == ' ') line++;
    len = strlen(line);
    if (line[len - 1] == '\n') line[len - 1] = 0;
    if (len > 2 && line[len - 2] == '\r') line[len - 2] = 0;

    if (0) { }
    cmd("exit") { break; }
    cmd("load") {
      clock_Time t0 = clock_getTime();
      char* filename = line + 5;
      while (*filename == ' ') filename++;

      parser = par_fromFile(filename, true, mode);
      if (!parser) continue;
      parser->issues.warnUnusedArg = //
          parser->issues.warnUnusedFunc = //
          parser->issues.warnUnusedType = //
          parser->issues.warnUnusedVar = 1;
      root = parseModule(parser, &modules, NULL);
      parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
    }
    cmd("drop") { }
    cmd("mods") {
      foreach (Module*, mod, modules)
        printf("%s %s %s\n", mod->name, mod->fqname, mod->filename);
    }
    cmd("stat") { printstats(parser, parser->elap); }
    cmd("syms") {
      foreach (Type*, type, root->types) {
        printf("%d:%d: %s\n", type->line, type->col, type->name);
        foreach (Var*, var, type->body->locals)
          printf("%d:%d: - %s\n", var->line, var->col, var->name);
      }
      foreach (Type*, enm, root->enums)
        printf("%d:%d: %s\n", enm->line, enm->col, enm->name);
      foreach (Func*, func, root->funcs)
        if (!func->intrinsic)
          printf("%d:%d: %s\n", func->line, func->col, func->psel);
      foreach (JetTest*, test, root->tests)
        printf("%d:%d: %s\n", test->line, 0, test->name);
    }
    cmd("def") { }
    cmd("expr") {
      Expr* ret;
      Func* fn;
      Scope* sco;
      Type* ty;
      char* pos = NULL;
      int col, lno = strtod(line + 5, &pos);
      if (pos) col = strtod(pos, NULL);
      ret = mod_getExpr(root, lno, col, &fn, &sco, &ty);
      if (ret) {
        expr_write(ret, 0, yes, no);
        puts("");
        if (fn)
          printf("from func %s at %d:%d\n", fn->psel, fn->line, fn->col);
        if (ty)
          printf("from type %s at %d:%d\n", ty->name, ty->line, ty->col);
      } else {
        eprintf("nothing found at %d:%d\n", lno, col);
      }
    }
    cmd("write") { mod_write(root); }
    cmd("dump") { mod_dumpc(root); }
    cmd("emit") { mod_emit(root); }
    else {
      fputs("Commands:\n"
            "    mods\n"
            "    load <file>\n"
            "    drop <file>\n"
            "    stat <file>\n"
            "    syms <file>\n"
            "    def <line> <col> <file>\n"
            "    expr <line> <col> <file> \n"
            "    write <file>\n"
            "    dump <file>\n"
            "    emit <file>\n"
            "    exit\n",
          stderr);
    }
  }

  return 0;
}
