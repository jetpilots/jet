#include "jet/base.h"
#include "jet/os/Process.h"
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

#include "lower.h"

#define outln(s) fwrite(s "\n", sizeof(s ""), 1, outfile)
#define outl(s) fwrite(s "", sizeof(s "") - 1, 1, outfile)
#define iprintf(nspc, fmt, ...)                                            \
  printf("%.*s", nspc, spaces), printf(fmt, __VA_ARGS__)
#define printf(...) fprintf(outfile, __VA_ARGS__)
#define puts(s) fputs(s, outfile), fputs("\n", outfile)

static thread_local FILE* outfile = NULL;
#include "write.h"
#include "emit.h"
#include "dumpc.h"

#undef outln
#undef outl
#undef iprintf
#undef printf
#undef puts

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

#include "parser.h"

#include "errors.h"
#include "stats.h"

#include "resolve.h"

#include "analyse.h"
#include "parse.h"

#include "serv.h"

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

int main(int argc, char* argv[]) {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sighandler;
  sigaction(SIGSEGV, &sa, NULL);

  // if (argc == 1) {
  //   eputs("cjet: no input file specified. What are you trying to do?\n"
  //         "      if you want language-server mode, it's `cjet -s`.\n");
  //   return 1;
  // }

  // JetOpts opts[1];
  // if (!getOpts(argc, argv, opts)) return 3;

  // todo: mode 'x' must be replaced with 'd', 'r', 'z' for build modes
  // TODO: these short circuits should not be here but in the mode switch
  // below. When you are in compile or build mode check these to save some
  // repeated generation. When linting this has no effect, unless of course
  // you have dumped the Jet into a binary format and want to check for
  // that. if (!(needsBuild(opts->srcfile, 'x') || opts->forceBuild)) return
  // 0; if (!(needsBuild(opts->srcfile, 'o') || opts->forceBuild)) return 0;
  // if (!(needsBuild(opts->srcfile, 'c') || opts->forceBuild)) return 0;

  CompilerMode mode = PMEmitC;
  bool stats = false;

  char* filename = (argc > 1) ? argv[1] : NULL;

  // language server mode with no options to jetc
  // TODO: later this should be REPL mode, with "-s" argument for
  // language server.
  if (!filename) return langserver(argc, argv);

  if (argc > 2) {
    if (*argv[2] == 'c' || *argv[2] == 'C')
      mode = PMEmitC, stats = (*argv[2] == 'C');
    if (*argv[2] == 'l' || *argv[2] == 'L')
      mode = PMLint, stats = (*argv[2] == 'L');
    if (*argv[2] == 't' || *argv[2] == 'T')
      mode = PMTest, stats = (*argv[2] == 'T');
  }
  clock_Time t0 = clock_getTime();
  Parser* parser = par_fromFile(filename, true, mode);
  if (!parser) return 2;

  parser->issues.warnUnusedArg = //
      parser->issues.warnUnusedFunc = //
      parser->issues.warnUnusedType = //
      parser->issues.warnUnusedVar = 1;

  List(Module)* modules = NULL;

  Module* root = parseModule(parser, &modules, NULL);
  if (_InternalErrs) {
    // nothing
  } else if (parser->mode == PMLint) {
    if (parser->issues.hasParseErrors) {
      /* TODO: fallback to token-based linter (formatter)*/
    } else {
      foreach (Module*, mod, modules) { mod_write(mod); }
      foreach (Module*, mod, modules) { mod_dumpc(mod); }
    }
  } else if (!(parser->issues.errCount)) {
    switch (parser->mode) {

    case PMEmitC: {
      // TODO: if (monolithic) printf("#define function static\n");
      par_emit_open(parser);
      // ^ This is called before including the runtime, so that the
      // runtime can know THISFILE NUMLINES etc.
      printf("#include \"jet/runtime.h\"\n");
      foreach (Module*, mod, modules) { mod_emit(mod); }
      parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
      foreach (Module*, mod, modules) {
        // TODO: check if the file needs updating and only then run cc
        char* enginePath = "engine"; // FIXME
        char* cmd[]
            = { "/usr/bin/gcc", "-I", enginePath, "-c", mod->out_c, NULL };
        Process_launch(cmd);
      }

      // TODO: do something useful while cc is running

      // here count the actual number of valid pids
      Process proc;
      do {
        proc = Process_awaitAny();
        if (proc.exited && proc.code) unreachable("cc failed\n", "");
        // here launch 1 more
      } while (proc.pid);
      par_emit_close(parser);

    } break;

    case PMTest: {
      printf("#include \"jet/tester.h\"\n");
      // TODO : THISFILE must be defined since function callsites need
      // it, but the other stuff in par_emit_open isn't required.
      // Besides, THISFILE should be the actual module's file not the
      // test file

      foreach (Module*, mod, modules) { mod_genTests(mod); }
    } break;

    default: break;
    }
  }
  parser->elap_tot = clock_clockSpanMicro(t0) / 1.0e3;
  if (stats) printstats(parser);
  eputs("\n");
  if (parser->issues.warnCount)
    eprintf("\e[33m*** warnings: %d\e[0m\n", parser->issues.warnCount);
  if (parser->issues.errCount)
    eprintf("\e[31m*** errors: %d\e[0m\n", parser->issues.errCount);
  if (_InternalErrs)
    eputs("\e[31m*** an internal error has ocurred.\e[0m\n");

  return parser->issues.errCount || _InternalErrs;
}
