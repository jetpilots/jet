#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <errno.h>

#include "jet/base.h"
#include "jet/os/Process.h"

static const char* _lastFunc;
static int _lastFuncLen;

#define FUNC_ENTRY _lastFunc = __func__, _lastFuncLen = strlen(_lastFunc);

#define STEP 4
#define JOIN(x, y) x##y
#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;
static const char* const spaces = //
    "                                                                     ";

#include "types.h"
#include "token.h"
#include "ast.h"
#include "clone.h"
#include "templ.h"
#include "lower.h"

#define outl(s) fwrite(s "", sizeof(s "") - 1, 1, outfile)
#define outln(s) outl(s "\n") // fwrite(s "\n", sizeof(s ""), 1, outfile)
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
#include "filehash.h"

static void par_emit_open(Parser* parser) {
  // printf("#define THISFILE \"%s\"\n", parser->filename);
  // printf("#define NUMLINES %d\n", parser->token.line);
}

static void par_emit_close(Parser* parser) {
  // printf("#undef THISFILE\n");
}

static void alloc_stat() { }

static void sighandler(int sig, siginfo_t* si, void* unused) {
  write(2, _lastFunc, _lastFuncLen);
  write(2,
      ":1:1-1: error: internal error: this file caused a segmentation "
      "fault (unknown location)\n",
      88);
  _exit(1);
}

bool file_newer(const char* file, const char* than) {
  static const unsigned long ONE_NANO = 1000000000;
  struct stat sb1 = {}, sb2 = {};
  if (stat(file, &sb1) + stat(than, &sb2)) return false;
  // ^ one or both file(s) not found or other error
  if (sb1.st_mtime > sb2.st_mtime) return true;
  size_t time1 = sb1.st_mtime * ONE_NANO + sb1.st_mtimensec;
  size_t time2 = sb2.st_mtime * ONE_NANO + sb2.st_mtimensec;
  return time1 > time2;
}

int main(int argc, char* argv[]) {
  FUNC_ENTRY
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
  bool forceBuildAll = false;
  bool monolithicBuild = true;

  char* filename = (argc > 2) ? argv[2] : (argc > 1) ? argv[1] : NULL;

  // language server mode with no options to jetc
  // TODO: later this should be REPL mode, with "-s" argument for
  // language server.
  if (!filename) return langserver(argc, argv);

  if (argc > 2) {
    char m = argv[1][1]; // -cClLrRtT
    if (m == 'c' || m == 'C') mode = PMEmitC, stats = (m == 'C');
    if (m == 'l' || m == 'L') mode = PMLint, stats = (m == 'L');
    if (m == 't' || m == 'T') mode = PMTest, forceBuildAll = (m == 'T');
    if (m == 'r' || m == 'R') mode = PMRun, forceBuildAll = (m == 'R');
  }
  clock_Time t0 = clock_getTime();

  Parser* parser = strcmp(filename, "-")
      ? par_fromFile(filename, true, mode)
      : par_fromStdin(true, mode);
  if (!parser) return 2;

  parser->issues.warnUnusedArg = //
      parser->issues.warnUnusedFunc = //
      parser->issues.warnUnusedType = //
      parser->issues.warnUnusedVar = (mode != PMRun && mode != PMTest);

  List(Module)* modules = NULL;

  Module* root = parseModule(parser, &modules, NULL);
  parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
  int modifiedMods = 0;

  // int b = file_hash_equal("/usr/bin/gcc", "/usr/bin/yes");
  // printf("---- %d\n", b);
  if (_InternalErrs) {
    // nothing
  } else if (parser->mode == PMLint) {
    if (parser->issues.hasParseErrors) {
      /* TODO: fallback to token-based linter (formatter)*/
    } else {
      foreach (Module*, mod, modules) { mod_write(mod); }
      foreach (Module*, mod, modules) { mod_dumpc(mod); }
    }
    parser->oelap = clock_clockSpanMicro(t0) / 1.0e3;

  } else if (!(parser->issues.errCount)) {
    Func* fstart = NULL;
    switch (parser->mode) { // REMOVE THIS SWITCH!! USELESS
    case PMRun:
      // don't break on the first match, keep looking so that duplicate
      // starts can be found
      foreach (Func*, func, root->funcs) {
        if (!strcmp(func->name, "start")) {
          if (fstart) err_duplicateFunc(parser, func, fstart);
          fstart = func;
          fstart->used++;
        }
      }
      if (!fstart) { // TODO: new error, unless you want to get rid of start
        eputs("\n\e[31m*** error:\e[0m cannot find function "
              "\e[33mstart\e[0m.\n");
        parser->issues.errCount++;
      }
      fallthrough;
    case PMTest:
    case PMEmitC: {

      char* enginePath = "engine"; // FIXME
      char* cc_cmd = "gcc";
      const bool cocoaUI = true;

      foreach (Module*, mod, modules) {
        // for emitting C, test out_o for newness not out_c, since out_c
        // is updated by clangformat etc. anyway O is the goal.
        if (!forceBuildAll && !mod->modified
            && file_newer(mod->out_o, mod->filename))
          continue;
        mod_emit(mod);
        modifiedMods++;
        eprintf("modified: %s %d\n", mod->filename, mod->modified);
      }
      parser->oelap = clock_clockSpanMicro(t0) / 1.0e3;

      Process proc;

      foreachn(Module*, mod, mods, modules) {
        // For monolithic builds, just compile the last module (root); it
        // will have pulled in the other source files by #including them.
        if (monolithicBuild && mods->next) continue;

        bool anyDependencyHModified = false;
        foreach (Import*, imp, mod->imports) {
          if (imp->mod && imp->used && imp->mod->hmodified)
            anyDependencyHModified = true;
          // public interface of some dep has changed, this mod will need t
          // be recompiled.
          break;
        }
        // Skip up-to-date object files
        if (!forceBuildAll && !anyDependencyHModified
            && file_newer(mod->out_o, mod->out_c))
          continue;

        // char* mono = monolithicBuild ? "-DJET_MONOBUILD" : "";
        char* cmd[] = { //
          cc_cmd, //
          "-g", "-O0", //
          "-I", enginePath, //
          monolithicBuild ? "-DJET_MONOBUILD" : "-DJET_INCRBUILD", //
          cocoaUI ? "-DGUI_COCOA" : "-DGUI_NONE", //
          "-x", cocoaUI ? "objective-c" : "c", //
          "-c", mod->out_c, //
          "-o", mod->out_o, //
          NULL
        };
        Process_launch(cmd);
      }
      // TODO: do something useful while cc is running
      // here count the actual number of valid pids
      do {
        proc = Process_awaitAny();
        if (proc.exited && proc.code) {
          unreachable("cc failed%s\n", "");
          return 1;
        }
        // here launch 1 more
      } while (proc.pid);

      if (parser->mode == PMRun) {
        char* cmd[] = { //
          cc_cmd, //
          "-I", enginePath, //
          "-x", cocoaUI ? "objective-c" : "c", //
          "-g", "-O0", //
          cocoaUI ? "-DGUI_COCOA" : "-DGUI_NONE", //
          "-c", // cocoaUI ? "engine/jet/rt0.m" :
          "engine/jet/rt0.c", //
          "-o", "engine/jet/rt0.o", //
          NULL
        };
        Process_launch(cmd);
        proc = Process_awaitAny();
        if (proc.exited && proc.code) {
          unreachable("rt0 failed%s\n", "");
          return 1;
        }

        if (modifiedMods) {
          PtrArray cmdexe = {};
          arr_push(&cmdexe, cmd[0]);
          arr_push(&cmdexe, "-g");
          foreachn(Module*, mod, mods, modules) {
            if (monolithicBuild && mods->next) continue;
            arr_push(&cmdexe, mod->out_o);
          }
          arr_push(&cmdexe, "engine/jet/rt0.o");
          if (cocoaUI) {
            arr_push(&cmdexe, "-framework");
            arr_push(&cmdexe, "Cocoa");
          }
          arr_push(&cmdexe, NULL);
          Process_launch((char**)cmdexe.ref);
          proc = Process_awaitAny();
          if (proc.exited && proc.code) {
            unreachable("ld failed%s\n", "");
            return 1;
          }
        }
        // Process_awaitAll();

        // Process_launch((char*[]) { "./a.out", NULL });
        // Process_awaitAll();

      } else if (mode == PMTest) {
        // see if this code can be shared with PMRun code

        char* cmd[] = { //
          cc_cmd, //
          "-I", enginePath, //
          "-g", "-O0", //
          cstr_interp_s(256, "-DTENTRY=%s", root->cname), "-c",
          "engine/jet/test0.c", //
          "-o", "engine/jet/test0.o", //
          NULL
        };
        Process_launch(cmd);
        proc = Process_awaitAny();
        if (proc.exited && proc.code) {
          unreachable("test0 failed%s\n", "");
          return 1;
        }
        if (modifiedMods) {
          PtrArray cmdexe = {};
          arr_push(&cmdexe, cc_cmd);
          arr_push(&cmdexe, "-g");
          foreachn(Module*, mod, mods, modules) {
            if (monolithicBuild && mods->next) continue;
            arr_push(&cmdexe, mod->out_o);
          }
          arr_push(&cmdexe, "engine/jet/test0.o");
          arr_push(&cmdexe, NULL);
          Process_launch((char**)cmdexe.ref);
          proc = Process_awaitAny();
          if (proc.exited && proc.code) {
            unreachable("ld failed%s\n", "");
            return 1;
          }
        }

        // Process_awaitAll();
      }
    } break;

      // case PMTest: {
      //   printf("#include \"jet/test0.h\"\n");
      //   // TODO : THISFILE must be defined since function callsites need
      //   // it, but the other stuff in par_emit_open isn't required.
      //   // Besides, THISFILE should be the actual module's file not the
      //   // test file

      //   foreach (Module*, mod, modules) { mod_genTests(mod); }
      // } break;

    default: break;
    }
  }
  parser->elap_tot = clock_clockSpanMicro(t0) / 1.0e3;
  if (stats) printstats(parser);
  if (parser->issues.warnCount)
    eprintf("\e[33m*** warnings: %d\e[0m\n", parser->issues.warnCount);
  if (parser->issues.errCount)
    eprintf("\e[31m*** errors: %d\e[0m\n", parser->issues.errCount);
  if (_InternalErrs)
    eputs("\e[31m*** an internal error has ocurred.\e[0m\n");

  int ret = parser->issues.errCount | _InternalErrs;

  eprintf("\e[90m[ p/c %.2f + e %.1f + cc %.0f ms ] %.0f ms\e[0m\n",
      parser->elap, parser->oelap - parser->elap,
      parser->elap_tot - parser->oelap, parser->elap_tot);
  eprintf("\e[90m%.*s\e[0m\n", 72, _dashes_);
  if (!ret && (mode == PMRun || mode == PMTest)) {
    // execv on macos has about 80-100ms overhead. why?
    // not just on first run of a.out
    // -- it is forst run, because you regenerate a.out every time
    execv("./a.out", (char*[]) { "./a.out", NULL });
    // Process_awaitAll();
  }

  return ret;
}
