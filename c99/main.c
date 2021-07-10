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
  PMLint,     // format on stdout, ALL errors on stderr
  PMEmitC,    // parse, stop on first error, or emit C
  // PMBuildO, // generate the object file
  PMMake, // run make and build the target
  PMRun,  // run the executable in debug mode (build it first)
  PMTest  // generate test code and run it
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

// static void par_emit_open(Parser* parser) {
//   // printf("#define THISFILE \"%s\"\n", parser->filename);
//   // printf("#define NUMLINES %d\n", parser->token.line);
// }

// static void par_emit_close(Parser* parser) {
//   // printf("#undef THISFILE\n");
// }

static void alloc_stat() { }

typedef struct {
  struct {
    bool cocoa, gtk3, winui;
  } use;
  struct {
    bool force, mono;
    char *opt, *dbg;
  } build;
  struct {
    char *engine, *cc;
  } path;
  CompilerMode mode;
  char* filename;
  bool stats, clean, help, vers, langserv, tccrun;
} Config;

bool dbglog = 0;
#define dprintf(...)                                                       \
  if (dbglog) eprintf(__VA_ARGS__)
#define dputs(...)                                                         \
  if (dbglog) eputs(__VA_ARGS__)

#define SWITCH(e)                                                          \
  for (char* __sw_c = e; __sw_c; __sw_c = NULL)                            \
    if (0) { }
#define CASE(s) else if (!strcmp(__sw_c, s))
// #define CASE2(s, l) else if (!strcmp(__sw_c, s) || !strcmp(__sw_c, l))
#define CASES(n, ...) else if (S_ISIN(n, __sw_c, __VA_ARGS__))
#define S_ICASE(s) else if (!strcasecmp(__sw_c, s))
#define DEFAULT else

static void argparse(Config* cfg, int argc, char* argv[]) {
  *cfg = (Config) {
    //
    .path.cc = "gcc",
    .path.engine = "engine",
    .mode = PMEmitC,
    .build.opt = "-O0",
    .build.dbg = "-g",
    .filename = "",
  };

  for (int i = 1; i < argc; i++) {
    SWITCH(argv[i]) //
    CASE("-C") cfg->path.cc = argv[++i];
    CASE("-E") cfg->path.engine = argv[++i];

    CASE("-ui") {
      // #if defined(JET_OS_MACOS)
      cfg->use.cocoa = 1;
      // #elif defined(JET_OS_LINUX)
      //       cfg->use.gtk3 = 1;
      // #endif
    }

    CASE("-m") cfg->build.mono = 1;
    CASE("-i") cfg->build.mono = 0;
    CASE("-f") cfg->build.force = 1;

    CASES(5, "-O0", "-O1", "-O2", "-O3", "-Os") cfg->build.opt = argv[i];
    // CASE("-g") cfg->build.dbg = argv[i];

    CASE("-l") cfg->mode = PMLint;
    CASE("-t") cfg->mode = PMTest;
    CASE("-c") cfg->mode = PMEmitC;
    CASE("-r") cfg->mode = PMRun;

    CASE("-") cfg->filename = argv[i];
    CASE("-ls") cfg->langserv = 1;

    CASE("-s") cfg->stats = 1;
    CASE("-d") dbglog = 1;
    CASE("-x") cfg->clean = 1;
    CASE("-h") cfg->help = 1;
    CASE("-v") cfg->vers = 1;

    CASE("-tc") {
      cfg->tccrun = 1;
      cfg->build.mono = 1;
    }

    DEFAULT {
      if (cstr_endsWith(argv[i], strlen(argv[i]), ".jet", 4)) {
        cfg->filename = argv[i];
      } else {
        eprintf("error: unknown option or argument '%s'\n", argv[i]);
        exit(2);
      }
    }
  }
}

static void sighandler(int sig, siginfo_t* si, void* unused) {
  write(2, _lastFunc, _lastFuncLen);
  write(2,
    ":1:1-1: error: internal error: this file caused a segmentation "
    "fault (unknown location)\n",
    88);
  _exit(1);
}

monostatic bool file_exists(const char* file) {
  struct stat sb = {};
  if (stat(file, &sb) != 0) return false;
  return !!sb.st_size;
}

monostatic bool file_newer(const char* file, const char* than) {
  static const unsigned long ONE_NANO = 1000000000;
  struct stat sb1 = {}, sb2 = {};
  int a = stat(file, &sb1), b = stat(than, &sb2);
  if (a | b) {
    // eprintf("&&& %d %s, %d %s\n", a, file, b, than);
    return false;
  }
  // ^ one or both file(s) not found or other error
  if (sb1.st_mtime > sb2.st_mtime) return true;
  size_t time1 = sb1.st_mtime * ONE_NANO + sb1.st_mtimensec;
  size_t time2 = sb2.st_mtime * ONE_NANO + sb2.st_mtimensec;
  // eprintf("%zu %s\n%zu %s\n", time1, file, time2, than);
  return time1 > time2;
}

void vers() {
  int maj = 1, min = 0, rev = 0;
  printf("jet v%d.%d.%d built %s %s\n", maj, min, rev, __DATE__, __TIME__);
}

void help() { printf("jet v built"); }

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

  Config cfg;
  argparse(&cfg, argc, argv);
  // CompilerMode mode = PMEmitC;
  // bool stats = false;
  // bool forceBuildAll = false;
  // bool monoBuild = true;

  // char* filename = (argc > 2) ? argv[2] : (argc > 1) ? argv[1] : NULL;

  if (cfg.vers) {
    vers();
    return 0;
  }
  if (cfg.help) {
    help();
    return 0;
  }
  if (cfg.clean) { return system("find . -name '.*.jet.*' -delete"); }

  // language server mode with no options to jetc
  // TODO: later this should be REPL mode, with "-s" argument for
  // language server.
  if (cfg.langserv) return langserver(argc, argv);

  if (!*cfg.filename) {
    eputs("cjet: no input file\n");
    return 1;
  }

  // if (argc > 2) {
  //   char m = argv[1][1]; // -cClLrRtT
  //   if (m == 'c' || m == 'C') mode = PMEmitC, stats = (m == 'C');
  //   if (m == 'l' || m == 'L') mode = PMLint, stats = (m == 'L');
  //   if (m == 't' || m == 'T') mode = PMTest, forceBuildAll = (m == 'T');
  //   if (m == 'r' || m == 'R') mode = PMRun, forceBuildAll = (m == 'R');
  // }

  clock_Time t0 = clock_getTime();

  Parser* parser = strcmp(cfg.filename, "-")
    ? par_fromFile(cfg.filename, true, cfg.mode)
    : par_fromStdin(true, cfg.mode);
  if (!parser) return 2;

  parser->issues.warnUnusedArg =    //
    parser->issues.warnUnusedFunc = //
    parser->issues.warnUnusedType = //
    parser->issues.warnUnusedVar =  //
    (cfg.mode != PMRun && cfg.mode != PMTest);

  List(Module)* modules = NULL;

  Module* root = parseModule(parser, &modules, NULL);
  parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
  int modifiedMods = 0;
  if (_InternalErrs) goto end;

  char* exeName = cstr_pclone(cstr_interp_s(512, "%s.%s.%s%s%s.%c",
    cstr_dir_ip(cstr_pclone(root->filename)),
    cstr_base(root->filename, '/', strlen(root->filename)), //
    cfg.build.opt + 2,                                      //
    cfg.build.mono ? "m" : "i",                             //
    cfg.build.dbg ? "g" : "",                               //
    cfg.mode == PMTest ? 't' : 'x'));

  foreach (Module*, mod, modules) {
    int l = strlen(mod->filename);
    static char buf[512];
    // TODO: improve this later
    mod->out_h = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.h",
      cstr_dir_ip(cstr_pclone(mod->filename)),
      cstr_base(mod->filename, '/', l)));
    int lh = strlen(mod->out_h);
    // root->out_hh = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.hh",
    //   cstr_dir_ip(cstr_pclone(root->filename)),
    //   cstr_base(root->filename, '/', l)));

    mod->out_hh = cstr_pclone(mod->out_h);
    mod->out_hh[lh - 1] = '_';

    mod->out_c = cstr_pclone(mod->out_h);
    mod->out_c[lh - 1] = 'c';

    mod->out_w = cstr_pclone(mod->out_h);
    mod->out_w[lh - 1] = 'w';

    mod->out_xc = cstr_pclone(mod->out_h);
    mod->out_xc[lh - 1] = 'u';

    // root->out_xc = cstr_pclone(__cstr_interp__s(512, buf, "%s.%s.0",
    //   cstr_dir_ip(cstr_clone(root->filename)),
    //   cstr_base(root->filename, '/', l)));

    // root->out_o = cstr_pclone(root->out_c);
    // root->out_o[strlen(root->out_o) - 1] = 'o';

    mod->out_o = cstr_pclone(__cstr_interp__s(
      512, buf, "%s.%s.%s%s%s.o", cstr_dir_ip(cstr_pclone(mod->filename)),
      cstr_base(mod->filename, '/', l), //
      cfg.build.opt + 2,                //
      cfg.build.mono ? "m" : "i",
      cfg.build.dbg ? "g" : "" //
      ));
  }

  if (parser->mode == PMLint) {
    if (parser->issues.hasParseErrors) {
      /* TODO: fallback to token-based linter (formatter)*/
    } else {
      foreach (Module*, mod, modules) { mod_write(mod); }
      foreach (Module*, mod, modules) { mod_dumpc(mod); }
    }
    parser->oelap = clock_clockSpanMicro(t0) / 1.0e3;
    goto end;
  }

  if (parser->issues.errCount) goto end;
  // ^ TODO: module should hold its err count, not parser. that way you can
  // still build the err-free mods

  if (parser->mode == PMRun) {
    Func* fstart = NULL;
    // don't break on the first match, keep looking so that duplicate
    // starts can be found. (even overloaded starts should be error)
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
  }

  // char* enginePath = "engine"; // FIXME
  // char* cc_cmd = "gcc";
  // const bool cocoaUI = true;

  foreach (Module*, mod, modules) {
    // for emitting C, test out_o for newness not out_c, since out_c
    // is updated by clangformat etc. anyway O is the goal.
    bool anyDependencyEmitted = false;
    if (cfg.build.mono) {
      // in mono mode you have to re-emit dependents because the .c will be
      // #include'd in the root mod.
      foreach (Import*, imp, mod->imports) {
        if (imp->mod /*&& imp->used*/ && imp->mod->emitted) {
          dprintf(
            ">>> dep %s of %s is emitted\n", imp->mod->name, mod->name);
          anyDependencyEmitted = true;
          break;
        }
      }
    }

    if (!cfg.build.force       //
      && !mod->modified        //
      && !anyDependencyEmitted //
      && file_newer(mod->out_c, mod->filename))
      continue;

    dprintf("emitting: %s (template-modified: %s)\n", mod->filename,
      _fp_bools_yn_[!!mod->modified]);
    mod_emit(mod);
    mod->emitted = 1;
    modifiedMods++;
  }
  mod_emit_mainwrapper(root);
  parser->oelap = clock_clockSpanMicro(t0) / 1.0e3;

  // Monolithic builds don't need the .o for each module.
  foreachn(Module*, mod, mods, modules) {
    if (cfg.build.mono && mods->next) continue;

    bool anyDependencyHModified = false;
    foreach (Import*, imp, mod->imports) {
      if (imp->mod && imp->used && imp->mod->hmodified) {
        dprintf(
          ">>> dep %s of %s is modified\n", imp->mod->name, mod->name);
        anyDependencyHModified = true;
        // public interface of some dep has changed, this mod will need
        // t be recompiled.
        break;
      }
    }
    // Skip up-to-date object files
    if (!cfg.build.force         //
      && !anyDependencyHModified //
      && file_newer(mod->out_o, mod->out_c))
      continue;

    dprintf("compiling: %s\n", mod->out_c);
    if (Process_exec(cfg.path.cc, cfg.build.dbg, cfg.build.opt,     //
          "-I", cfg.path.engine,                                    //
          "-D", cfg.build.mono ? "JET_MONOBUILD" : "JET_INCRBUILD", //
          "-D", cfg.use.cocoa ? "GUI_COCOA" : "GUI_NONE",           //
          "-x", cfg.use.cocoa ? "objective-c" : "c",                //
          "-c", mod->out_c, "-o", mod->out_o)) {
      unreachable("cc failed for: %s\n", mod->filename);
    }
  }

  if (_InternalErrs) goto end; // cc can fail

  if (parser->mode == PMRun || parser->mode == PMTest) {
    bool istest = parser->mode == PMTest;
    if (!file_newer( //
          istest ? "engine/jet/test0.o" : "engine/jet/rt0.o",
          istest ? "engine/jet/test0.c" : "engine/jet/rt0.c")
      || cfg.build.force) {
      if (Process_exec(cfg.path.cc, cfg.build.dbg, cfg.build.opt,     //
            "-I", cfg.path.engine,                                    //
            "-x", cfg.use.cocoa ? "objective-c" : "c",                //
            "-D", cfg.use.cocoa ? "GUI_COCOA" : "GUI_NONE",           //
            "-D", cstr_interp_s(256, "TENTRY=%s", root->cname),       //
            "-c", istest ? "engine/jet/test0.c" : "engine/jet/rt0.c", //
            "-o", istest ? "engine/jet/test0.o" : "engine/jet/rt0.o")) {
        unreachable("rt0 or test0 failed%s\n", "");
        goto end;
      }
    }
    if (modifiedMods || !file_exists(exeName)) {
      dprintf("linking: %s\n", exeName);
      PtrArray cmd = {};
      arr_push(&cmd, cfg.path.cc), arr_push(&cmd, cfg.build.dbg);
      foreachn(Module*, mod, mods, modules) {
        if (cfg.build.mono && mods->next) continue;
        arr_push(&cmd, mod->out_o);
      }
      arr_push(&cmd, istest ? "engine/jet/test0.o" : "engine/jet/rt0.o");
      if (cfg.use.cocoa) {
        arr_push(&cmd, "-framework"), arr_push(&cmd, "Cocoa");
      }
      arr_push(&cmd, "-o"), arr_push(&cmd, exeName);
      arr_push(&cmd, NULL);

      if (Process_execIn_((char**)cmd.ref, NULL)) {
        unreachable("ld failed%s\n", "");
        goto end;
      }
    } else {
      dprintf(">>> %s is up to date\n", exeName);
    }
  }

end:

  parser->elap_tot = clock_clockSpanMicro(t0) / 1.0e3;
  if (cfg.stats) printstats(parser);
  if (parser->issues.warnCount)
    eprintf("\e[33m*** warnings: %d\e[0m\n", parser->issues.warnCount);
  if (parser->issues.errCount)
    eprintf("\e[31m*** errors: %d\e[0m\n", parser->issues.errCount);
  if (_InternalErrs)
    eputs("\e[31m*** an internal error has ocurred.\e[0m\n");

  dprintf("p/c %.2f + e %.1f + cc %.0f ms ] %.0f ms\n",
    parser->elap, //
    parser->oelap - parser->elap,
    parser->elap_tot - parser->oelap, //
    parser->elap_tot);
  dprintf("%.*s\n", 72, _dashes_);

  if (parser->issues.errCount | _InternalErrs) return -1;

  if (parser->mode == PMRun || parser->mode == PMTest) {
    dprintf("launching: %s%s\n", cfg.tccrun ? "tcc -run " : "", exeName);
    if (cfg.tccrun) {
      execvp("tcc",
        (char*[]) {                                                    //
          "tcc", "-run", "-x", "c", "-I", cfg.path.engine,             //
          "-D", "JET_MONOBUILD",                                       //
          "-D", cfg.mode == PMTest ? "JET_MODE_TEST" : "JET_MODE_RUN", //
          root->out_w, NULL });
    } else {
      execv(exeName, (char*[]) { exeName, NULL });
    }
  }
  return 0;
}
