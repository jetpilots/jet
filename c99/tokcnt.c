#define _POSIX_C_SOURCE 200809L

#include "jet/base.h"
#include "signal.h"
#include "types.h"
#include "token.h"

typedef enum CompilerMode {
  PMTokenize, // just tokenize (debug only)
  PMLint, // format on stdout, ALL errors on stderr
  PMEmitC, // parse, stop on first error, or emit C
  // PMBuildO, // generate the object file
  PMMake, // run make and build the target
  PMRun, // run the executable in debug mode (build it first)
  PMTest // generate test code and run it
} CompilerMode;

#include "parser.h"
char* lastFile = "";
int lastFileL = 0;
static char* __lastfunc__ = "";
static int __lastfuncl__ = 0;

static void sighandler(int sig, siginfo_t* si, void* unused) {
  write(2, lastFile, lastFileL);
  write(2, "in function: ", 13);
  write(2, __lastfunc__, __lastfuncl__);
  write(2,
      ":1:1-1: error: internal error: this file caused a segmentation "
      "fault (unknown location)\n",
      88);
  _exit(1);
}

int main(int argc, char* argv[]) {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sighandler;
  sigaction(SIGSEGV, &sa, NULL);

  CompilerMode mode = PMLint;
  List(Module)* modules = NULL;
  Parser* parser;

  for (int i = 1; i < argc; i++) {
    lastFile = argv[i];
    lastFileL = strlen(lastFile);
    clock_Time t0 = clock_getTime();
    parser = par_fromFile(argv[i], true, mode);
    if (!parser) return 2;
    int ntok = 0;

    while (parser->token.kind != tkEOF) {
      tok_advance(&parser->token);
      ntok++;
    }
    parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
    printf("%7zu B, %5d L, %5d T, %6.2f N/f, %4.1fms, %s\n",
        parser->end - parser->data, parser->orig.used, ntok,
        ntok * 32.0 / (parser->end - parser->data), parser->elap,
        parser->filename);
    par_fini(parser);
  }
  return 0;
}
