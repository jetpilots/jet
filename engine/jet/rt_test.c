#ifndef JET_MONOBUILD
#include <stdio.h>
// #include "jet/os/clock.h"
#endif

#include "runtest.h"
#include "_rt/globals.h"

#ifndef eprintf
#define eprintf(s, ...) fprintf(stderr, s, __VA_ARGS__)
#endif

#ifdef GUI_COCOA
#include "ui/ui_cocoa.h"
#endif

#ifndef JET_ENTRY
#error specify the name of the entry function with -DJET_ENTRY=(name)
#endif

void (*const _jet_entry_test_)(int runDeps) = JET_ENTRY;
// int jet_quicktest = 0;

int main(int argc, char* argv[]) {

  jet_quicktest = argc > 1 && argv[1][0] == 'q';

  if (isatty(STDERR_FILENO)) {
    s_err = "\e[31m✘\e[0m";
    s_crash = "\e[31m✽\e[0m";
    s_skp = "\e[33m⁃\e[0m";
    s_ok = "\e[32m✔︎\e[0m";
  }

  int runDeps = !(argc > 1 && argv[1][0] == '-');
  int ret = 0;

  clock_Time t0 = clock_getTime();
  if (jet_quicktest) {
    pid_t pid = fork();
    if (pid) {
      int t;
      pid_t w = waitpid(pid, &t, 0);
      if (WIFSIGNALED(t)) {
        eprintf("%s", strsignal(WTERMSIG(t)));
        ret = WTERMSIG(t);
      } else if (WIFSTOPPED(t)) {
        eprintf("%s", strsignal(WSTOPSIG(t)));
        ret = WSTOPSIG(t);
      } else if (WEXITSTATUS(t)) {
        ret = WEXITSTATUS(t);
      }
      eputs(ret ? "some tests failed.\n" : "all tests passed.\n");
    } else {
      _jet_entry_test_(runDeps);
      return !!(_total - _pass);
    }
  } else {
    _jet_entry_test_(runDeps);
  }
  double elap = clock_clockSpanMicro(t0) / 1e3;

  char* units = "ms";
  if (elap > 1000.0) {
    elap /= 1000;
    units = "s";
    if (elap > 60.0) {
      elap /= 60.0;
      units = "min";
      if (elap > 60.0) {
        elap /= 60.0;
        units = "hr";
      }
    }
  }
  if (jet_quicktest) return ret;

  eprintf("%.*s\n", 66, _dashes_);
  eprintf("   Total |  %s Passed |  %s Failed | %s Skipped | Stopped | "
          "%s Crash\n",
    s_ok, s_err, s_skp, s_crash);

#define PCT *100.0 / _total

  eprintf(" %7d | %9d | %9d | %9d | %7d | %7d\n", _total, _pass, _fail,
    _skip, _stop, _crash);
  eprintf("         | %8.0f%% | %8.0f%% | %8.0f%% | %6.0f%% | %6.0f%% \n",
    _pass PCT, _fail PCT, _skip PCT, _stop PCT, _crash PCT);
  eprintf("%.*s\n", 66, _dashes_);
  eprintf("-> Time elapsed: %g [%s]\n", elap, units);

  return !!(_total - _pass);
}