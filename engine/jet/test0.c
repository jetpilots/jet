#ifndef HAVE_JET_BASE_H
#include "runtime.h"
#endif
#include "runtest.h"
#include "_rt/globals.h"

#ifdef GUI_COCOA
#include "ui/ui_cocoa.h"
#endif

int main(int argc, char* argv[]) {
  if (isatty(STDERR_FILENO)) {
    s_err = "\e[31m✘\e[0m";
    s_crash = "\e[31m✽\e[0m";
    s_skp = "\e[33m⁃\e[0m";
    s_ok = "\e[32m✔︎\e[0m";
  }

  int runDeps = !(argc > 1 && argv[1][0] == '-');

  clock_Time t0 = clock_getTime();
  _jet_entry_test_(runDeps);
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