#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
// #include <signal.h>

// #ifndef HAVE_JET_BASE_H
// #include "jet/base.h"
// #endif
#define monostatic
#define thread_local _Thread_local
// #include "os/clock.h"
typedef uint64_t clock_Time;
typedef uint64_t PreciseTime;

PreciseTime clock_clockSpanMicro(clock_Time clockStart);
clock_Time clock_getTime();

// int test2() {
//   int x = 5 + 5;
//   abort();
//   sleep(2);
//   return !(x == 10);
// }

// int test1() {
//   int x = 43 / 2;
//   // int y = *(int*)0;
//   return !(x == 21);
// }

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
// static Dict(UInt32, Ptr) runDict;

monostatic thread_local const char* _err_ = NULL;

#include "runtest.h"

// #define jet_runTestS(s, skip) jet_runTest(s, "\"" #s "\"", skip)
// extern void jet_runModuleTests();

// void jet_runModuleTests() {
//   jet_runTestS(test1, 0);
//   jet_runTestS(test2, 0);
// }

#define J_(a, b) a##b
#define JETTEST_(s) J_(jet_runTests_, s)

extern void JETTEST_(TENTRY)();

int main(int argc, char* argv[]) {
  if (isatty(STDERR_FILENO)) {
    s_err = "[\e[31mERR\e[0m]";
    s_skp = "[\e[33mSKP\e[0m]";
    s_ok = "[\e[32mOK!\e[0m]";
  }

  clock_Time t0 = clock_getTime();
  JETTEST_(TENTRY)(); // use -DTENTRY=... on the cmdline
  double elap = clock_clockSpanMicro(t0) / 1e3;
  // eprintf("elapsed: %g ms\n", elap);
  char* units = "ms";
  if (elap > 1000.0) {
    elap /= 1000;
    units = "s";
  }
  if (elap > 60.0) {
    elap /= 60.0;
    units = "min";
  }
  if (elap > 60.0) {
    elap /= 60.0;
    units = "hr";
  }
  eprintf("\n   Total |  Passed |  Failed | Skipped | Stopped | Crashed | "
          "Elapsed [%s]\n",
      units);
  // eputs(
  //     "--------------------------------------------------------------------"
  //     "-------\n");
  // eputs(
  //     "--------------------------------------------------------------------"
  //     "-------\n");
#define PCT *100.0 / _total

  eprintf(" %7d | %7d | %7d | %7d | %7d | %7d | %11g\n", _total, _pass,
      _fail, _skip, _stop, _crash, elap);
  eprintf("         | %6.0f%% | %6.0f%% | %6.0f%% | %6.0f%% | %6.0f%% | \n",
      _pass PCT, _fail PCT, _skip PCT, _stop PCT, _crash PCT);

  // eputs(
  //     "--------------------------------------------------------------------"
  //     "-------\n");
}