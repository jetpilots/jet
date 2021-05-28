// #include "runtime.h"
// #include "_rt/profiler.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
// #include "os/sys.h"

#define monostatic
#define thread_local _Thread_local
// #include "os/clock.h"
typedef uint64_t clock_Time;
typedef uint64_t PreciseTime;
#ifndef NDEBUG
#define IFDEBUG(s) s
#define IFDEBUGELSE(s, e) s
#else
#define IFDEBUG(s)
#define IFDEBUGELSE(s, e) e
#endif

PreciseTime clock_clockSpanMicro(clock_Time clockStart);
clock_Time clock_getTime();
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
// #include "_rt/stack.h"
#include "_rt/errhandler.h"
extern thread_local char* _stack_boundary;
extern size_t sys_stackSize();

extern void start(IFDEBUG(const char* callsite_));

monostatic thread_local const char* _err_ = NULL;
// monostatic const PtrArray _os_args;
#include "runtest.h"

int main(int argc, char* argv[]) {
  _stack_boundary = (char*)&argc - sys_stackSize() + 1024;
  // _os_args = (CStrArray) { .ref = argc, .used = argc };
  // Ticks t0 = Ticks_get();

  // srand(time(0));
  // lineprofile_begin();

  start(IFDEBUG("\e[0mstart\n"));

  // double dt = clock_;//Ticks_elapsed(Ticks_get(), t0) / 1e9;
  if (_err_ == ERROR_TRACE) {
    printf("Terminated due to an unhandled error.\n");
    IFDEBUG(printf("(run in debug mode to see a backtrace)\n"));
  } else if (_err_ == NULL) {
    ; //   printf("[%.3fs] Completed successfully.\n", dt);
  }
  // coverage_report();
  // lineprofile_report();
  return !!_err_;
}
