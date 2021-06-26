
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define monostatic
#define thread_local _Thread_local

typedef uint64_t clock_Time;
typedef uint64_t PreciseTime;
#ifndef NDEBUG
#define IFDEBUG(...) __VA_ARGS__
#define IFDEBUGELSE(s, e) s
#else
#define IFDEBUG(s)
#define IFDEBUGELSE(s, e) e
#endif

PreciseTime clock_clockSpanMicro(clock_Time clockStart);
clock_Time clock_getTime();
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#include "_rt/errhandler.h"
extern thread_local char* _stack_boundary;
extern size_t sys_stackSize();

extern void start(IFDEBUG(const char* callsite_));

monostatic thread_local const char* _err_ = NULL;
#include "runtest.h"

#ifdef GUI_COCOA
#include "ui/ui_cocoa.h"
#endif

int main(int argc, char* argv[]) {
  _stack_boundary = (char*)&argc - sys_stackSize() + 1024;

#ifdef GUI_COCOA
  App_start(); // must be compiled as ObjC
#else
  start(IFDEBUG("start\n"));
#endif

  if (_err_ == ERROR_TRACE) {
    printf("Terminated due to an unhandled error.\n");
    IFDEBUG(printf("(run in debug mode to see a backtrace)\n"));
  } else if (_err_ == NULL) {
    ;
  }
  return !!_err_;
}
