#ifndef JET_MONOBUILD
#include <stdio.h>
// #include "_rt/errhandler.h"
#endif

#include "runtest.h"
#include "_rt/globals.h"

#ifdef GUI_COCOA
#include "ui/ui_cocoa.h"
#endif

#ifndef IFDEBUG
#ifdef NDEBUG
#define IFDEBUG(s)
#define IFDEBUGELSE(s, e) e
#else
#define IFDEBUG(s) s
#define IFDEBUGELSE(s, e) s
#endif
#endif

#ifndef JET_ENTRY
#error specify the name of the entry function with -DJET_ENTRY=(name)
#endif

void (*const _jet_entry_run_)(IFDEBUGELSE(const char* callsite, void))
  = JET_ENTRY;

int main(int argc, char* argv[]) {
  _stack_boundary = (char*)&argc - sys_stackSize() + 8192;

#ifdef GUI_COCOA
  App_start(); // must be compiled as ObjC
#else
  _jet_entry_run_(IFDEBUG("start\n"));
#endif

  if (_err_ == ERROR_TRACE) {
    fputs("Terminated due to an unhandled error.\n", stderr);
#ifdef NDEBUG
    fputs(stderr, "(run in debug mode to see a backtrace)\n", stderr);
#endif
  }
  return !!_err_;
}
