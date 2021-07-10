#ifndef HAVE_JET_BASE_H
#include "runtime.h"
#endif
#include "runtest.h"
#include "_rt/globals.h"

#ifdef GUI_COCOA
#include "ui/ui_cocoa.h"
#endif

int main(int argc, char* argv[]) {
  _stack_boundary = (char*)&argc - sys_stackSize() + 8192;

#ifdef GUI_COCOA
  App_start(); // must be compiled as ObjC
#else
  _jet_entry_run_(IFDEBUG("start\n"));
#endif

  if (_err_ == ERROR_TRACE) {
    eputs("Terminated due to an unhandled error.\n");
    IFDEBUGELSE(;, eputs("(run in debug mode to see a backtrace)\n"));
  }
  return !!_err_;
}
