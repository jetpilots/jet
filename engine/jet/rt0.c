#include "runtime.h"

monostatic void start(IFDEBUG(const char* callsite_));

monostatic thread_local const char* _err_ = NULL;
// monostatic const PtrArray _os_args;

int main(int argc, char* argv[]) {
  _stack_boundary = &argc - sys_stackSize() + 1024;
  // _os_args = (CStrArray) { .ref = argc, .used = argc };
  Ticks t0 = Ticks_get();

  srand(time(0));
  lineprofile_begin();

  start(IFDEBUG("\e[0mstart\n"));

  double dt = Ticks_elapsed(Ticks_get(), t0) / 1e9;
  if (_err_ == ERROR_TRACE) {
    printf("[%.3fs] Terminated due to an unhandled error.\n", dt);
    IFDEBUG(printf("(run in debug mode to see a backtrace)\n"));
  } else if (_err_ == NULL) {
    ; //   printf("[%.3fs] Completed successfully.\n", dt);
  }
  coverage_report();
  lineprofile_report();
  return !!_err_;
}
