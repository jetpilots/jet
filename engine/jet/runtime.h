
#include "jet/base.h"

// #include "jet/core/Dict.h"
#include "jet/os/Ticks.h"
#include "jet/os/sys.h"

#include "_rt/errhandler.h"
#include "_rt/check.h"
#include "_rt/stack.h"
#include "_rt/coverage.h"
#include "_rt/profiler.h"

// *** This should go in the generated .c file of each module!
#define DECL_COV_PROF(nlines)                                              \
  monostatic UInt64 _cov_[nlines];                                         \
  monostatic Ticks _lprof_last_, _lprof_tmp_, _lprof_[nlines];
