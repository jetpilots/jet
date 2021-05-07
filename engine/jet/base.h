/* Jet | jetpilots.dev | github.com/jetpilots/jet | GPLv3 (see LICENSE) */

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

// #include "jet/include.h"

#ifdef SLEEF
#include "sleef.h"
#else
#ifdef YEPPP
#include "yeppp.h"
#else
#include <math.h>
#endif
#endif

#ifdef __SSE4_2__
#include <x86intrin.h>
#endif

#ifndef FPLUS_BASE_H
#define FPLUS_BASE_H

#if __GNUC__ >= 3
#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

#ifndef thread_local
#if __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#define thread_local _Thread_local
#elif defined _WIN32                                                       \
    && (defined _MSC_VER || defined __ICL || defined __DMC__               \
        || defined __BORLANDC__)
#define thread_local __declspec(thread)
/* note that ICC (linux) and Clang are covered by __GNUC__ */
#elif defined __GNUC__ || defined __SUNPRO_C || defined __xlC__
#define thread_local __thread
#else
#error "Cannot define thread_local"
#endif
#endif

// #define and &&
// #define or ||
// #define not !

#ifndef monostatic
#define monostatic
#endif

#define KB *1024UL
#define MB *1024 KB
#define GB *1024 GB

#define eprintf(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#define eputs(str) fputs(str, stderr)

// *** Move this to the compiler header. But what about jet's own
// unreachable?
#define unreachable(fmt, ...)                                              \
  (eprintf("file:1:1-1: error: \n\e[31m*** COMPILER INTERNAL ERROR\e[0m "  \
           "at ./%s:%d\n"                                                  \
           "    in %s\n"                                                   \
           "    unreachable location hit, quitting\n"                      \
           "    msg: " fmt ";;\n",                                         \
       __FILE__, __LINE__, __func__, __VA_ARGS__),                         \
      _InternalErrs++)

#define countof(x) (sizeof(x) / sizeof(x[0]))

#pragma mark - Heap allocation stuff

#ifndef RELEASE

monostatic size_t _called_calloc = 0;
monostatic size_t _called_malloc = 0;
monostatic size_t _called_realloc = 0;
monostatic size_t _called_strdup = 0;
monostatic size_t _called_strlen = 0;

#define malloc(s) (++_called_malloc, malloc(s))
#define calloc(n, s) (++_called_calloc, calloc(n, s))
#define realloc(ptr, s) (++_called_realloc, realloc(ptr, s))
#define strdup(s) (++_called_strdup, strdup(s))
#define strlen(s) (++_called_strlen, strlen(s))

// This macro should be invoked on each struct defined.
#define MKSTAT(T) static int T##_allocTotal = 0;
#define allocstat(T)                                                       \
  if (1 || T##_allocTotal)                                                 \
    eprintf("*** %-24s %4ld B x %5d = %7ld B\n", #T, sizeof(T),            \
        T##_allocTotal, T##_allocTotal * sizeof(T));

#else

#define MKSTAT(T)
#define allocstat(T)

#endif

#pragma mark - Custom types
typedef char bool;
static const bool true = 1;
static const bool false = 0;
static const bool yes = 1;
static const bool no = 0;

typedef int64_t Int;
typedef unsigned long ulong;
// typedef double Number;
typedef double Real;
typedef char** CStrings;

#define min(a, b) ((a) < (b)) ? (a) : (b)
#define max(a, b) ((a) > (b)) ? (a) : (b)
#define min3(a, b, c)                                                      \
  ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

monostatic ulong min3ul(ulong a, ulong b, ulong c) {
  return a < b ? (a < c ? a : c) : (b < c ? b : c);
}

// use self in switches to indicate explicit fallthrough
#define fallthrough

#pragma mark - Variant

union Value {
  // think about returning larger things like Interval etc.
  char* s;
  int64_t i;
  uint64_t u;
  double d;
};

char* ask() {
  size_t sz = 128;
  char* s = malloc(sz);
  char* buf = s;
  while (fread(buf, sz, 1, stdin) == sz) {
    sz += 128;
    s = realloc(s, sz);
    buf += 128;
  }
  s[sz - 1] = 0;
  return s;
}

// this is used to mark a hopeless failure to infer the type of something
typedef struct Error_Type {
} Error_Type;

#define SArray(T) T*

// works only for known-size static arrays
#define Array_make(A, sz) A

static int _InternalErrs = 0;

#define Array_get_Number(A, i) A[(i)-1]
//    exit(12);

#pragma mark - Array

//#define DEFAULT0(T) DEFAULT0_##T
//#define DEFAULT0_double 0.0
//#define DEFAULT0_float 0.0
//#define DEFAULT0_uint64_t 0
//#define DEFAULT0_uint32_t 0
//#define DEFAULT0_int32_t 0
//#define DEFAULT0_int64_t 0
//#define DEFAULT0_voidptr NULL

typedef void* Ptr;
typedef uint32_t UInt32;
typedef uint64_t UInt64;
typedef int32_t Int32;
typedef int64_t Int64;
// typedef size_t Size;
// typedef const char* CString;
typedef float Real32;
typedef double Real64;
#define ctstrlen(s) (sizeof(s) - 1)

// #define PtrArray_topAs(T, self) Array_topAs(T, self)

#define ispow2(num) (((num)-1) & (num))

// round n up to a multiple of a power-of-2 number b.
#define roundmpow2(n, b) ((n + (b)-1) & -(intptr_t)(b))
#define roundm8(n) roundmpow2(n, 8)
#define roundm16(n) roundmpow2(n, 16)
#define roundm32(n) roundmpow2(n, 32)

// round a 32-bit number n upto the next power of 2.
#define roundUp32(x)                                                       \
  (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,               \
      (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

// dont get smart and try to do Array(Array(Array(whatever)))

#define for_to(i, n) for (int i = 0; i < (n); i++)
#define for_to_where(i, n, cond) for_to(i, n) if (cond)

// Should be using adhoc to generate these.

static const double __RRANDFMAX = 1.0 / RAND_MAX;
/// Return a random double. Note that srand() is called on program start
/// with the current time.
Real64 randf() { return rand() * __RRANDFMAX; }

// display first "digits" many digits of number plus unit (kilo-exabytes)
static int human_readable(char* buf, double num) {
  //    size_t snap = 0;
  //    size_t orig = num;
  int unit = 0;
  while (num >= 1000) {
    num /= 1024;
    unit++;
  }
  int len;
  if (unit && num < 100.0)
    len = snprintf(buf, 8, "%.3g", num);
  else
    len = snprintf(buf, 8, "%d", (int)num);

  unit = "\0kMGTPEZY"[unit];

  if (unit) buf[len++] = unit;
  buf[len++] = 'B';
  buf[len] = 0;

  return len;
}

#include "jet/core/Array.h"
#include "jet/core/Pool.h"
#include "jet/core/CString.h"
#include "jet/core/String.h"
#include "jet/math/Vector.h"
#include "jet/math/SpVector.h"
MKSTAT(PtrList)
#include "jet/core/List.h"

#include "jet/os/clock.h"
#include "jet/math/random.h"

#include "jet/core/isin.h"

// FIXME: this should go into runtime.h

// DO NOT USE strdup,strndup,strcasecmp,strncasecmp: OK reimplemented
// strcasecmp.

// #include <stdio.h>
// #include "jet/CString.h"

// val should be evaluated every time since it could be a func with side
// effects e.g. random(). BUt if it is not, then it should be cached.
#define Slice2D_set1_IJ(arr, ri, rj, val)                                  \
  for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step)            \
    for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step)          \
  Array2D_setAt(arr, ri_, rj_, val)

#define Slice2D_set_IJ(arr, ri, rj, arr2, r2i, r2j)                        \
  for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step)            \
    for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step)          \
  Array2D_setAt(arr, ri_, rj_, Array2D_getAt(arr2, r2i_, r2j_))
#endif
