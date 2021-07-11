/* Jet | jetpilots.dev | github.com/jetpilots/jet | GPLv3 (see LICENSE) */

#ifndef HAVE_JET_BASE_H
#define HAVE_JET_BASE_H

#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <errno.h>

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

// FIXME: hack to get incremental compilation running: everything is static.
// so funcs are repeated & o files will be bloated. fix by separating
// interface/impl of engine code
#ifndef monostatic
#define monostatic static
#endif

// #ifndef __APPLE__
#include "_ext/strcasecmp.h"
// #endif

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

#if __GNUC__ >= 3
#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#define _CTOR_ __attribute__((malloc))
#else
#define likely(x) (x)
#define unlikely(x) (x)
#define _CTOR_
#endif

#ifndef thread_local
#if __TINYC__
#define thread_local
#elif __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#define thread_local _Thread_local
#elif defined _WIN32                                                       \
  && (defined _MSC_VER || defined __ICL || defined __DMC__                 \
    || defined __BORLANDC__)
#define thread_local __declspec(thread)
/* note that ICC (linux) and Clang are covered by __GNUC__ */
#elif defined __GNUC__ || defined __SUNPRO_C || defined __xlC__
#define thread_local __thread
#else
#error "Cannot define thread_local"
#endif
#endif

#define DROP(...)

#define KB *1024UL
#define MB *1024 KB
#define GB *1024 GB

#define eprintf(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#define eputs(str) fputs(str, stderr)

// *** Move this to the compiler header. But what about jet's own
// unreachable?
#define unreachable(fmt, ...)                                              \
  (eprintf("file:1:1-1: error: unreachable hit at ./%s:%d in %s: " fmt     \
           "\n",                                                           \
     __FILE__, __LINE__, __func__, __VA_ARGS__),                           \
    _InternalErrs++)

#define countof(x) (sizeof(x) / sizeof(x[0]))

#ifndef NDEBUG
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
#define MKSTAT(T) monostatic int _allocTotal_##T = 0;
#define allocstat(T)                                                       \
  if (1 || _allocTotal_##T)                                                \
    eprintf("*** %-24s %4ld B x %5d = %7ld B\n", #T, sizeof(T),            \
      _allocTotal_##T, _allocTotal_##T * sizeof(T));
#else
#define MKSTAT(T)
#define allocstat(T)
#endif

typedef char bool;
static const bool true = 1;
static const bool false = 0;
static const bool yes = 1;
static const bool no = 0;

typedef int64_t Int;
typedef unsigned long ulong;
typedef double Real;
typedef char** CStrings;

#define min(a, b) ((a) < (b)) ? (a) : (b)
#define max(a, b) ((a) > (b)) ? (a) : (b)
#define min3(a, b, c)                                                      \
  ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

monostatic ulong min3ul(ulong a, ulong b, ulong c) {
  return a < b ? (a < c ? a : c) : (b < c ? b : c);
}

#define fallthrough

#pragma mark - Variant

union Value {
  // think about returning larger things like Interval etc.
  char* s;
  int64_t i;
  uint64_t u;
  double d;
};

monostatic char* ask(void) {
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
// #define Array_make(A, sz) A

static int _InternalErrs = 0;

typedef void* VPtr;
typedef char* CString;
typedef const char* const_CString;
typedef uint32_t UInt32;
typedef uint64_t UInt64;
typedef int32_t Int32;
typedef int64_t Int64;
// typedef size_t Size;
// typedef const char* CString;
typedef float Real32;
typedef double Real64;
#define ctstrlen(s) (sizeof(s "") - 1)

// #define PtrArray_topAs(T, self) Array_topAs(T, self)
#define Range(T) IRange
typedef struct IRange {
  const Int32 start, end, step;
} IRange;
// typedef struct URange {
//   const UInt32 start, end, step;
// } URange;
#define range_to_by(s, e, d) (&(IRange) { s, e + d, d })
#define range_to(s, e) range_to_by(s, e, 1)
#define Range_for(T, v, a)                                                 \
  for (int __f = 0; !__f;)                                                 \
    for (IRange* const restrict __r = a; !__f; __f = 1)                    \
      for (Int32 v = __r->start, __i = 0;                                  \
           __i < (__r->end - __r->start) / __r->step;                      \
           v += __r->step, __i++)

// #define urange_to_by(s, e, d) (&(URange) { s, e + d, d })
// #define urange_to(s, e) urange_to_by(s, e, 1)
// #define URange_for(T, v, a) \
//   for (int __f = 0; !__f;) \
//     for (URange* const restrict __r = a; !__f; __f = 1) \
//       for (UInt32 v = __r->start; v != __r->end; v += __r->step)

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

#define for_to(i, n) for (UInt32 i = 0; i < (n); i++)
#define for_to_where(i, n, cond) for_to(i, n) if (cond)

#define TIMER                                                              \
  for (clock_Time t0 = clock_getTime(), t1 = 0; t1 == 0;                   \
       t1 = clock_clockSpanNano(t0),                                       \
                  printf("%s:%d: timer: %g ms\n", __FILE__, __LINE__,      \
                    t1 / 1e6))

// Should be using adhoc to generate these.

static const double __RRANDFMAX = 1.0 / RAND_MAX;
/// Return a random double. Note that srand() is called on program start
/// with the current time.
monostatic Real64 randf(void) { return rand() * __RRANDFMAX; }

// display first "digits" many digits of number plus unit (kilo-exabytes)
static int human_readable(char* buf, double num) {
  int unit = 0, len;
  while (num >= 1000) { num /= 1024, unit++; }
  if (unit && num < 100.0)
    len = snprintf(buf, 8, "%.3g", num);
  else
    len = snprintf(buf, 8, "%d", (int)num);

  if ((unit = "\0kMGTPEZY"[unit])) buf[len++] = unit;
  buf[len++] = 'B', buf[len] = 0;

  return len;
}

extern void jet_runTest(int (*f)(void), char* s, int skip);

#ifndef NDEBUG
#define HANDLE_UNCAUGHT eprintf("error: %s\n", _err_);
#else
// its the same for now, tweak it
#define HANDLE_UNCAUGHT eprintf("error: %s\n", _err_);
#endif

// ---------
//         // If something causes an error and the handler is not installed,
//         insert
//         // a goto to the catch-all error handler for the function,
//         labeled by
//         // uncaught:.
//         uncaught : // error:
//                    HANDLE_UNCAUGHT;
// // If something has set _err_=ERROR_TRACE it has initiated a backtrace
// and the
// // unwrap must continue, so insert a goto to backtrace:.
// backtrace : SHOW_BACKTRACE_LINE;
// // The function that initates a backtrace does not go to backtrace:, but
// // prints the initial site description, sets _err_=ERROR_TRACE, and
// starts
// // an unwind, going to unwind1:
// unwind : // one:
//          STACKDEPTH_DOWN;
// return DEFAULT_VALUE;
// }

/// This allows a single statement or subexpression to be included only in
/// debug mode.
#ifndef NDEBUG
#define IFDEBUG(...) __VA_ARGS__
#define IFDEBUGELSE(s, e) s
#else
#define IFDEBUG(s)
#define IFDEBUGELSE(s, e) e
#endif

// FIXME: String will be a proper String type whereas normal C strings
// are CString. For now ignoring
typedef double Number;
typedef const double const_Number;
typedef CStrings Strings;
#ifndef GUI_COCOA
typedef bool Boolean;
#endif
typedef const bool const_Boolean;
typedef unsigned char Byte;
#define STR(x) #x
monostatic void Number_drop_(Number f) { }

static const char* const _fp_bools_tf_[2] = { "false", "true" };
static const char* const _fp_bools_yn_[2] = { "no", "yes" };

#define CString_cmp(op, a, b) (strcmp(a, b) op 0)

#define Boolean_json_(x, _) printf("%s", _fp_bools_tf_[x])
#define Number_json_(x, _) printf("%g", x)
#define CString_json_(x, _) printf("\"%s\"", x) // should be escape(x)

#define Boolean_json(x) printf("\"%s\": %s\n", #x, _fp_bools_tf_[x])
#define Number_json(x) printf("\"%s\": %g\n", #x, x)
#define CString_json(x)                                                    \
  printf("\"%s\": \"%s\"\n", #x, x) // should be escape(x)

static const char* _spaces_ = //
  "                                                                    ";
static const char* _dashes_ = //
  "---------------------------------------------------------------------";

#define DECL_json_wrap_(T) static void T##_json_wrap_(const T this);

#define MAKE_JSON_WRAP_(T)                                                 \
  monostatic void T##_json_wrap_(const T self) {                           \
    T##_json_(self, 0);                                                    \
    puts("");                                                              \
  }

#define MAKE_JSON_FILE(T)                                                  \
  monostatic void T##_json_file(const T self, const char* file) {          \
    FILE* fd = fopen(file, "w");                                           \
    if (!fd) ERR;                                                          \
    T##_json(self, fd);                                                    \
    fclose(fd);                                                            \
  }

#define MAKE_cmp3way(T)                                                    \
  monostatic bool T##_cmp3way_LT_LT(T a, T b, T c) {                       \
    return a < b && b < c;                                                 \
  }                                                                        \
  monostatic bool T##_cmp3way_LT_LE(T a, T b, T c) {                       \
    return a < b && b <= c;                                                \
  }                                                                        \
  monostatic bool T##_cmp3way_LE_LT(T a, T b, T c) {                       \
    return a <= b && b < c;                                                \
  }                                                                        \
  monostatic bool T##_cmp3way_LE_LE(T a, T b, T c) {                       \
    return a <= b && b <= c;                                               \
  }                                                                        \
  monostatic bool T##_cmp3way_GT_GT(T a, T b, T c) {                       \
    return a > b && b > c;                                                 \
  }                                                                        \
  monostatic bool T##_cmp3way_GT_GE(T a, T b, T c) {                       \
    return a > b && b >= c;                                                \
  }                                                                        \
  monostatic bool T##_cmp3way_GE_GT(T a, T b, T c) {                       \
    return a >= b && b > c;                                                \
  }                                                                        \
  monostatic bool T##_cmp3way_GE_GE(T a, T b, T c) {                       \
    return a >= b && b >= c;                                               \
  }

MAKE_cmp3way(Number)

#include "jet/core/Array.h"
#include "jet/core/Pool.h"
#include "jet/core/CString.h"
#include "jet/core/Regex.h"
#include "jet/core/String.h"
#include "jet/core/Dict.h"
#include "jet/math/Vector.h"
#include "jet/math/SpVector.h"

#include "jet/core/List.h"

#include "jet/os/clock.h"
#include "jet/math/random.h"

#include "jet/core/isin.h"

// output funcs: print -> normal print, debug -> only prints (to stderr) in
// debug mode, error -> print to stderr, fatal -> print to stderr and exit
#define CString_equals !strcmp
#define print printf
#define String_print(x) fwrite(x.ref, x.len, 1, stdout)
#define CString_print puts
#define Boolean_print(x) printf("%s\n", _fp_bools_yn_[x])
#define Number_print(x) printf("%g\n", x)
#define CString_describe(x) printf("%s as String =\n    \"%s\"\n", #x, x)
#define Number_describe(x) printf("%s as Number =\n    %g\n", #x, x)
#define Boolean_describe(x)                                                \
  printf("%s as Boolean =\n    %s\n", #x, _fp_bools_yn_[x])

// FIXME: this should go into runtime.h

// DO NOT USE strdup,strndup,strcasecmp,strncasecmp: OK reimplemented
// strcasecmp.

// #include <stdio.h>
// #include "jet/CString.h"

// val should be evaluated every time since it could be a func with side
// effects e.g. random(). BUt if it is not, then it should be cached.
// #define Slice2D_set1_IJ(arr, ri, rj, val) \
//   for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step) \
//     for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step) \
//   Array2D_setAt(arr, ri_, rj_, val)

// #define Slice2D_set_IJ(arr, ri, rj, arr2, r2i, r2j)                        \
//   for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step)            \
//     for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step)          \
//   Array2D_setAt(arr, ri_, rj_, Array2D_getAt(arr2, r2i_, r2j_))
#endif
