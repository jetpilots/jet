#ifndef FPLUS_RUNTIME_H
#define FPLUS_RUNTIME_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <unistd.h> /* sysconf(3) */

#include "jet_base.h"

#include "hash.h"

#ifdef WINDOWS
#include <windows.h>
#endif

size_t sys_pageSize()
{
#ifdef WINDOWS
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwPageSize;
#else
    return sysconf(_SC_PAGESIZE);
#endif
}

#include "cycle.h"

// FIXME
#define _Pool_alloc_(x, y) malloc(y)

#define GB *1024 MB
#define MB *1024 KB
#define KB *1024UL

#include <sys/resource.h>
static size_t sys_stackSize()
{
    struct rlimit limit;
    getrlimit(RLIMIT_STACK, &limit);
    return limit.rlim_cur; //, limit.rlim_max);
}

#ifndef NONULLCHECK
// this is only for idents, not for funcs etc.: those will get evaluated
// twice if you use DEREF. Have to find another way for them (e.g.
// a.pop().xyz ) luckily if we stay away from methods and chaining we should
// be fine. P is for pointer to something, S is string, N is number (default
// values)
#define DEREF1(x, y) (x ? x->y : NULL)
#define DEREF2(x, y, z) (x && x->y ? x->y->z : NULL)
#define DEREF3(x, y, z, a) (x && x->y && x->y->z ? x->y->z->a : NULL)
#define DEREF4(x, y, z, a, b)                                                  \
    (x && x->y && x->y->z && x->y->z->a ? x->y->z->a->b : NULL)

#else
// fast path, all guns blazing, no protection
#define DEREF1(x, y) x->y
#define DEREF2(x, y, z) x->y->z
#define DEREF3(x, y, z, a) x->y->z->a
#define DEREF4(x, y, z, a, b) x->y->z->a->b

#endif

#define PROP(var, propname, type) JOIN_(type, propname)(var)
#define SETPROP(var, propname, type) JOIN3_(type, set, propname)(var)
// a.x becomes AType_x(a),  a.x = y becomes AType_set_x(a, y)

#define _btLimit_ 10
#define ERROR_TRACE (char*)0xFFFFFFFFFFFFFFFF
#define DONE                                                                   \
    {                                                                          \
        _err_ = ERROR_TRACE;                                                   \
        goto return_;                                                          \
    }
#define BACKTRACE                                                              \
    {                                                                          \
        _err_ = ERROR_TRACE;                                                   \
        goto backtrace;                                                        \
    }
static size_t _scSize_; // size of stack
// static const char* _scStart_; // start of stack, set in main()
static size_t _scDepth_ = 0; // current depth, updated in each function
static size_t _scUsage_ = 0; // current depth, updated in each function
static size_t _scPrintAbove_ = 0; // used for truncating long backtraces

static UInt64 _cov_[NUMLINES] = {};
static ticks _lprof_last_, _lprof_tmp_;
static ticks _lprof_[NUMLINES] = {};

#ifndef NOSTACKCHECK
// define these unconditionally, they are needed for both debug and release
// for fast mode, NOSTACKCHECK is defined globally, so these will not be
// used.
#define STACKDEPTH_UP                                                          \
    {                                                                          \
        _scDepth_++;                                                           \
        _scUsage_ += MYSTACKUSAGE;                                             \
    }
#define STACKDEPTH_DOWN                                                        \
    {                                                                          \
        _scDepth_--;                                                           \
        _scUsage_ -= MYSTACKUSAGE;                                             \
    }
#else
#define STACKDEPTH_UP
#define STACKDEPTH_DOWN
#endif
// what is the point of a separate NOSTACKCHECK option if release mode
// doesn't track ANY info (stack depth, function name etc.) other than
// showing "stack overflow" instead of "segmentation fault".

#define CHECK_HELP_OPEN printf("Here's some help:\n");
#define CHECK_HELP_CLOSE                                                       \
    printf("\n");                                                              \
    printf("\e[90mBacktrace (innermost first):\n");                            \
    if (_scDepth_ > 2 * _btLimit_)                                             \
        printf("    limited to %d outer and %d inner entries.\n", _btLimit_,   \
            _btLimit_);                                                        \
    BACKTRACE;

#define CHECK_HELP_DISABLED                                                    \
    eputs("(run in debug mode to get more info)\n");                           \
    exit(1);

#ifdef DEBUG
#define FUNC_ENTRY_STACK_BLOWN                                                 \
    _scPrintAbove_ = _scDepth_ - _btLimit_;                                    \
    printf("\e[31mfatal: stack overflow at call depth %lu.\n    in %s\e[0m\n", \
        _scDepth_, sig_);                                                      \
    printf("\e[90mBacktrace (innermost first):\n");                            \
    if (_scDepth_ > 2 * _btLimit_)                                             \
        printf("    limited to %d outer and %d inner entries.\n", _btLimit_,   \
            _btLimit_);                                                        \
    printf("[%lu] \e[36m%s\n", _scDepth_, callsite_);
#else
#define FUNC_ENTRY_STACK_BLOWN                                                 \
    printf("\e[31mfatal: stack  overflow at call depth %lu.\e[0m\n", _scDepth_);
#endif

#ifndef NOSTACKCHECK
#define DO_STACK_CHECK                                                         \
    if (_scUsage_ >= _scSize_) {                                               \
        FUNC_ENTRY_STACK_BLOWN;                                                \
        DONE;                                                                  \
    }
#else
#define DO_STACK_CHECK
#endif

#ifdef DEBUG
#define SHOW_BACKTRACE_LINE                                                    \
    if (_scDepth_ <= _btLimit_ || _scDepth_ > _scPrintAbove_)                  \
        printf("\e[90m[%lu] \e[36m%s\n", _scDepth_, callsite_);                \
    else if (_scDepth_ == _scPrintAbove_)                                      \
        printf("\e[90m... truncated ...\e[0m\n");
#else
#define SHOW_BACKTRACE_LINE
#endif

#ifdef DEBUG
#define HANDLE_UNCAUGHT eprintf("error: %s\n", _err_);
#else
// its the same for now, tweak it
#define HANDLE_UNCAUGHT eprintf("error: %s\n", _err_);
#endif

// ---------
//         // If something causes an error and the handler is not installed,
//         insert
//         // a goto to the catch-all error handler for the function, labeled by
//         // uncaught:.
//         uncaught : // error:
//                    HANDLE_UNCAUGHT;
// // If something has set _err_=ERROR_TRACE it has initiated a backtrace and
// the
// // unwrap must continue, so insert a goto to backtrace:.
// backtrace : SHOW_BACKTRACE_LINE;
// // The function that initates a backtrace does not go to backtrace:, but
// // prints the initial site description, sets _err_=ERROR_TRACE, and starts
// // an unwind, going to unwind1:
// unwind : // one:
//          STACKDEPTH_DOWN;
// return DEFAULT_VALUE;
// }
// -----------
/// This allows a single statement or subexpression to be included only in debug
/// mode.
#ifdef DEBUG
#define IFDEBUG(s) s
#define IFDEBUGELSE(s, e) s
#else
#define IFDEBUG(s)
#define IFDEBUGELSE(s, e) e
#endif

// FIXME: String will be a proper String type whereas normal C strings
// are CString. For now ignoring
typedef double Number;
typedef CString String;
typedef CStrings Strings;
typedef bool Boolean;
typedef unsigned char Byte;
#define STR(x) #x

static const char* const _fp_bools_tf_[2] = { "false", "true" };
static const char* const _fp_bools_yn_[2] = { "no", "yes" };

// #define str_cmp_EQ(a, b) (not strcmp(a, b))
#define str_cmp(op, a, b) (strcmp(a, b) op 0)
// #define str_cmp_GE(a, b) (strcmp(a, b) >= 0)
// #define str_cmp_LE(a, b) (strcmp(a, b) <= 0)
// #define str_cmp_GT(a, b) (strcmp(a, b) > 0)
// #define str_cmp_LT(a, b) (strcmp(a, b) < 0)

#define Boolean_json_(x, _) printf("%s", _fp_bools_tf_[x])
#define Number_json_(x, _) printf("%g", x)
#define String_json_(x, _) printf("\"%s\"", x) // should be escape(x)

#define Boolean_json(x) printf("\"%s\": %s\n", #x, _fp_bools_tf_[x])
#define Number_json(x) printf("\"%s\": %g\n", #x, x)
#define String_json(x) printf("\"%s\": \"%s\"\n", #x, x) // should be escape(x)

static const char* _spaces_ = //
    "                                                                    ";

#define DECL_json_wrap_(T) static void T##_json_wrap_(const T this);

#define MAKE_json_wrap_(T)                                                     \
    static void T##_json_wrap_(const T this)                                   \
    {                                                                          \
        T##_json_(this, 0);                                                    \
        puts("");                                                              \
    }

#define MAKE_json_file(T)                                                      \
    static void T##_json_file(const T* const this, const char* file)           \
    {                                                                          \
        FILE* fd = fopen(file, "w");                                           \
        if (!fd) ERR;                                                          \
        T##_json(this, fd);                                                    \
        fclose(fd);                                                            \
    }

#define MAKE_cmp3way(T)                                                        \
    static bool T##_cmp3way_LT_LT(T a, T b, T c) { return a < b && b < c; }    \
    static bool T##_cmp3way_LT_LE(T a, T b, T c) { return a < b && b <= c; }   \
    static bool T##_cmp3way_LE_LT(T a, T b, T c) { return a <= b && b < c; }   \
    static bool T##_cmp3way_LE_LE(T a, T b, T c) { return a <= b && b <= c; }

MAKE_cmp3way(Number)

// #define DEFAULT_VALUE
// #define SArray(x) x[]

// output funcs: print -> normal print, debug -> only prints (to stderr) in
// debug mode, error -> print to stderr, fatal -> print to stderr and exit
#define print printf
#define String_print puts
#define Boolean_print(x) printf("%g\n", _fp_bools_yn_[x])
#define Number_print(x) printf("%g\n", x)
#define String_describe(x) printf("%s String =\n    \"%s\"\n", #x, x)
#define Number_describe(x) printf("%s Number =\n    %g\n", #x, x)
#define Boolean_describe(x)                                                    \
    printf("%s Boolean =\n    %s\n", #x, _fp_bools_yn_[x])

    static Number Strings_main(const Strings a
#ifdef DEBUG
        ,
        const char* callsite_
#endif
    );

static const char* _err_ = NULL;

static const char* _undersc72_ = "------------------------"
                                 "------------------------"
                                 "------------------------";

#define JET_COVERAGE_UP(l)                                                     \
    {                                                                          \
        _cov_[l - 1]++;                                                        \
    }
#define JET_PROFILE_LINE(l)                                                    \
    {                                                                          \
        _lprof_tmp_ = getticks();                                              \
        _lprof_[l - 1] += (_lprof_tmp_ - _lprof_last_) / 100;                  \
        _lprof_last_ = _lprof_tmp_;                                            \
    }

// static void jet_coverage_report();
// static void jet_lineprofile_report();
// static void jet_lineprofile_begin();

static void jet_coverage_report()
{
    int count = 0, l = NUMLINES;
    while (--l > 0) count += !!_cov_[l];
    printf("coverage: %d/%d lines = %.2f%%\n", count, NUMLINES,
        count * 100.0 / NUMLINES);
}
static void jet_lineprofile_report()
{
    FILE* fd = fopen("." THISFILE "r", "w");
    ticks sum = 0;
    for (int i = 0; i < NUMLINES; i++) sum += _lprof_[i];
    for (int i = 0; i < NUMLINES; i++) {
        double pct = _lprof_[i] * 100.0 / sum;
        if (pct > 1.0)
            fprintf(fd, " %8.1f%% |\n", pct);
        else if (pct == 0.0)
            fprintf(fd, "           |\n");
        else
            fprintf(fd, "         ~ |\n");
    }
    fclose(fd);
    system("paste -d ' ' ." THISFILE "r " THISFILE " > " THISFILE "r");
}

static void jet_lineprofile_begin() { _lprof_last_ = getticks(); }

int main(int argc, char* argv[])
{
    srand(time(0));
    ticks t0 = getticks();

    // _scStart_ = (char*)&argc;
    _scSize_ = sys_stackSize() - 1024; //- 8192;
    // the difference is because you don't know what the stack size of
    // the last called function is going to be. the stack overflows *when*
    // you call a function that needs more than the available space, not
    // AFTER the func has been entered into. But you check only after the
    // function has been entered into. So the stack check is effectively
    // in the *penultimate* function called before the stack blows.
    // Along these lines, the difference above should be the maximum func
    // stack size allowed, excess vars should go on the heap.
    // OR -----
    // just minimise use of stack and don't bother checking, esp. if
    // you can statically disallow unrestrained recursive or mutually
    // recursive funcs.
    jet_lineprofile_begin();
    Strings_main(NULL
#ifdef DEBUG
        ,
        "\e[0mmain\n"
#endif
    );

    double dt = elapsed(getticks(), t0) / 1e9;
    if (_err_ == ERROR_TRACE) {
        printf("[%.3fs] Terminated due to an unhandled error.\n", dt);
#ifndef DEBUG
        printf("(run in debug mode to see a backtrace)\n");
#endif
    } else if (_err_ == NULL) {
        ; //   printf("[%.3fs] Completed successfully.\n", dt);
    }
    jet_coverage_report();
    jet_lineprofile_report();
    return _err_ ? 1 : 0;
}

#endif // FPLUS_RUNTIME_H