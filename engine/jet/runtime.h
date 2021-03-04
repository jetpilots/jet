
#include "jet/base.h"

#include "jet/core/Dict.h"
#include "jet/os/Ticks.h"

#ifdef WINDOWS
#include <windows.h>
#endif

size_t sys_pageSize() {
#ifdef WINDOWS
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwPageSize;
#else
    return sysconf(_SC_PAGESIZE);
#endif
}

#include <sys/resource.h>
static size_t sys_stackSize() {
    struct rlimit limit;
    getrlimit(RLIMIT_STACK, &limit);
    return limit.rlim_cur; //, limit.rlim_max);
}

// #include "jet/os/cycle.h"

// FIXME
#define _Pool_alloc_(x, y) malloc(y)

#define PROP(var, propname, type) JOIN_(type, propname)(var)
#define SETPROP(var, propname, type) JOIN3_(type, set, propname)(var)
// a.x becomes AType_x(a),  a.x = y becomes AType_set_x(a, y)

#include "_rt/errors.h"

// *** This should go in the generated .c file of each module!
static UInt64 _cov_[NUMLINES] = {};
static Ticks _lprof_last_, _lprof_tmp_;
static Ticks _lprof_[NUMLINES] = {};

#include "_rt/check.h"
#include "_rt/stack.h"

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

// #define CString_cmp_EQ(a, b) (not strcmp(a, b))
#define CString_cmp(op, a, b) (strcmp(a, b) op 0)
// #define CString_cmp_GE(a, b) (strcmp(a, b) >= 0)
// #define CString_cmp_LE(a, b) (strcmp(a, b) <= 0)
// #define CString_cmp_GT(a, b) (strcmp(a, b) > 0)
// #define CString_cmp_LT(a, b) (strcmp(a, b) < 0)

#define Boolean_json_(x, _) printf("%s", _fp_bools_tf_[x])
#define Number_json_(x, _) printf("%g", x)
#define CString_json_(x, _) printf("\"%s\"", x) // should be escape(x)

#define Boolean_json(x) printf("\"%s\": %s\n", #x, _fp_bools_tf_[x])
#define Number_json(x) printf("\"%s\": %g\n", #x, x)
#define CString_json(x) printf("\"%s\": \"%s\"\n", #x, x) // should be escape(x)

static const char* _spaces_ = //
    "                                                                    ";

#define DECL_json_wrap_(T) static void T##_json_wrap_(const T this);

#define MAKE_json_wrap_(T)                                                     \
    static void T##_json_wrap_(const T this) {                                 \
        T##_json_(this, 0);                                                    \
        puts("");                                                              \
    }

#define MAKE_json_file(T)                                                      \
    static void T##_json_file(const T* const this, const char* file) {         \
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
#define CString_equals !strcmp
#define print printf
#define String_print puts
#define CString_print puts
#define Boolean_print(x) printf("%g\n", _fp_bools_yn_[x])
#define Number_print(x) printf("%g\n", x)
#define CString_describe(x) printf("%s as String =\n    \"%s\"\n", #x, x)
#define Number_describe(x) printf("%s as Number =\n    %g\n", #x, x)
#define Boolean_describe(x)                                                    \
    printf("%s as Boolean =\n    %s\n", #x, _fp_bools_yn_[x])

    static void start(
#ifdef DEBUG
        const char* callsite_
#endif
    );

static const char* _err_ = NULL;

static const char* _undersc72_ = "------------------------"
                                 "------------------------"
                                 "------------------------";

// static void coverage_report();
// static void lineprofile_report();
// static void lineprofile_begin();

#include "_rt/coverage.h"
#include "_rt/profiler.h"

int main(int argc, char* argv[]) {
    srand(time(0));
    Ticks t0 = Ticks_get();

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
    lineprofile_begin();
    start(
#ifdef DEBUG
        "\e[0mmain\n"
#endif
    );

    double dt = Ticks_elapsed(Ticks_get(), t0) / 1e9;
    if (_err_ == ERROR_TRACE) {
        printf("[%.3fs] Terminated due to an unhandled error.\n", dt);
#ifndef DEBUG
        printf("(run in debug mode to see a backtrace)\n");
#endif
    } else if (_err_ == NULL) {
        ; //   printf("[%.3fs] Completed successfully.\n", dt);
    }
    coverage_report();
    lineprofile_report();
    return _err_ ? 1 : 0;
}
