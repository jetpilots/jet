// typedef union {
//     void* ptr; // Promise_(T) is just a T* with high bit set
//     unsigned long long _v;
//     //     struct {
//     // #ifndef BIG_ENDIAN
//     //         unsigned long long _ : 63, working : 1;
//     // #else
//     //         unsigned long long working : 1, _ : 63;
//     // #endif
//     //     };
// } Promise_;

// #ifndef BIG_ENDIAN
// #define HIGHBIT(v) (v >> 63)
// #else
// #define HIGHBIT(v) (v & 1UL)
// #endif

// void* Promise__resolve(Promise_ p) {
//     // the background thread holds a reference to the returned promise and
//     will
//     // unset the high bit when done. Further calls to resolve will not sleep.
//     while (HIGHBIT(p._v))
//         ;
//     // sleep();
//     return p.ptr;
// }

// #define Promise_resolve(T, p) ((T*)Promise__resolve(p))
// #define Promise(T) Promise_ // just so you can annotate promises
// typedef struct {
//     int vals[400];
// } MyObj;

#include <pthread.h>
#include <curl/curl.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

// Promise(String) async_httpget(char* url, int header, int debug) {
//     // put pthreads stuff here
//     String s = httpget(url, header, debug);
// }

// AUTOMATIC CODE MOTION IS IMPORTANT TO MAKE GOOD USE OF ASYNC/AWAIT!!!
// Don't rely on the user to pipeline things manually
// These should be ThreadPool_run and ThreadPool_join at some point, and at some
// point you  should use  runDetached instead of run. That can happen once
// functions switch over to having a return arg instead of returning stuff.
#define async(x, f, ...)                                                       \
    {                                                                          \
        if (!(_async_promise_##x = _async__run(f, __VA_ARGS__)))               \
            f(__VA_ARGS__);                                                    \
    }
#define _async__run(f, ...)                                                    \
    Thread_run(&_async_wrap_##f, (_async_args_##f[1]) { { __VA_ARGS__ } })
// if func takes no args, pass NULL for the ...
// ^if Thread_run returns NULL meaning pthread_create failed, f(a) is
// immediately eval'd and set into x. if not, x is 0 for now (works with
// doubles/ints/ptrs) and await() will resolve it
#define await(x)                                                               \
    (_async_promise_##x                                                        \
            ? (Thread_join(_async_promise_##x), _async_promise_##x = NULL, x)  \
            : x)
// x##_pr is the (transparent) promise object (really just a pthread_t).
// actually async(..) sets _async_promise_##x and not (yet) x itself.
// await(x) joins the thread and waits on the first time, subsequent calls
// just return x. The compiler should still optimise away repeated await
// calls when possible. _async_promise_##x always goes on the caller's stack
// frame!

// if you want to invoke async again without await-ing the result, make sure to
// pthread_detach the old thread

// ^^ TODO**DONE: if pthread_create fails, the function should be invoked
// directly (blocking) and the result returned. so you need to generate a
// struct for the ***DONE
// ^^ TODO
// args and a wrapper for the func that takes the struct as arg.
// there must be a NEAT way to reduce async/await to a blocking op when
// thread_create fails.  yes see it above its right there
// You call async() on the async_wrapper of f, not on f
// directly because f may take multiple args.

typedef struct {
    char* url;
    int header;
    int debug;
    String* ret;
} _async_args_httpget;
void* _async_wrap_httpget(void* a) {
    _async_args_httpget* args = a;
    String* ret = httpget(args->url, args->header, args->debug, args->ret);
    return ret;
}

void* _noop(void* arg) { return NULL; }
