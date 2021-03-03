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

typedef struct {
    char* ref;
    int len, cap;
} String;
void String_print(const String* const str) {
    // printf("---[%d]\n%.*s\n---\n", str->len, str->len, str->ref);
    printf("%.*s", str->len, str->ref);
}

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

void String_resize(String* str, int size) {
    printf("resize %p [%d] -> ", str->ref, str->cap);
    str->cap = size;
    str->ref = realloc(str->ref, str->cap);
    printf("%p [%d]\n", str->ref, str->cap);
}

void String_growTo(String* str, int size) {
    if (size > str->cap) String_resize(str, roundUp32(size));
}

void String_growBy(String* str, int size) {
    String_growTo(str, str->len + size);
}

void String_appendCString(String* str, char* data, int size) {
    String_growBy(str, size);
    memcpy(str->ref + str->len, data, size);
    str->len += size;
}

void String_append(String* str, String* str2) {
    String_appendCString(str, str2->ref, str2->len);
}

int String_len(String* str) { return str->len; }

void String_justPush(String* str, char ch) { str->ref[str->len++] = ch; }

void String_push(String* str, char ch) {
    String_growBy(str, 1);
    String_justPush(str, ch);
}

typedef enum { TextEncoding_ascii, TextEncoding_utf8 } TextEncoding;
String* String_iconv(String* str, TextEncoding from, TextEncoding to) { }

static int curldbgcbfn(
    CURL* handle, curl_infotype type, char* data, size_t size, void* userptr) {
    if (type == CURLINFO_HEADER_IN) String_appendCString(userptr, data, size);
    return 0;
}

static size_t curlcbfn(void* data, size_t size, size_t nmemb, void* tgt) {
    String_appendCString(tgt, data, nmemb * size);
    return nmemb * size;
}

static size_t curlcbhead(void* data, size_t size, size_t nmemb, void* target) {
    if (!strncmp("Content-Length: ", data, 16)) {
        // printf("*** found len: (%d) %s\n", atoi(data + 16), data + 16);
        String_growTo(target, atoi(data + 16));
        // this way you can get it in 1 alloc, well mostly. If you have headers
        // on they take some more space, so in practice you will need 1 more
        // allocation.

        // You could also do more fancy stuff like reading content-type and
        // doing something based on that. But let's leave that for the more
        // capable methods which take Request and return Response instead of the
        // simple async_httpget which just takes a url and returns a string.
    }
    return nmemb * size;
}

// I THINK ALL FUNCTIONS SHOULD BE PROMOTED TO TOPLEVEL EXPRS
// AND ALL RETURN TYPES SHOULD BE VOID
String* httpget(char* url, int header, int debug, String* ret) {
    CURL* curl_handle = curl_easy_init(); // find a way to reuse the handle
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);
    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    // curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, curlcbfn);
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, ret);
    curl_easy_setopt(curl_handle, CURLOPT_HEADER, !!header);
    curl_easy_setopt(curl_handle, CURLOPT_HEADERFUNCTION, curlcbhead);
    curl_easy_setopt(curl_handle, CURLOPT_HEADERDATA, ret);

    if (header) String_resize(ret, 512);
    curl_easy_perform(curl_handle);
    curl_easy_cleanup(curl_handle);
    return ret;
}

String* imaplist(char* url, char* user, char* pass, char* cmd, String* ret) {
    CURL* curl_handle = curl_easy_init(); // find a way to reuse the handle
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);
    // curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);

    // curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, curlcbfn);
    // curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, ret);

    curl_easy_setopt(curl_handle, CURLOPT_DEBUGFUNCTION, curldbgcbfn);
    curl_easy_setopt(curl_handle, CURLOPT_DEBUGDATA, ret);

    curl_easy_setopt(curl_handle, CURLOPT_USERNAME, user);
    curl_easy_setopt(curl_handle, CURLOPT_PASSWORD, pass);
    curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, cmd);

    // curl_easy_setopt(curl_handle, CURLOPT_HEADER, !!header);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERFUNCTION, curlcbhead);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERDATA, ret);

    // if (header) String_resize(ret, 512);
    curl_easy_perform(curl_handle);
    curl_easy_cleanup(curl_handle);
    return ret;
}

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

typedef enum {
    HTTPMethods_get,
    HTTPMethods_post,
    HTTPMethods_put,
    HTTPMethods_delete
} HTTPMethods;

typedef enum {
    MIMETypes_plain,
    MIMETypes_json,
    MIMETypes_xml,
    MIMETypes_yaml
} MIMETypes;

typedef struct {
    char* url;
    HTTPMethods method;
    TextEncoding dataEncoding;
    MIMETypes dataType;
    char* data;
} Request;

typedef struct {
    const char** headers;
    String body;
    uint16_t time, code;
    TextEncoding encoding : 8;
} Response;

httpdo(Request* req, Response* resp) { }

void* jet__noop(void* arg) { return NULL; }

#include "../modules/jet_clock.h"
void pthreadBench(int NTH) {
    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 128 * 1024);
    // pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    jet_clock_Time t0 = jet_clock_getTime();
    // about 20-50us on desktop
    for (int i = 0; i < NTH; i++) //
    {
        pthread_create(&tid, &attr, jet__noop, NULL);
        // pthread_join(tid, NULL);
    }
    // for low num of threads not joining is actually faster
    double tm = jet_clock_clockSpanNano(t0) / 1e3;
    printf("%8d thr %15.1f %15.1f us ", NTH, tm, tm / NTH);

    t0 = jet_clock_getTime();
    for (int i = 0; i < NTH; i++) //
    {
        pthread_create(&tid, &attr, jet__noop, NULL);
        pthread_join(tid, NULL);
    }
    // for low num of threads not joining is actually faster
    tm = jet_clock_clockSpanNano(t0) / 1e3;
    printf("%15.1f %15.1f us w/join\n", tm, tm / NTH);
}

static pthread_attr_t jet_pthread_attr_detached_default[1];
static pthread_attr_t jet_pthread_attr_joinable_default[1];

pthread_t Thread_run(void* (*f)(void*), void* arg) {
    pthread_t ret = NULL;
    int err = pthread_create(&ret, jet_pthread_attr_joinable_default, f, arg);
    if (err) printf("Thread_run failed\n");
    return ret;
}

void* Thread_join(pthread_t thr) {
    void* ret = NULL;
    pthread_join(thr, &ret);
    return ret;
}

pthread_t Thread_runDetached(void* (*f)(void*), void* arg) {
    pthread_t ret = NULL;
    int err = pthread_create(&ret, jet_pthread_attr_detached_default, f, arg);
    if (err) printf("Thread_run failed\n");
    return ret;
}

void jet_threads_init() {
    pthread_attr_init(jet_pthread_attr_detached_default);
    pthread_attr_init(jet_pthread_attr_joinable_default);
    pthread_attr_setdetachstate(
        jet_pthread_attr_detached_default, PTHREAD_CREATE_DETACHED);
    pthread_attr_setdetachstate(
        jet_pthread_attr_joinable_default, PTHREAD_CREATE_JOINABLE);
    pthread_attr_setstacksize(jet_pthread_attr_joinable_default, 512 * 1024);
    pthread_attr_setstacksize(jet_pthread_attr_detached_default, 512 * 1024);
}

#define TIMEIT(expr) TIMEIT_(expr, __FILE__, __LINE__)
#define TIMEIT_(expr, f, l)                                                    \
    {                                                                          \
        jet_clock_Time t0 = jet_clock_getTime();                               \
        expr;                                                                  \
        printf("%s:%d: elapsed: %.9g s\n", f, l,                               \
            jet_clock_clockSpanNano(t0) / 1e9);                                \
    }
// static double nums[10000];
#include <math.h>
// Thread safe random numbers by tempering the upper 32 bits
// of a 64 bit int.  The calculations are based on a seed.
//
// Create a single seed per thread and use that for every call
// lcg64_temper.
//
// credit: http://stackoverflow.com/a/19083740/2635342
static int32_t temper(int32_t x) {
    x ^= x >> 11;
    x ^= x << 7 & 0x9D2C5680;
    x ^= x << 15 & 0xEFC60000;
    x ^= x >> 18;
    return x;
}

int32_t lcg64_temper(uint64_t* seed) {
    *seed = 6364136223846793005ULL * *seed + 1;
    return temper(*seed >> 32);
}

int threadFunction(void* data) {
    int32_t id = *(int32_t*)data;
    printf("%d-th thread up\n", id);

    uint64_t threadSeed = time(NULL) ^ id;

    int32_t n = lcg64_temper(&threadSeed) % 20 + 1;
    // thrd_sleep(&(struct timespec) { .tv_sec = n },
    //    NULL); // Sleep for a random number of seconds.

    printf("%d-th thread done\n", id);
    return 0;
}

// *Really* minimal PCG32 code / (c) 2014 M.E. O'Neill / pcg-random.org
// Licensed under Apache License 2.0 (NO WARRANTY, etc. see website)

typedef struct {
    uint64_t state;
    uint64_t inc;
} pcg32_random_t;

uint32_t pcg32_random_r(pcg32_random_t* rng) {
    uint64_t oldstate = rng->state;
    // Advance internal state
    rng->state = oldstate * 6364136223846793005ULL + (rng->inc | 1);
    // Calculate output function (XSH RR), uses old state for max ILP
    uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
    uint32_t rot = oldstate >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

// Jet standard library random() functions (may be replaced by above)
uint64_t makerandstate(uint64_t id) {
    FILE* urandom = fopen("/dev/urandom", "r");
    setvbuf(urandom, NULL, _IONBF, 0); // turn off buffering
    // setup state buffer
    union {
        uint64_t u64;
        short s[4];
    } u;
    // actually it only needs 3 shorts.
    // unsigned short randstate[4] = {};
    // fgetc() returns a `char`, we need to fill a `short`
    u.s[0] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[1] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[2] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[3] = (fgetc(urandom) << 8) | fgetc(urandom);
    // cleanup urandom
    fclose(urandom);
    return u.u64 | id;
}
double frand(uint64_t* state) { return erand48((unsigned short*)state); }

void avgsqrt(int n, double* sum) {
    // FILE* urandom = fopen("/dev/urandom", "r");
    // setvbuf(urandom, NULL, _IONBF, 0); // turn off buffering
    // setup state buffer
    // unsigned short randstate[3] = {};
    uint64_t rst = makerandstate(pthread_self());
    // fgetc() returns a `char`, we need to fill a `short`
    // randstate[0] = (fgetc(urandom) << 8) | fgetc(urandom);
    // randstate[1] = (fgetc(urandom) << 8) | fgetc(urandom);
    // randstate[2] = (fgetc(urandom) << 8) | fgetc(urandom);
    // cleanup urandom
    // fclose(urandom);

    double d = 0;
    // *sum = 0;
    jet_clock_Time t0 = jet_clock_getTime();

    // it turns out rand() is not a good func to call within a thread, takes way
    // too long. erand46 is better, but you should seed it
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            d += sqrt(frand(&rst)); // sqrt(rand() / (1.0 * RAND_MAX)); //
    printf("thread %p: elapsed: %.9g s ", pthread_self(),
        jet_clock_clockSpanNano(t0) / 1e9);
    *sum = d / n;
    printf("wrote at %p: %f\n", sum, *sum);
}
typedef struct {
    int n;
    double* sum;
} _async_args_avgsqrt;
void* _async_wrap_avgsqrt(void* a) {
    _async_args_avgsqrt* args = a;
    // printf("dispatch %d items target %p\n", args->n, args->sum);

    // String* ret =
    avgsqrt(args->n, args->sum);
    // return ret;
    return NULL;
}

typedef struct {
    boolean_t imap4, imap4rev1, authPlain, authXOAuth2, saslIR, uidPlus, id,
        unselect, children, idle, namespace, literalPlus;
} IMAPServerCapabilities;

typedef struct {
    int count;
    char* first;
} StringString;

typedef struct {
    char* name;
    StringString flags, permanentFlags;
    int uidValidity;
    int uidNext;
    int messageCount;
    int recentCount;
} IMAPFolderInfo;

typedef struct {
    char *name, *email;
} IMAPUserAddress;

typedef struct {
    // make all these char* instead of stringstring
    StringString flags;
    char* subject;
    StringString unsubscribe;
    int id, uid;
    // DateTime date, internalDate;
    int size, rfc822Size;
    IMAPUserAddress from, to;
} IMAPMessageHeader;

IMAPMessageHeader* parseImapHeader(char* pos, int len);

void parseImapFetch(char* pos, int len) {
    char* end = pos + len;
    while (pos < end) {
        switch (*pos) {
        case ' ':
        case '\r':
        case '\n':
            break;
        case '*': {
            char* w1 = pos + 2;
            char* eo = strpbrk(w1, " \r\n");
            if (eo) {
                *eo++ = 0;
                pos = eo;
            }
            if (!strcmp(w1, "CAPABILITY")) {
            } else {
                char* w2 = pos;
                eo = strpbrk(w2, " \r\n");
                if (eo) {
                    *eo++ = 0;
                    pos = eo;
                }
                if (!strcmp(w2, "EXISTS")) {
                    int exi = atoi(w1);
                    printf("exi --> %d\n", exi);
                } else if (!strcmp(w2, "RECENT")) {
                    int recent = atoi(w1);
                    printf("recent --> %d\n", recent);
                } else if (!strcmp(w2, "[UNSEEN")) {
                    int unseen = atoi(pos);
                    printf("unseen --> %d\n", unseen);
                } else if (!strcmp(w2, "[UIDVALIDITY")) {
                    int uidvali = atoi(pos);
                    printf("uidvali --> %d\n", uidvali);
                } else if (!strcmp(w2, "[UIDNEXT")) {
                    int uidnext = atoi(pos);
                    printf("uidnext --> %d\n", uidnext);
                } else if (!strcmp(w2, "FETCH")) {
                    int idx = atoi(w1);
                    printf("fetch idx --> %d\n", idx);
                }
            }
        }
            continue;
        case '{': {
            char* num = ++pos;
            while (isdigit(*pos)) pos++;
            if (*pos != '}') perror("expected '}', not found");
            *pos++ = 0;
            int sz = atoi(num);
            printf("parse %d B --- \n", sz);
            IMAPMessageHeader* header = parseImapHeader(pos, sz);
            pos += sz;
        }
        case '(':
        case '[':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':;
        }
        pos++;
    }
}

char* imaptst
    = "* 4379 EXISTS\r\n"
      "* 0 RECENT\r\n"
      "* FLAGS (\\Seen \\Answered \\Flagged \\Deleted \\Draft $MDNSent)\r\n"
      "* OK [PERMANENTFLAGS (\\Seen \\Answered \\Flagged \\Deleted \\Draft "
      "$MDNSent)]Permanent flags\r\n"
      "* OK [UNSEEN 4379] Is the first unseen message\r\n"
      "* OK [UIDVALIDITY 14] UIDVALIDITY value\r\n"
      "* OK [UIDNEXT 24868] The next unique identifier value\r\n"
      "A003 OK [READ-WRITE] SELECT completed.\r\n"
      "* 119 FETCH (UID 7508 RFC822.SIZE 102161 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {451}\r\n"
      "Subject: "
      "=?utf-8?Q?Fantastic=20Weekend=20Deals=20at=20Domino=27s=20Pizza=C2=A0="
      "F0=9F=91=8C?=\r\n"
      "From: =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.ch>, "
      " =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.co.uk>\r\n"
      "Date: Sat, 3 Jun 2017 09:16:01 +0000\r\n"
      "List-Unsubscribe: "
      "<http://dominos.us12.list-manage1.com/"
      "unsubscribe?u=0a3531a6a054617a5478d1324&id=8dcc978ca9&e=895bfd4899&c="
      "016c232dbc>, "
      "<mailto:unsubscribe-mc.us12_0a3531a6a054617a5478d1324.016c232dbc-"
      "895bfd4899@mailin1.us2.mcsv.net?subject=unsubscribe>\r\n"
      "\r\n"
      ")\r\n"
      "* 120 FETCH (UID 7513 RFC822.SIZE 48069 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {127}\r\n"
      "From: <Notification@Jio.com>\r\n"
      "Subject: Data quota exhausted for Jio Number 8169236325\r\n"
      "Date: Sat, 3 Jun 2017 09:05:52 -0700\r\n"
      "\r\n"
      ")\r\n"
      "* 121 FETCH (UID 7535 RFC822.SIZE 64661 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {184}\r\n"
      "From: LC <mail@executive-learning.co.in>\r\n"
      "Subject: Letter of Credit Transactions International Trade UCP 600, "
      "ISBP 745 INCOTERMS  2010 & URBPO\r\n"
      "Date: Mon, 5 Jun 2017 06:08:55 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 122 FETCH (UID 7536 RFC822.SIZE 84459 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {143}\r\n"
      "From: Princeton Academy <mail@executive-learning.co.in>\r\n"
      "Subject: Workshop on Key Account Management\r\n"
      "Date: Mon, 05 Jun 2017 06:12:57 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 123 FETCH (UID 7547 RFC822.SIZE 50492 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {144}\r\n"
      "From: Jio Notifications <Notification@Jio.com>\r\n"
      "Subject: Recharge successful for Jio Number 8169236325\r\n"
      "Date: Tue, 6 Jun 2017 01:56:30 -0700\r\n"
      "\r\n"
      ")";

#include "bqdecode.c"

IMAPMessageHeader* parseImapHeader(char* pos, int len) {
    char* end = pos + len;
    IMAPMessageHeader* head = calloc(1, sizeof(IMAPMessageHeader));
    while (pos < end) {
        while (*pos == ' ' || *pos == '\r' || *pos == '\n') pos++;
        char* col = strpbrk(pos, ":\r\n");
        if (col && col[0] == ':' && col[1] == ' ') {
            // line is a (new) header field
            char* fld = pos;
            *col = 0;
            char* val = col + 2;
            col = strpbrk(val, "\r\n");
            while (col && col < end - 2
                && col[2] == ' ') // start with space means continuation
                col = strpbrk(col, "\r\n");
            if (!col)
                col = end;
            else
                *col = 0;

            if (!strcmp(fld, "Date")) {
                struct tm dttm = {};
                strptime(val, "%a, %d %b %Y %H:%M:%S", &dttm);
                char redate[128];
                strftime(redate, 128, "%a %d-%m-%Y %H:%M:%S ", &dttm);
                printf("  date --> %s\n", redate);
            } else if (!strcmp(fld, "From")) {
            } else if (!strcmp(fld, "Subject")) {
            } else if (!strcmp(fld, "List-Unsubscribe")) {
            }

            // TODO: now val may be multiline (softwrap) in which case unwrap it

            // TODO: ewdecode should also handle multiline or does it?
            if (*val == '=') ewdecode(val, col - val);

            printf("field: [%s = %s]\n", fld, val);

            pos = col + 1;

        } else {
            // continuation of an existing header, most likely
        }
    }
}

int main(int argc, char* argv[]) {
    // Promise(MyObj*) objp = asyncFunc(400);
    // MyObj* obj = Promise_resolve(MyObj*, objp);

    parseImapFetch(strdup(imaptst), strlen(imaptst));
    return 0;

    curl_global_init(CURL_GLOBAL_ALL);
    String simapresp[1] = {};
    imaplist("imaps://imap.outlook.com/Inbox", "sushpa@hotmail.com",
        "kojHiw-2sepfi-wiwgob",
        "fetch 1:* (uid flags body.peek[header.fields (subject from date "
        "list-unsubscribe)])",
        simapresp);
    // puts("\nOutput:");
    // String_print(simapresp);
    return 0;
    srand(time(0));
    jet_threads_init();
    int nnum = argc > 2 ? atoi(argv[2]) : 20000000;

    double sum;
    TIMEIT(avgsqrt(nnum, &sum))
    printf("%f\n", sum);

    static const int nth = 8;
    double sums[nth];
    pthread_t _async_promise_sums[nth] = {};
    void* _async_argholder_sums[nth] = {};
    // TIMEIT( //
    // for (int i = 0; i < nth; i++) {
    //     printf("%d %p\n", i, sums + i);
    // async(sums[i], avgsqrt, nnum, &sums[i]);
    // you cannot dispatch async items in a loop because async creates a
    // temporary literal object to hold the args. You should have an array
    // of those objects in the parent scope, of type void*, and each of them
    // should be init'd ON THE HEAP from within the async macro, at the
    // right index . Until then you can only have async vars by themselves,
    // not in an array (unless you want to unroll the dispatch loop fully
    // yourself like so) IF YOU FORBID making arrays of async vars you
    // should be fine. async var results[] as Number = .... NOOOOPE
    async(sums[0], avgsqrt, nnum, &sums[0]);
    async(sums[1], avgsqrt, nnum, &sums[1]);
    async(sums[2], avgsqrt, nnum, &sums[2]);
    async(sums[3], avgsqrt, nnum, &sums[3]);
    // async(sums[4], avgsqrt, nnum, &sums[4]);
    // async(sums[5], avgsqrt, nnum, &sums[5]);
    // async(sums[6], avgsqrt, nnum, &sums[6]);
    // async(sums[7], avgsqrt, nnum, &sums[7]);
    // }
    // for (int i = 0; i < nth; i++) printf("%p\n", _async_promise_sums[i]);
    // for (int i = 0; i < nth; i++) (void)await(sums[i]); //
    // )
    // for (int i = 0; i < nth; i++) printf("%p\n", _async_promise_sums[i]);
    for (int i = 0; i < nth; i++) printf("%d: %f\n", i, await(sums[i])); //
    pthread_exit(NULL);

    return 0;

    String s = {};
    httpget(argv[1], 0, 0, &s);

    // String_print(&s);

    // the args struct must be on the stack frame of the real caller (who
    // calls the async function) typedef struct {
    //     char* url;
    //     int header;
    //     int debug;
    // } async_args_httpget;
    // [1]
    //     = { { .url = url, .header = header, .debug = debug } };

    // String sp[1] = {};
    String* sp = (String[1]) {};
    pthread_t _async_promise_sp;
    async(sp, httpget, argv[1], 0, 0, sp);

    // ... other work ...

    String_print(await(sp));

    String_print(await(sp));

    // /* Must initialize libcurl before any threads are started */
    // if (!pthread_create(&tid, NULL, httpget, argv[1])) {

    //     pthread_join(tid, NULL);
    // } else {
    //     fprintf(stderr, "Couldn't run thread\n");
    // }

    // detached threads take about 7-10us overhead per thread regardless of
    // how many you are creating. joinables take about 10us/thr upto 1000
    // threads, 50us/thr at 10k threads without join, and 20-50us/thr upto
    // 1000 threads and 800us/thr!!! at 10K threads, with join. since
    // joinables are pointless if you aren't really joining them, the choice
    // is either detached (large no of threads) or joinable and actually
    // join.

    // for small no of threads: 10-100, 10 us/thr w/o join, 20 w/join, 9
    // detached. So it doesn't matter unless you have a large no of threads.
    // pthreadBench(argc > 2 ? atoi(argv[2]) : 100);
    curl_global_cleanup();
    pthread_exit(NULL);
    return 0;
}

// x :: T = f(y)
// await x: or Promise resolve: is pthread_join(prom, &x)
// async f: or Promise create: is pthread_create with the dispatcher for f
// with arg y the dispatcher writes its result (T*) in pthread_exit and it
// becomes avbl to pthread_join. Promise struct is a single pthread_t.

// Wrap a thread_t struct which can run in pthread mode or just normal async
// mode?