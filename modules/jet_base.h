#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef JET_MATHLIB_SLEEF
#include "sleef.h"
#else
#ifdef JET_MATHLIB_YEPPP
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
#define jet_likely(x) __builtin_expect(!!(x), 1)
#define jet_unlikely(x) __builtin_expect(!!(x), 0)
#else
#define jet_likely(x) (x)
#define jet_unlikely(x) (x)
#endif

#define and &&
#define or ||
#define not !

#define min(a, b) ((a) < (b)) ? (a) : (b)
#define max(a, b) ((a) > (b)) ? (a) : (b)
#define KB *1024UL
#define MB *1024 KB
#define GB *1024 GB

#define eprintf(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
#define eputs(str) fputs(str, stderr)

#define jet_countof(x) (sizeof(x) / sizeof(x[0]))

#ifndef STATIC
#define STATIC
#endif

#pragma mark - Heap allocation stuff

STATIC int jet_globals__callocCount = 0;
#define calloc(n, s)                                                           \
    calloc(n, s);                                                              \
    jet_globals__callocCount++;
STATIC int jet_globals__mallocCount = 0;
#define malloc(s)                                                              \
    malloc(s);                                                                 \
    jet_globals__mallocCount++;
STATIC int jet_globals__strdupCount = 0;
#define strdup(s)                                                              \
    strdup(s);                                                                 \
    jet_globals__strdupCount++;
STATIC int jet_globals__reallocCount = 0;
#define realloc(ptr, s)                                                        \
    realloc(ptr, s);                                                           \
    jet_globals__reallocCount++;
STATIC int jet_globals__strlenCount = 0;
#define strlen(s)                                                              \
    strlen(s);                                                                 \
    jet_globals__strlenCount++;

#pragma mark - Custom types
typedef char bool;
#define true 1
#define false 0
typedef int64_t Int;
// typedef double Number;
typedef double Real;
typedef char** CStrings;

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

// this is used to mark a hopeless failure to infer the type of something
typedef struct Error_Type {
} Error_Type;

#define SArray(T) T*

// works only for known-size static arrays
#define Array_make(A, sz) A

static int _jet_InternalErrs = 0;

#define jet_Array_get_Number(A, i) A[(i)-1]
#define unreachable(fmt, ...)                                                  \
    {                                                                          \
        eprintf("\n\e[31m*** COMPILER INTERNAL ERROR\e[0m at ./%s:%d\n"        \
                "    in %s\n"                                                  \
                "    unreachable location hit, quitting\n"                     \
                "    msg: " fmt "\n",                                          \
            __FILE__, __LINE__, __func__, __VA_ARGS__);                        \
        _jet_InternalErrs++;                                                   \
    }
//    exit(12);

#pragma mark - jet_Array

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
typedef const char* CString;
typedef float Real32;
typedef double Real64;

#define jet_Array(T) jet_Array_##T
#define jet_Array_free(T) jet_Array_free_##T
#define jet_Array_get(T) jet_Array_get_##T
#define jet_Array_growTo(T) jet_Array_growTo_##T
#define jet_Array_resize(T) jet_Array_resize_##T
#define jet_Array_concatCArray(T) jet_Array_concatCArray_##T
#define jet_Array_concatArray(T) jet_Array_concatArray_##T
#define jet_Array_grow(T) jet_Array_grow_##T
#define jet_Array_push(T) jet_Array_push_##T
#define jet_Array_justPush(T) jet_Array_justPush_##T
#define jet_Array_clear(T) jet_Array_clear_##T
#define jet_Array_initWithCArray(T) jet_Array_initWithCArray_##T
#define jet_Array_pop(T) jet_Array_pop_##T
#define jet_Array_top(T) jet_Array_top_##T
#define jet_Array_empty(T) jet_Array_empty_##T

// convenience for manual writing
#define jet_PtrArray jet_Array(Ptr)
// #define jet_PtrArray_free jet_Array_free(Ptr)
// #define jet_PtrArray_growTo jet_Array_growTo(Ptr)
// #define jet_PtrArray_concatCArray jet_Array_concatCArray(Ptr)
// #define jet_PtrArray_concatArray jet_Array_concatArray(Ptr)
// #define jet_PtrArray_grow jet_Array_grow(Ptr)
#define jet_PtrArray_push jet_Array_push(Ptr)
// #define jet_PtrArray_clear jet_Array_clear(Ptr)
// #define jet_PtrArray_initWithCArray jet_Array_initWithCArray(Ptr)
// #define jet_PtrArray_justPush jet_Array_justPush(Ptr)
#define jet_PtrArray_pop jet_Array_pop(Ptr)
#define jet_PtrArray_top jet_Array_top(Ptr)
#define jet_PtrArray_empty jet_Array_empty(Ptr)
#define jet_PtrArray_topAs(T, self) ((T)jet_PtrArray_top(self))

// #define jet_PtrArray_topAs(T, self) jet_Array_topAs(T, self)

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

// dont get smart and try to do jet_Array(jet_Array(jet_Array(whatever)))

#define for_to(i, n) for (int i = 0; i < (n); i++)

// Should be using adhoc to generate these.

// At some point, make these use some smart ass tricks to enable good
// perf even with -O0 since -O takes too long -- doesn't seem like
// "instant" typically 200-300 ms should be the upper limit for
// compilation time YES "COMPILATION" MEANS TO BINARY NOT TO C, that
// should be < 20ms for a typical project.
#define MAKE_Array(T)                                                          \
    typedef struct jet_Array(T) {                                              \
        T* ref;                                                                \
        UInt32 used;                                                           \
        UInt32 cap;                                                            \
    }                                                                          \
    jet_Array(T);                                                              \
    STATIC void jet_Array_free(T)(jet_Array(T) * self) {                       \
        if (self->cap) free(self->ref);                                        \
    }                                                                          \
    STATIC void jet_Array_growTo(T)(jet_Array(T) * self, UInt32 size) {        \
        self->cap = roundUp32(size);                                           \
        self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
        memset(                                                                \
            self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
    }                                                                          \
    STATIC void jet_Array_resize(T)(jet_Array(T) * self, UInt32 size) {        \
        if (size > self->cap) jet_Array_growTo(T)(self, size);                 \
        self->used = size;                                                     \
    }                                                                          \
    STATIC T jet_Array_get(T)(jet_Array(T) * self, UInt32 index) {             \
        return self->ref[index];                                               \
    }                                                                          \
    STATIC void jet_Array_concatCArray(T)(                                     \
        jet_Array(T) * self, T * cArray, int count) {                          \
        const UInt32 reqd = self->used + count;                                \
        if (reqd >= self->cap) jet_Array_growTo(T)(self, reqd);                \
        memcpy(self->ref + self->used, cArray, count * sizeof(T));             \
    }                                                                          \
    STATIC void jet_Array_concatArray(T)(                                      \
        jet_Array(T) * self, jet_Array(T) * other) {                           \
        jet_Array_concatCArray(T)(self, other->ref, other->used);              \
    }                                                                          \
    STATIC void jet_Array_clear(T)(jet_Array(T) * self) { self->used = 0; }    \
    STATIC void jet_Array_initWithCArray(T)(                                   \
        jet_Array(T) * self, T * cArray, int count) {                          \
        jet_Array_clear(T)(self);                                              \
        jet_Array_concatCArray(T)(self, cArray, count);                        \
    } /* maybe this can be merged with growTo */                               \
    STATIC void jet_Array_grow(T)(jet_Array(T) * self) {                       \
        self->cap = self->cap ? 2 * self->cap : 8;                             \
        self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
        memset(                                                                \
            self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
    }                                                                          \
    STATIC void jet_Array_justPush(T)(jet_Array(T) * self, T node) {           \
        self->ref[self->used++] = node; /* when you know that cap is enough */ \
    }                                                                          \
    STATIC void jet_Array_push(T)(jet_Array(T) * self, T node) {               \
        if (self->used >= self->cap) jet_Array_grow(T)(self);                  \
        jet_Array_justPush(T)(self, node);                                     \
    }                                                                          \
    STATIC T jet_Array_pop(T)(jet_Array(T) * self) {                           \
        assert(self->used > 0);                                                \
        return self->ref[--self->used];                                        \
    }                                                                          \
    STATIC T jet_Array_top(T)(jet_Array(T) * self) {                           \
        return self->used ? self->ref[self->used - 1] : 0;                     \
    }                                                                          \
    STATIC bool jet_Array_empty(T)(jet_Array(T) * self) {                      \
        return self->used == 0;                                                \
    }

MAKE_Array(Ptr);
MAKE_Array(UInt32);
MAKE_Array(Real64);
static const double __RRANDFMAX = 1.0 / RAND_MAX;
/// Return a random double. Note that srand() is called on program start with
/// the current time.
Real64 jet_randf() { return rand() * __RRANDFMAX; }

#include "vector.h"
#include "spvector.h"

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

// MAKE_Array(UInt32);
// MAKE_Array(uint64_t);
// MAKE_Array(int64_t);
// MAKE_Array(int32_t);
// MAKE_Array(Number);
// MAKE_Array(float);
// make array for strings etc later

// jet_Array_top(T) is only defined for value types to keep the number
// of instantiations (of the "template" jet_Array) down. So void* represents
// object ptrs of all types. Cast them when you need to deref or do ->
// etc. self is used to get a void* as a T (usually a SomeType*)

// TODO: StaticArray type with size and array, StaticArray2D/3Detc.
// since self is not templated, it's your job to send items of the
// right size, or face the music
// ASSUMING SIZE IS 8. THAT MEANS NO FLOAT OR UINT32, only sizeof(void*)
// #define jet_Array_concatCArray(T, jet_Array, arr, count) \
//     jet_Array_concat_cjet_Array_(jet_Array, arr, count * sizeof(T))
// TODO: the compiler should optimise away calls to concat if the
// original arrays can be used one after the other. e.g. concat two
// arrays then print it can be done by simply printing first then
// second, no memcpy involved.
// #define jet_Array_concatArray(T, s1, s2) \
//     jet_Array_concatjet_Array_(s1, s2, sizeof(T))
#pragma mark - Pool

typedef struct jet_Pool {
    void* ref;
    UInt32 cap, capTotal; // BYTES
    jet_Array(Ptr) ptrs;
    jet_Array(UInt32) caps;
    UInt32 used, usedTotal; // used BYTES, unlike in jet_Array!
} jet_Pool;

STATIC void* jet_Pool_alloc(jet_Pool* self, size_t reqd) {
    void* ans = NULL;
    // printf("asked for %zu B\n", reqd);

    // This is a pool for single objects, not arrays or large strings.
    // dont ask for a big fat chunk larger than 16KB (or up to 256KB
    // depending on how much is already there) all at one time.
    if (self->used + reqd > self->cap) {
        if (self->ref) {
            jet_Array_push(Ptr)(&self->ptrs, self->ref);
            jet_Array_push(UInt32)(&self->caps, self->cap);
        }
        self->cap
            = (self->cap ? (self->cap > 2 MB ? 2 MB : self->cap * 2) : 4 KB);
        self->capTotal += self->cap;
        self->ref = calloc(1, self->cap);
        assert(self->ref != NULL);
        self->used = 0;
    }
    ans = (void*)((uintptr_t)self->ref + self->used);
    self->used += reqd;
    self->usedTotal += reqd;
    return ans;
}

typedef union {
    UInt32 bits;
    struct {
        UInt32 id : 8, ptr : 24;
    };
} jet_SmallPtr;

// returns a "jet_SmallPtr"
STATIC jet_SmallPtr jet_Pool_allocs(jet_Pool* self, size_t reqd) {
    jet_SmallPtr ans = {};

    // This is a pool for single objects, not arrays or large strings.
    // dont ask for a big fat chunk larger than 16KB (or up to 256KB
    // depending on how much is already there) all at one time.
    if (self->used + reqd > self->cap) {
        if (self->ref) jet_Array_push(Ptr)(&self->ptrs, self->ref);
        self->cap = (self->cap > 64 KB ? 256 KB : 4 KB);
        self->capTotal += self->cap;
        self->ref = calloc(1, self->cap);
        assert(self->ref != NULL);
        self->used = 0;
    }
    ans.ptr = (self->used);
    ans.id = self->ptrs.used; // if 0, means current otherwise ptrs.ref[id-1]

    self->used += reqd;
    self->usedTotal += reqd;

    return ans;
}

STATIC void* jet_Pool_deref(jet_Pool* self, jet_SmallPtr sptr) {
    return sptr.id ? self->ptrs.ref[sptr.id - 1] + sptr.ptr
                   : self->ref + sptr.ptr;
}

STATIC void jet_Pool_free(jet_Pool* self) {
    // TODO: reset used here?
    if (self->cap) free(self->ref);
    for (int i = 0; i < self->ptrs.used; i++) free(self->ptrs.ref[i]);
}

// FIXME: this should go into jet_runtime.h
jet_Pool jet_gPool[1] = {};
jet_Pool jet_sPool[1] = {};

#define jet_new(T) (T##_allocTotal++, jet_Pool_alloc(jet_gPool, sizeof(T)));
// Use jet_newn for contiguous alloc of n objects of type T (for small n!)
#define jet_newn(T, n)                                                         \
    (T##_allocTotal += n, jet_Pool_alloc(jet_gPool, n * sizeof(T)));

// This macro should be invoked on each struct defined.
#define MKSTAT(T) static int T##_allocTotal = 0;

#define allocstat(T)                                                           \
    if (T##_allocTotal)                                                        \
        eprintf("*** %-24s %4ld B x %5d = %7ld B\n", #T, sizeof(T),            \
            T##_allocTotal, T##_allocTotal * sizeof(T));

#pragma mark - jet_PtrList

MKSTAT(jet_PtrList)

#define List(T) jet_PtrList
typedef struct jet_PtrList {
    void* item;
    struct jet_PtrList* next;
} jet_PtrList;

STATIC jet_PtrList* jet_PtrList_with(void* item) {
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    jet_PtrList* li = jet_new(jet_PtrList);
    li->item = item;
    return li;
}

STATIC jet_PtrList* jet_PtrList_withNext(void* item, void* next) {
    // TODO: how to get separate alloc counts of List_ASTType
    // List_ASTFunc etc.?
    jet_PtrList* li = jet_new(jet_PtrList);
    // printf("%d\n", jet_PtrList_allocTotal);
    li->item = item;
    li->next = next;
    return li;
}

STATIC int jet_PtrList_count(jet_PtrList* listPtr) {
    int i = 0;
    while (listPtr) {
        listPtr = listPtr->next;
        i++;
    }
    return i;
}

// returns the a ref to the last listitem so you can use that for
// repeated appends in O(1) and not O(N)
STATIC jet_PtrList** jet_PtrList_append(jet_PtrList** selfp, void* item) {
    if (*selfp == NULL) { // first append call
        *selfp = jet_PtrList_with(item);
        return selfp;
    } else {
        jet_PtrList* self = *selfp;
        while (self->next) self = self->next;
        self->next = jet_PtrList_with(item);
        return &(self->next);
    }
}

STATIC void jet_PtrList_shift(jet_PtrList** selfp, void* item) {
    *selfp = jet_PtrList_withNext(item, *selfp);
}

#define jet_foreach(T, var, listSrc) jet_foreachn(T, var, _listp_, listSrc)
#define jet_foreachn(T, var, listp, listSrc)                                   \
    for (jet_PtrList* listp = listSrc; listp; listp = NULL)                    \
        for (T var = (T)listp->item; listp and (var = (T)listp->item);         \
             listp = listp->next)
#endif

#pragma mark - String Functions

#include "strcasecmp.h"

#define str_endswith(str, lenstr, suffix, lensuffix)                           \
    !strncmp(str + lenstr - lensuffix, suffix, lensuffix)

#define str_startswith(str, prefix, lenprefix) !strncmp(str, prefix, lenprefix)

// DO NOT USE strdup,strndup,strcasecmp,strncasecmp: OK reimplemented
// strcasecmp.

#define jet_ispow2(num) (((num)-1) & (num))

// round n up to a multiple of a power-of-2 number b.
#define jet_roundmpow2(n, b) ((n + (b)-1) & -(intptr_t)(b))
#define jet_roundm8(n) jet_roundmpow2(n, 8)
#define jet_roundm16(n) jet_roundmpow2(n, 16)
#define jet_roundm32(n) jet_roundmpow2(n, 32)

// round a 32-bit number n upto the next power of 2.
#define jet_roundup32(x)                                                       \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

STATIC char* pstrndup(const char* str, size_t len) {
    char* ret = jet_Pool_alloc(jet_sPool, jet_roundm8(len + 1));
    memcpy(ret, str, len); // jet_sPool uses calloc, so no need to zero last
    return ret;
}

STATIC char* pstrdup(const char* str) {
    const size_t len = strlen(str);
    return pstrndup(str, len);
}

STATIC char* str_noext(char* str) {
    const size_t len = strlen(str);
    char* s = pstrndup(str, len);
    char* sc = s + len;
    while (sc > s and *sc != '.') sc--;
    if (sc >= s) *sc = '\0';
    return s;
}

STATIC char* str_base(char* str, char sep, size_t slen) {
    if (!slen)
        return str; // you should pass in the len. len 0 is actually
                    // valid since basename for 'mod' is 'mod' itself,
                    // and self would have caused a call to strlen
                    // below. so len 0 now means really just return what
                    // came in.
    char* s = str;
    char* sc = s + slen;
    while (sc > s and sc[-1] != sep) sc--;
    if (sc >= s) s = sc;
    return s;
}

STATIC char* str_dir(char* str) {
    const size_t len = strlen(str);
    char* s = pstrndup(str, len);
    char* sc = s + len;
    while (sc > s and *sc != '/') sc--;
    if (sc >= s) *sc = '\0';
    return s;
}

STATIC char* str_upper(char* str) {
    char* s = pstrdup(str);
    char* sc = s - 1;
    while (*++sc)
        if (*sc >= 'a' and *sc <= 'z') *sc -= 32;
    return s;
}

// in place
STATIC void str_tr_ip(
    char* str, const char oldc, const char newc, const size_t length) {
    char* sc = str - 1;
    char* end = length ? str + length : (char*)0xFFFFFFFFFFFFFFFF;
    while (*++sc && sc < end)
        if (*sc == oldc) *sc = newc;
}

STATIC char* str_tr(char* str, const char oldc, const char newc) {
    size_t len = strlen(str);
    char* s = pstrndup(str, len);
    str_tr_ip(s, oldc, newc, len);
    return s;
}

STATIC char* str_nthField(char* str, int len, char sep, int nth) {
    return NULL;
}

STATIC int str_countFields(char* str, int len, char sep) { return 0; }

// caller sends target as stack array or NULL
STATIC char** str_getAllOccurences(char* str, int len, char sep, int* count) {
    // result will be malloced & realloced
    return 0;
}

STATIC int str_getSomeOccurences(
    char* str, int len, char sep, char** result, int limit) {
    // result buf is expected from caller
    return 0;
}

// #include <stdio.h>
// #include <string.h>

#define MIN3(a, b, c)                                                          \
    ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

unsigned long leven(
    char* s1, char* s2, unsigned long s1len, unsigned long s2len) {
    unsigned long x, y, lastdiag, olddiag;
    // s1len = strlen(s1);
    // s2len = strlen(s2);
    assert(s1len < 64 && s2len < 64);
    unsigned long column[64]; // = malloc(s1len + 1);
    for (y = 1; y <= s1len; y++) column[y] = y;
    for (x = 1; x <= s2len; x++) {
        column[0] = x;
        for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
            olddiag = column[y];
            column[y] = MIN3(column[y] + 1, column[y - 1] + 1,
                lastdiag + (s1[y - 1] == s2[x - 1] ? 0 : 1));
            lastdiag = olddiag;
        }
    }
    return column[s1len];
}

// val should be evaluated every time since it could be a func with side
// effects e.g. random(). BUt if it is not, then it should be cached.
#define Slice2D_set1_IJ(arr, ri, rj, val)                                      \
    for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step)              \
        for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step)          \
    Array2D_setAt(arr, ri_, rj_, val)

#define Slice2D_set_IJ(arr, ri, rj, arr2, r2i, r2j)                            \
    for (uint32_t ri_ = ri.start; ri_ <= ri.stop; ri_ += ri.step)              \
        for (uint32_t rj_ = rj.start; ri_ <= rj.stop; ri_ += rj.step)          \
    Array2D_setAt(arr, ri_, rj_, Array2D_getAt(arr2, r2i_, r2j_))
