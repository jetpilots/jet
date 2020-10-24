// #include <stdlib.h>
// #include <string.h>
// #include <assert.h>
// #include <stdio.h>
// #include <stdint.h>
#include "jet_base.h"
#include "hash.h"

// options:
// JET_MEM_DISABLED: disable everything (fallback to malloc/free)
// JET_MEM_NOMEMPOOL: don't use the pool (allocate all from heap)
// JET_MEM_NOHEAPTRACKER: don't track heap allocations

// tODO: this is a heap checker that reports whether what is allocated is
// freed. also need a mem profiler that will report what the mem usage is at
// any given location as a % of the total mem AT THE POINT IN TIME just
// after it is allocated.

static int EL = 0;
static const char* spc = "                ";
#ifdef TRACE
#define EP                                                                     \
    printf("%.*s%s -> %s:%d\n", 2 * EL++, spc, __func__, __FILE__, __LINE__);
#define EX EL--;
#else
#define EP
#define EX
#endif

MKSTAT(jet_mem__SizeClassInfo)

typedef struct jet_mem__SizeClassInfo {
    jet_PtrArray arr; // init with null
    // jet_PtrList* list;
    // int count;
} jet_mem__SizeClassInfo;
// size -> jet_mem__SizeClassInfo* map
static jet_Dict(UInt64, Ptr) jet_mem__sizeDict[1] = {};

#define STR(X) #X
#define SPOT(x, fn, fil, lin) fil ":" STR(lin) ": " x

// TODO: jet_mem_alloc should not be affected: disabling jet_heap_alloc merely
// makes jet_heap_alloc, jet_mem_heapfree identical to malloc,free.
// TODO: need 2 switches, one to turn off heap checker and one to just make
// everything fall back to malloc/free (no pool).

// API:
// jet_mem_alloc(str, size) -> str is usually var name. file/line/func are added
// jet_mem_dealloc(ptr, nelem) -> drop ptr with n elems (of size sizeof(*ptr))
// jet_mem_realloc(ptr, newsize) -> realloc the buf for ptr to a new size
// jet_mem_stats()
// jet_mem__alloc(size,rawstr) -> rawstr a string, file/line/func are not added
// jet_mem__dealloc(ptr, nbytes, rawstr) -> drop ptr with nbytes size

// #define JET_MEM_DISABLED
// #define JET_MEM_NOHEAPTRACKER

#ifdef JET_MEM_DISABLED // --------------------------------------------------

#define jet_mem_alloc(nam, sz) jet_mem__alloc(sz, "")
void* jet_mem__heapmalloc(UInt64 size, const char* desc) {
    return malloc(size);
}
#define jet_mem_heapfree free
void* jet_mem__alloc(UInt64 size, const char* desc) {
    return jet_mem__heapmalloc(size, "");
}
UInt64 jet_mem_stats(bool heap) { return 0; }
#define jet_mem__dealloc(ptr, size, desc) jet_mem_heapfree(ptr)
#define jet_mem_dealloc(ptr, size) free(ptr)

#else // JET_MEM_DISABLED ---------------------------------------------------//
      // TODO: the compiler will call jet_mem__alloc directly with a custom
      // built string.
#define jet_mem_alloc(nam, sz)                                                 \
    jet_mem__alloc(sz, SPOT(nam, __func__, __FILE__, __LINE__))

typedef struct jet_mem__PtrInfo {
    UInt64 size : 63, heap : 1;
    const char* desc;
} jet_mem__PtrInfo;
MAKE_DICT(Ptr, jet_mem__PtrInfo)
// void* -> jet_mem__PtrInfo map

// TODO: rename
typedef struct {
    UInt64 count, sum;
} jet_mem__Stat;

MAKE_DICT(CString, jet_mem__Stat)

// TODO: rename

// void* jet_mem__heapmalloc(UInt64 size, const char* desc)
// {
//     EP;
//     void* ret = malloc(size);
//     // if (ret) {
//     //     int stat[1];
//     //     jet_mem__PtrInfo inf = { //
//     //         .size = size,
//     //         .heap = true,
//     //         .desc = desc
//     //     };
//     //     UInt32 p = jet_Dict_put(Ptr, jet_mem__PtrInfo)(jet_mem__ptrDict,
//     ret, stat);
//     //     jet_Dict_val(jet_mem__ptrDict, p) = inf;
//     //     jet_mem__heapTotal += size;
//     // }
//     EX;
//     return ret;
// }

#define jet_mem_dealloc(x, n)                                                  \
    jet_mem__dealloc(                                                          \
        x, (n) * sizeof(*(x)), SPOT(#x, __func__, __FILE__, __LINE__))
#define jet_mem_heapfree(x)                                                    \
    jet_mem__heapfree(x, SPOT(#x, __func__, __FILE__, __LINE__))

// just put into freelist. why need desc?
// f+ in module fp.mem
// var sizeDict[UInt64] as SizeClassInfo = {} -- can be modif within module
// function dealloc(ptr as Ptr, size as UInt64, desc as CCharPtr)
//    var inf as SizeClassInfo =
//        getOrSet(sizeDict, key = size, value = SizeClassInfo())
//    insert(&inf.list, item = ptr)
//    inf.count += 1
// end function

void* jet_mem__alloc(UInt64 size, const char* desc);
void jet_mem__dealloc(void* ptr, UInt64 size, const char* desc);
void jet_mem__heapfree(void* ptr, const char* desc);

// TODO: this func is not so great for strings, as it is "exact-fit". Freed
// pointers are reused only for objects of exactly the same size as the
// pointer's previous data. This is nearly useless for strings. They have their
// own string pool, you should probably have a jet_alloc_str that is best-fit.
// f+ fp.mem._alloc

static void* LIBC_MALLOC(size_t s) { return malloc(s); }
static void LIBC_FREE(void* p) { free(p); }
// TODO: fix free, add realloc/calloc
// right now the drop-in free can only be used when JET_MEM_NOMEMPOOL is set!
// otherwise the free call should pass in the size...
#define malloc(s) jet_mem_alloc("(name unknown)", s)
#ifdef JET_MEM_NOMEMPOOL
#define free(ptr) jet_mem_heapfree(ptr)
#endif
#endif // JET_MEM_DISABLED --------------------------------------------------
