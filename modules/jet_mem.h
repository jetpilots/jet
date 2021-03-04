// #include <stdlib.h>
// #include <string.h>
// #include <assert.h>
// #include <stdio.h>
// #include <stdint.h>
#include "base.h"
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

MKSTAT(mem__SizeClassInfo)

typedef struct mem__SizeClassInfo {
    PtrArray arr; // init with null
    // PtrList* list;
    // int count;
} mem__SizeClassInfo;
// size -> mem__SizeClassInfo* map
static Dict(UInt64, Ptr) mem__sizeDict[1] = {};

#define STR(X) #X
#define SPOT(x, fn, fil, lin) fil ":" STR(lin) ": " x

// TODO: mem_alloc should not be affected: disabling heap_alloc merely
// makes heap_alloc, mem_heapfree identical to malloc,free.
// TODO: need 2 switches, one to turn off heap checker and one to just make
// everything fall back to malloc/free (no pool).

// API:
// mem_alloc(str, size) -> str is usually var name. file/line/func are added
// mem_dealloc(ptr, nelem) -> drop ptr with n elems (of size sizeof(*ptr))
// mem_realloc(ptr, newsize) -> realloc the buf for ptr to a new size
// mem_stats()
// mem__alloc(size,rawstr) -> rawstr a string, file/line/func are not added
// mem__dealloc(ptr, nbytes, rawstr) -> drop ptr with nbytes size

// #define JET_MEM_DISABLED
// #define JET_MEM_NOHEAPTRACKER

#ifdef JET_MEM_DISABLED // --------------------------------------------------

#define mem_alloc(nam, sz) mem__alloc(sz, "")
void* mem__heapmalloc(UInt64 size, const char* desc) { return malloc(size); }
#define mem_heapfree free
void* mem__alloc(UInt64 size, const char* desc) {
    return mem__heapmalloc(size, "");
}
UInt64 mem_stats(bool heap) { return 0; }
#define mem__dealloc(ptr, size, desc) mem_heapfree(ptr)
#define mem_dealloc(ptr, size) free(ptr)

#else // JET_MEM_DISABLED ---------------------------------------------------//
      // TODO: the compiler will call mem__alloc directly with a custom
      // built string.
#define mem_alloc(nam, sz)                                                     \
    mem__alloc(sz, SPOT(nam, __func__, __FILE__, __LINE__))

typedef struct mem__PtrInfo {
    UInt64 size : 63, heap : 1;
    const char* desc;
} mem__PtrInfo;
MAKE_DICT(Ptr, mem__PtrInfo)
// void* -> mem__PtrInfo map

// TODO: rename
typedef struct {
    UInt64 count, sum;
} mem__Stat;

MAKE_DICT(CString, mem__Stat)

// TODO: rename

// void* mem__heapmalloc(UInt64 size, const char* desc)
// {
//     EP;
//     void* ret = malloc(size);
//     // if (ret) {
//     //     int stat[1];
//     //     mem__PtrInfo inf = { //
//     //         .size = size,
//     //         .heap = true,
//     //         .desc = desc
//     //     };
//     //     UInt32 p = Dict_put(Ptr, mem__PtrInfo)(mem__ptrDict,
//     ret, stat);
//     //     Dict_val(mem__ptrDict, p) = inf;
//     //     mem__heapTotal += size;
//     // }
//     EX;
//     return ret;
// }

#define mem_dealloc(x, n)                                                      \
    mem__dealloc(x, (n) * sizeof(*(x)), SPOT(#x, __func__, __FILE__, __LINE__))
#define mem_heapfree(x) mem__heapfree(x, SPOT(#x, __func__, __FILE__, __LINE__))

// just put into freelist. why need desc?
// f+ in module fp.mem
// var sizeDict[UInt64] as SizeClassInfo = {} -- can be modif within module
// function dealloc(ptr as Ptr, size as UInt64, desc as CCharPtr)
//    var inf as SizeClassInfo =
//        getOrSet(sizeDict, key = size, value = SizeClassInfo())
//    insert(&inf.list, item = ptr)
//    inf.count += 1
// end function

void* mem__alloc(UInt64 size, const char* desc);
void mem__dealloc(void* ptr, UInt64 size, const char* desc);
void mem__heapfree(void* ptr, const char* desc);

// TODO: this func is not so great for strings, as it is "exact-fit". Freed
// pointers are reused only for objects of exactly the same size as the
// pointer's previous data. This is nearly useless for strings. They have their
// own string pool, you should probably have a alloc_str that is best-fit.
// f+ fp.mem._alloc

static void* LIBC_MALLOC(size_t s) { return malloc(s); }
static void LIBC_FREE(void* p) { free(p); }
// TODO: fix free, add realloc/calloc
// right now the drop-in free can only be used when JET_MEM_NOMEMPOOL is set!
// otherwise the free call should pass in the size...
#define malloc(s) mem_alloc("(name unknown)", s)
#ifdef JET_MEM_NOMEMPOOL
#define free(ptr) mem_heapfree(ptr)
#endif
#endif // JET_MEM_DISABLED --------------------------------------------------
