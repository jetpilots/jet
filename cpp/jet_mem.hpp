

#include "base.h"
#include "hash.h"

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
    PtrArray arr;

} mem__SizeClassInfo;

static Dict(UInt64, Ptr) mem__sizeDict[1] = {};

#define STR(X) #X
#define SPOT(x, fn, fil, lin) fil ":" STR(lin) ": " x

#ifdef JET_MEM_DISABLED

#define mem_alloc(nam, sz) mem__alloc(sz, "")
void* mem__heapmalloc(UInt64 size, const char* desc) { return malloc(size); }
#define mem_heapfree free
void* mem__alloc(UInt64 size, const char* desc) {
    return mem__heapmalloc(size, "");
}
UInt64 mem_stats(bool heap) { return 0; }
#define mem__dealloc(ptr, size, desc) mem_heapfree(ptr)
#define mem_dealloc(ptr, size) free(ptr)

#else

#define mem_alloc(nam, sz)                                                     \
    mem__alloc(sz, SPOT(nam, __func__, __FILE__, __LINE__))

typedef struct mem__PtrInfo {
    UInt64 size : 63, heap : 1;
    const char* desc;
} mem__PtrInfo;
MAKE_DICT(Ptr, mem__PtrInfo)

typedef struct {
    UInt64 count, sum;
} mem__Stat;

MAKE_DICT(CString, mem__Stat)

#define mem_dealloc(x, n)                                                      \
    mem__dealloc(x, (n) * sizeof(*(x)), SPOT(#x, __func__, __FILE__, __LINE__))
#define mem_heapfree(x) mem__heapfree(x, SPOT(#x, __func__, __FILE__, __LINE__))

void* mem__alloc(UInt64 size, const char* desc);
void mem__dealloc(void* ptr, UInt64 size, const char* desc);
void mem__heapfree(void* ptr, const char* desc);

static void* LIBC_MALLOC(size_t s) { return malloc(s); }
static void LIBC_FREE(void* p) { free(p); }

#define malloc(s) mem_alloc("(name unknown)", s)
#ifdef JET_MEM_NOMEMPOOL
#define free(ptr) mem_heapfree(ptr)
#endif
#endif
