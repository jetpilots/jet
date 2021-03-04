#define JET_MEM_NOMEMPOOL
#include "mem.h"

#ifndef JET_MEM_DISABLED

Dict(Ptr, mem__PtrInfo) mem__ptrDict[1];
UInt64 mem__heapTotal = 0;
Dict(CString, mem__Stat) mem__allocStats[1];

int mem__cmpsum(const void* a, const void* b) {
    // For sorting in descending order of sum
    int ia = *(int*)a;
    int ib = *(int*)b;
    UInt64 va = Dict_val(mem__allocStats, ia).sum;
    UInt64 vb = Dict_val(mem__allocStats, ib).sum;
    return va > vb ? -1 : va == vb ? 0 : 1;
}

void mem__heapfree(void* ptr, const char* desc) {
    EP;
    // int stat[1];
    int d = Dict_get(Ptr, mem__PtrInfo)(mem__ptrDict, ptr);
    // use put so we can still access deleted items
    // printf("%d %d\n", d, mem__ptrDict->nBuckets);
    // if (stat == 2) {

    if (d < Dict_end(mem__ptrDict)) {
        // else {
        if (not Dict_val(mem__ptrDict, d).heap) {
            printf("free non-heap pointer at %s\n", desc);
        } else {
            LIBC_FREE(ptr);
            mem__heapTotal -= Dict_val(mem__ptrDict, d).size;
            Dict_delete(Ptr, mem__PtrInfo)(mem__ptrDict, d);
        }
    } else {
        printf("calling free on unknown pointer at %s\n", desc);
    }
    EX
}
void mem__dealloc(void* ptr, UInt64 size, const char* desc) {
    EP;
    int stat[1] = {};
    int d = Dict_put(UInt64, Ptr)(mem__sizeDict, size, stat);

    // newly added, or got a previously deleted bucket so set new list
    if (*stat /* or not Dict_val(mem__sizeDict, d)*/) {
        mem__SizeClassInfo* ptr = NEW(mem__SizeClassInfo);
        //*ptr =
        Dict_val(mem__sizeDict, d) = ptr;
    }
    mem__SizeClassInfo* inf = Dict_val(mem__sizeDict, d);
    // if (inf->count>=32) { /* really free it if on heap using mem_heapfree
    // */} else
    // {

    // inf->list = PtrList_with_next(ptr, inf->list);
    PtrArray_push(&inf->arr, ptr);

    // inf->count++;
#ifndef JET_MEM_NOHEAPTRACKER
    Dict_deleteByKey(Ptr, mem__PtrInfo)(mem__ptrDict, ptr);
#endif

    EX
}

void* mem__alloc(UInt64 size, const char* desc)
// alloc from freelist, or pool or heap, in that order of preference
{
    EP;
#ifndef JET_MEM_NOMEMPOOL
    int d = Dict_get(UInt64, Ptr)(mem__sizeDict, size);
    if (d < Dict_end(mem__sizeDict)) {
        mem__SizeClassInfo* inf = Dict_val(mem__sizeDict, d);
        if (inf and inf->arr.used) {
            void* ret = PtrArray_pop(&inf->arr); // inf->arr->item;
            // mem_dealloc(inf->list, 1); // WHY?? -> to drop the listitem
            // inf->list = inf->list->next;
            // inf->count--;
            return ret;
        }
    }
    // see if it can fit in the pool, if yes, allocate from there.
    // if size is smallish <= N, force an allocation from the pool.
    // If current subpool doesn't have enough bytes, it'll calloc
    // a new subpool, you'll lose at most N-1 bytes.
    bool fromPool = size <= 256 or size <= (gPool->cap - gPool->used);
    void* ret = fromPool //
        ? Pool_alloc(gPool, size)
        // else fall back to the heap allocator.
        : LIBC_MALLOC(size); // mem__heapmalloc(size, desc);
    if (not fromPool) mem__heapTotal += size;
#else
    bool fromPool = false;
    void* ret = LIBC_MALLOC(size);
#ifndef NOHEAPTOTAL
    mem__heapTotal += size;
#endif
#endif

#ifndef JET_MEM_NOHEAPTRACKER
    if (ret) {
        int stat[1];
        mem__PtrInfo inf = { //
            .size = size,
            .heap = not fromPool,
            .desc = desc
        };
        UInt32 p = Dict_put(Ptr, mem__PtrInfo)(mem__ptrDict, ret, stat);
        Dict_val(mem__ptrDict, p) = inf;
    }
#endif

    EX;
    return ret;
}

#endif

// f+ fp.mem.stats()
UInt64 mem_stats(bool heap) {
    EP;
    int i = 0;
    UInt64 sum = 0;
    if (Dict_size(mem__ptrDict)) {
        int stat[1];

        Dict_foreach(mem__ptrDict, Ptr key, mem__PtrInfo val, {
            if (heap != val.heap) continue;
            int d
                = Dict_put(CString, mem__Stat)(mem__allocStats, val.desc, stat);
            if (*stat) {
                Dict_val(mem__allocStats, d).sum = 0;
                Dict_val(mem__allocStats, d).count = 0;
            }
            Dict_val(mem__allocStats, d).sum += val.size;
            Dict_val(mem__allocStats, d).count++;
            sum += val.size;
        });

        if (mem__allocStats->size) {
            int i = 0,
                *indxs = LIBC_MALLOC(mem__allocStats->size * sizeof(int));
            Dict_foreach(mem__allocStats, CString key, mem__Stat val,
                { indxs[i++] = _i_; });
            qsort(indxs, mem__allocStats->size, sizeof(int), mem__cmpsum);
            printf("\n%s STATISTICS\n", heap ? "HEAP" : "POOL");
            puts("-------------------------------------------------------------"
                 "-");
            printf("Allocations | Total Size (B) | Variable and Location\n");
            puts("-------------------------------------------------------------"
                 "-");
            for (i = 0; i < mem__allocStats->size; i++) {
                CString key = mem__allocStats->keys[indxs[i]];
                mem__Stat val = mem__allocStats->vals[indxs[i]];
                printf("%11llu | %14llu | %s\n", val.count, val.sum, key);
            }
            UInt64 rss = heap ? mem__heapTotal : gPool->usedTotal;
            // if (sum) {
            puts("---------------------------------------------------------"
                 "-----");
            // todo: this should be heap total or pool total resp. depending on
            // the arg
            printf("Leaked %llu bytes (%.2f%% of %s total %llu "
                   "bytes)\n",
                sum, sum * 100.0 / rss, heap ? "heap" : "pool", rss);
            // }
            Dict_clear(CString, mem__Stat)(mem__allocStats);
            LIBC_FREE(indxs);
        } else {
            puts("-- no leaks.");
        }
    }

    EX;
    return sum;
}

void user(double* b) {
    (void)b;
    // return b;
    // free(b);
}

double* func();

int main(int argc, char* argv[]) {
    char* bufc = malloc(sizeof(char) * 51200);
    free(bufc);

    // 1. heap buffer not freed
    for (int i = 0; i < 10; i++) {
        double* buf = malloc(sizeof(double) * 1024);
        user(buf);
        if (i % 2) free(buf);
    }

    double* external = func();

    // 2. heap buffer freed more than once
    // free(bufc);

    // 3. attempt to free non-heap buffer
    char stbuf[512];
    // free(stbuf);

    mem_stats(true);

    return 0;
}
/*
int msain()
{
    EP;
    double* d = mem_alloc("d", 8 * sizeof(double));
    double* d3 = mem_alloc("d3", 2 * sizeof(double));
    // mem_dealloc(d3, 2);

    double* d2 = mem_alloc("d2", 32 * sizeof(double));
    double *e, *ex;
    double sum = 0.0;
    for (int i = 0; i < 100000; i++) {
        // TODO: in such a tight and long loop, if the subpool is close to the
        // end around the start of the loop, most of the allocations will go to
        // the heap -> a new subpool will not be created at all if size > 256,
        // and 1000s of allocs can fall through to malloc. So instead of this
        // 256 threshold you should, when you see subpool remainder not enough
        // for the request, make a fake allocation that covers the subpool
        // remainder, and alloc a new subpool anyway. direct fallthrough to
        // malloc should be only for requests that are really large.
        // to see the difference, change 256 to 257 below and see the effect.
        e = mem_alloc("e", 320 * sizeof(double));
        ex = mem_alloc("ex", 32 * sizeof(double));
        for (int j = 0; j < 32; j++) sum += e[j] + ex[j];
        // really large requests would be those larger than the next upcoming
        // subpool size.
        // printf("%d. %p\n", i, e);
        //...
        // if (i > 23)
        // mem_dealloc(e, 32);
        // mem_dealloc(ex, 32);
        // mem_heapfree(e);
        // ex = mem_alloc("ex as String", 14 * i * sizeof(char));
        // }
        // nopool: 3.5  alloc/free , 4.7  alloc/nofree
        // pool: 2 , 4.10
        // tracker: 3 , 1m45s
    }
    printf("sum = %g\n", sum);

    // no pool -> 5.845s O3 S, 4.550 L
    // pool + tracking -> 3.415 O3 S, 4.624 L
    // pool no tracking -> 3.194 O3 S, 4.447 L
    // stack storage -> 2.502s S, L

    // mem_dealloc(d, 8);
    mem_dealloc(d3, 2);
    // mem_dealloc(d2, 32);
    // mem_heapfree(d3);
    // mem_heapfree(d);
    // mem_heapfree(d2);
    // typeof(*d) dc = d[0];
    // printf("%s", typeof(*d));
    // WHY REPORT? JUST WALK THE LIST AND FREE THEM BEFORE EXIT.
    // REPORT IS ONLY USEFUL TO SEE WHAT COULD HAVE BEEN RELEASED WHILE
    // THE PROGRAM WAS RUNNING, TO GIVE BACK MEM to the system.
    // SO DON'T CALL IT "LEAKED", JUST "LEFT OVER"

    mem_stats(true);
    mem_stats(false);

    Dict_foreach(mem__sizeDict, UInt64 key,
        mem__SizeClassInfo * inf,
        {
            printf("size class %llu B -> %d ptrs, total %u B / actual %u B\n",
                key, inf->arr.used, inf->arr.used * 8, inf->arr.cap * 8);
        })

        printf("%d x %zu B -> %zu B SizeClassInfo\n",
            mem__SizeClassInfo_allocTotal, sizeof(mem__SizeClassInfo),
            mem__SizeClassInfo_allocTotal * sizeof(mem__SizeClassInfo));

    printf("%d x %zu B -> %zu B PtrList\n", PtrList_allocTotal,
        sizeof(PtrList), PtrList_allocTotal * sizeof(PtrList));

    EX return 0;
}*/