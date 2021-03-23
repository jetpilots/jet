#define JET_MEM_NOMEMPOOL
#include "mem.h"

#ifndef JET_MEM_DISABLED

Dict(Ptr, mem__PtrInfo) mem__ptrDict[1];
UInt64 mem__heapTotal = 0;
Dict(CString, mem__Stat) mem__allocStats[1];

int mem__cmpsum(const void* a, const void* b) {

    int ia = *(int*)a;
    int ib = *(int*)b;
    UInt64 va = Dict_val(mem__allocStats, ia).sum;
    UInt64 vb = Dict_val(mem__allocStats, ib).sum;
    return va > vb ? -1 : va == vb ? 0 : 1;
}

void mem__heapfree(void* ptr, const char* desc) {
    EP;

    int d = Dict_get(Ptr, mem__PtrInfo)(mem__ptrDict, ptr);

    if (d < Dict_end(mem__ptrDict)) {

        if (!Dict_val(mem__ptrDict, d).heap) {
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

    if (*stat) {
        mem__SizeClassInfo* ptr = new mem__SizeClassInfo;

        Dict_val(mem__sizeDict, d) = ptr;
    }
    mem__SizeClassInfo* inf = Dict_val(mem__sizeDict, d);

    PtrArray_push(&inf->arr, ptr);

#ifndef JET_MEM_NOHEAPTRACKER
    Dict_deleteByKey(Ptr, mem__PtrInfo)(mem__ptrDict, ptr);
#endif

    EX
}

void* mem__alloc(UInt64 size, const char* desc)

{
    EP;
#ifndef JET_MEM_NOMEMPOOL
    int d = Dict_get(UInt64, Ptr)(mem__sizeDict, size);
    if (d < Dict_end(mem__sizeDict)) {
        mem__SizeClassInfo* inf = Dict_val(mem__sizeDict, d);
        if (inf and inf->arr.used) {
            void* ret = PtrArray_pop(&inf->arr);

            return ret;
        }
    }

    bool fromPool = size <= 256 or size <= (gPool->cap - gPool->used);
    void* ret = fromPool ? Pool_alloc(gPool, size)

                         : LIBC_MALLOC(size);
    if (!fromPool) mem__heapTotal += size;
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
        mem__PtrInfo inf = { .size = size, .heap = not fromPool, .desc = desc };
        UInt32 p = Dict_put(Ptr, mem__PtrInfo)(mem__ptrDict, ret, stat);
        Dict_val(mem__ptrDict, p) = inf;
    }
#endif

    EX;
    return ret;
}

#endif

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

            puts("---------------------------------------------------------"
                 "-----");

            printf("Leaked %llu bytes (%.2f%% of %s total %llu "
                   "bytes)\n",
                sum, sum * 100.0 / rss, heap ? "heap" : "pool", rss);

            Dict_clear(CString, mem__Stat)(mem__allocStats);
            LIBC_FREE(indxs);
        } else {
            puts("-- no leaks.");
        }
    }

    EX;
    return sum;
}

void user(double* b) { (void)b; }

double* func();

int main(int argc, char* argv[]) {
    char* bufc = malloc(sizeof(char) * 51200);
    free(bufc);

    for (int i = 0; i < 10; i++) {
        double* buf = malloc(sizeof(double) * 1024);
        user(buf);
        if (i % 2) free(buf);
    }

    double* external = func();

    char stbuf[512];

    mem_stats(true);

    return 0;
}
/*
int msain()
{
    EP;
    double* d = mem_alloc("d", 8 * sizeof(double));
    double* d3 = mem_alloc("d3", 2 * sizeof(double));


    double* d2 = mem_alloc("d2", 32 * sizeof(double));
    double *e, *ex;
    double sum = 0.0;
    for (int i = 0; i < 100000; i++) {









        e = mem_alloc("e", 320 * sizeof(double));
        ex = mem_alloc("ex", 32 * sizeof(double));
        for (int j = 0; j < 32; j++) sum += e[j] + ex[j];













    }
    printf("sum = %g\n", sum);







    mem_dealloc(d3, 2);











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