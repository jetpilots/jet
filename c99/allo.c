
#include <unistd.h>

unsigned long long getTotalSystemMemory() {
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  return pages * page_size;
}
// this stuff is similar to that paper I once read

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
typedef unsigned long long uint;
#include <string.h>

typedef struct FreeBlock {
  uint size;
  struct FreeBlock* next; /* (+/- offset from &size to) next free block */
} FreeBlock;

typedef struct {
  FreeBlock* ffb;
  uint size;
  // int ffb; // (+/- offset from &s[] to) first free block
  FreeBlock s[];
} Subpool;

#include <sys/mman.h>
#include <mach/vm_statistics.h>

Subpool* Subpool_new(uint bytes) {
  // bytes = 1 << 24;
  // const int MEMORY = 2 * 1024 * 1024;
  // bytes -= (bytes % MEMORY); // + MEMORY;

  Subpool* sp = mmap(NULL, bytes, PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  // malloc(bytes);
  // mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE,
  //     VM_FLAGS_SUPERPAGE_SIZE_2MB, // mach flags in fd argument
  //     0);
  printf("%p\n", sp);
  if (sp == MAP_FAILED) perror(NULL);
  uint spsize = bytes / sizeof(FreeBlock);
  spsize -= 2; // one header; one sentinel
  sp->ffb = sp->s;
  sp->size = spsize; // always const
  sp->s[spsize] = (FreeBlock) {}; // sentinel
  sp->ffb->size = spsize; // setup the ffb (which is at idx 0 now)
  sp->ffb->next = NULL;
  // printf("ffb %p\n", sp->ffb);

  return sp;
}

// void* Subpool_alloc16B(Subpool** spp, uint size) {
//   Subpool* sp = *spp;
//   // size must be in 16B multiples, not bytes
//   FreeBlock* block = sp->ffb;
//   uint avbl;
//   FreeBlock** referer = &sp->ffb;
//   //^ this is the pointer which you followed to get to the current block
//   you
//   // are checking. You start from sp->ffb, but when following a chain
//   from a
//   // block the referer is updated to the addr of the ->next of that
//   block.

// retry:
//   avbl = block->size;

//   if (avbl > size) {
//     // big blocks will be instantly decimated this way even if exact
//     matches
//     // are present down the chain. what to do...
//     FreeBlock* next = block->next;
//     void* ret = block;
//     *referer += size;
//     block += size;
//     block->size = avbl - size, block->next = next;
//     return ret;
//   } else if (avbl == size) {
//     void* ret = block;
//     *referer = block->next;
//     return ret;
//   } else if (!avbl) {
//     uint nsz;
//   notavbl:
//     // hit sentinel.
//     // subpool exhausted. let pool malloc a new one & alloc from there.
//     nsz = (sp->size + 2) * 2 - 2;
//     if (size <= nsz) {
//       *spp = Subpool_new(nsz);
//       return Subpool_alloc(spp, size);
//     }
//     // you should probably let Pool handle this case
//     return NULL; // don't create next subpool with excessive size.
//   } else {
//     if (!block->next) {
//       goto notavbl;
//     } else {
//       block = block->next;
//       referer = &block->next;
//       goto retry;
//     }
//   }
// }
#if __GNUC__ >= 3
#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

void Subpool_snap(Subpool* sp) {
  // return;
  FreeBlock* block = sp->ffb;
  while (block && block->size) {
    if (block->next == block + block->size) {
      block->size += block->next->size;
      block->next = block->next->next;
    }
    block = block->next;
  }
}

void Subpool_jfree16B(Subpool* sp, void* ptr, uint size) {
  FreeBlock* block = ptr;
  block->size = size;
  block->next = sp->ffb;
  sp->ffb = block; // set this block as the new first free block.
  Subpool_snap(sp);
}
void Subpool_free16B(Subpool* sp, void* ptr, uint size) {
  FreeBlock* block = sp->ffb;
  FreeBlock** referer = &sp->ffb;
  while (block && block->size) {
    if (block == (FreeBlock*)ptr + size) {
      // fwd adjacent block is free, so just coalesce the to-be-freed chunk
      // into its adjacent space.
      FreeBlock tmp = *block;
      *referer = ptr; //-= size;
      block = ptr; //-= size;
      block->size = tmp.size + size;
      block->next = tmp.next;

      // we have coalesced with the forward block, but we should try
      // coalescing also with the prev block.

      Subpool_snap(sp);

      // printf("ffb %p\n", sp->ffb);

      return;
    } else if (block + block->size == ptr) {
      // prev adjacent block is free, coalesce
      block->size += size;
      return;
    }
    referer = &block->next;
    block = block->next;
  } // hit sentinel. adjacent block is not free so you cannot coalesce.

  Subpool_jfree16B(sp, ptr, size);
}

// {
//   FreeBlock* block = ptr;

//   if (likely(sp->ffb == block + size)) {
//     block->size = size + sp->ffb->size;
//     block->next = sp->ffb->next;
//   } else {
//     block->size = size;
//     block->next = sp->ffb;
//   }
//   sp->ffb = block; // the new ffb is the freed block.
// }
void* Subpool_alloc16B(Subpool** spp, uint size);

void* Subpool_realloc16B(Subpool** spp, void* ptr, uint szold, uint sznew) {
  Subpool* sp = *spp;

  FreeBlock* block = sp->ffb;
  uint avbl;
  FreeBlock** referer = &sp->ffb;
  uint size = sznew - szold;
  void* ret;

retry:
  avbl = block->size;

  // this always hits when calling alloc which passes ptr=sp->ffb, szold=0
  if (likely(block == (FreeBlock*)ptr + szold)) { // adjacent block is free
    if (likely(avbl > size)) { // and free adjacent block has enough space
      FreeBlock* next = block->next;
      ret = ptr; // when ptr=ffb behave like malloc
      *referer += size;
      block += size;
      block->size = avbl - size, block->next = next;
      // ^ reducing block size only when avbl>size, so block->size cannot be
      // set to zero since that is reserved for the sentinel
      // printf("ffb %p\n", sp->ffb);
      return ret; // nothing to free, old space extended
    } else if (avbl == size) {
      ret = ptr;
      *referer = block->next;
      return ret; // nothing to free, old space extended
    } else if (!avbl) {
      uint nsz;
    notavbl:
      // hit sentinel.
      // subpool exhausted. let pool malloc a new one & alloc from there.
      nsz = (sp->size + 2) * 2 - 2;
      if (size <= nsz) {
        *spp = Subpool_new(nsz);
        goto fallback;
        // void* ret = Subpool_alloc16B(spp, size);
        // memmove(ret, ptr, szold * sizeof(FreeBlock));
        // Subpool_free16B(sp, ptr, szold); // sp is still old subpool
        // return ret;
      }
      // you should probably let Pool handle this case
      return NULL; // don't create next subpool with excessive size.
      // ^ in this case old ptr is still valid.
    } else { // avbl < size, follow chain to next block
      if (!block->next) { goto notavbl; }
      // else {
      //   block = block->next;
      //   referer = &block->next;
      //   goto retry;
      // }
    }
  }

  //  else {
  referer = &block->next;
  if ((block = block->next)) goto retry;

fallback:
  // if you end up here means the adjacent soae is taken .
  // nothing to do except fresh alloc, memcpy & free.
  ret = Subpool_alloc16B(spp, sznew);
  memmove(ret, ptr, szold * sizeof(FreeBlock));
  // since you know that the adj space isnt available, call jfree which will
  // not bother walking the freelist
  Subpool_jfree16B(sp, ptr, szold); // sp is still old subpool
  return ret;
}

void* Subpool_alloc16B(Subpool** spp, uint size) {
  return Subpool_realloc16B(spp, (*spp)->ffb, 0, size);
}

void* Subpool_alloc(Subpool** spp, uint bytes) {
  return Subpool_alloc16B(spp, bytes / sizeof(FreeBlock));
}
void* Subpool_realloc(
    Subpool** spp, void* ptr, uint oldbytes, uint newbytes) {
  return Subpool_realloc16B(
      spp, ptr, oldbytes / sizeof(FreeBlock), newbytes / sizeof(FreeBlock));
}
void Subpool_free(Subpool* sp, void* ptr, uint bytes) {
  return Subpool_free16B(sp, ptr, bytes / sizeof(FreeBlock));
}

void Subpool_stat(Subpool* sp) {
  FreeBlock* block = sp->ffb;
  uint sum = 0, count = 0;
  printf("---\n");
  if (!block->size) {
    // hit sentinel at end; subpool is full
  } else {
    while (1) {
      sum += block->size;
      count++;
      printf("%llu: %lluB at %p (next %p)\n", count,
          block->size * sizeof(FreeBlock), block, block->next);

      if (!block->next) break;
      block = block->next;
    }
    printf("%llu free blocks with %llu free bytes total\n%lld diff\n",
        count, sum * sizeof(FreeBlock),
        sum * sizeof(FreeBlock) - (getTotalSystemMemory() - 32));
  }
  printf("---\n");
}

// #include <unistd.h>

// unsigned long long getTotalSystemMemory() {
//   long pages = sysconf(_SC_PHYS_PAGES);
//   long page_size = sysconf(_SC_PAGE_SIZE);
//   return pages * page_size;
// }
// #define volatile
#define thread_local
volatile void *p, *m;
// #include "../engine/jet/os/clock.h"
// void* p;
volatile Subpool* sp;
#define SZ 64

#include "../engine/jet/base.h"

int main() {
  // return 0;
  printf("%llu\n", getTotalSystemMemory());
  tic();
  sp = Subpool_new(getTotalSystemMemory());
  toc();

  tic();
  for (int i = 0; i < 50000; i++) {
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
    p = Pool_alloc(gPool, SZ);
  }
  toc();

  // Subpool_stat(sp);
  tic();
  for (int i = 0; i < 50000; i++) {
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    // Subpool_free(sp, p, SZ);
    p = Subpool_alloc(&sp, SZ);
    // m = Subpool_alloc(&sp, 256);
    // if (!p) {
    //   puts("err");
    //   break;
    // }
    // void* m = Subpool_alloc(&sp, 256);
    // if (i % 2 == 1) //
    // p = Subpool_realloc(&sp, p, 256, 512);
    // Subpool_stat(sp);

    // Subpool_free(sp, p, SZ);
  }
  // Subpool_stat(sp);

  // Subpool_free(sp, m, 256);
  // }
  toc();
  // Subpool_stat(sp);

  tic();
  for (int i = 0; i < 50000; i++) {
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    // free(p);
    p = malloc(SZ);
    // m = malloc(256);
    // if (!p) {
    //   puts("err");
    //   break;
    // }
    // if (i % 2 == 1) //
    // free(p);
  }
  toc();
}

// // this stuff is similar to that paper I once read

// #include <stdio.h>
// #include <stdlib.h>
// #include <assert.h>
// // #include "../engine/jet/core/Array.h"

// typedef struct {
//   int size; // whole s[] buf size excluding sentinel
//   int ffb; // (+/- offset from &s[] to) first free block
//   int s[];
// } Subpool;

// typedef struct {
//   int size;
//   int next; /* (+/- offset from &size to) next free block */
// } FreeBlock;

// // typedef struct {
// //   PtrArray subpools;
// // } Pool;

// Subpool* Subpool_new(int spsize) {
//   Subpool* ret = malloc(spsize * sizeof(int));
//   spsize -= 2; // 4 + 4 + 4;
//   ret->ffb = 0;
//   ret->s[spsize] = 0;
//   //printf("new subpool of size %dB at %p\n", spsize * sizeof(int), ret);
//   // ret->s[0]=spsize;
//   // ret->s[1]=0;
//   FreeBlock* block = (FreeBlock*)ret->s;
//   block->size = spsize;
//   block->next = 0;
//   return ret;
// }

// void* Subpool_alloc(Subpool* sp, int size) {
//   // size must be in qwords not bytes, and must be even
//   if (size & 1) size++;
//   FreeBlock* block = (FreeBlock*)(sp->s + sp->ffb);
//   int avbl;
// // don't access block->next now since it could just be the sentinel at
// end
// // of buf which is only 4B.
// retry:
//   avbl = block->size;
//   //printf("need %d, avbl %d qwords in ffb at %p\n", size, avbl, block);

//   if (avbl > size) {
//     // big blocks will be instantly decimated this way even if exact
//     matches
//     // are present down the chain. what to do...
//     int next = block->next;
//     void* ret = block;
//     sp->ffb += size;
//     block += size / 2; // because it has 2 ints
//     block->size = avbl - size;
//     block->next = next;
//     //printf("- alloc %d qwords at %p\n", size, ret);
//     return ret;
//   } else if (avbl == size) {
//     void* ret = block;
//     sp->ffb = block->next;
//     //printf("- exact alloc %d qwords at %p\n", size, ret);
//     return ret;
//   } else if (!avbl) {
//   notavbl:
//     // subpool exhausted. let pool malloc a new one & alloc from there.
//     //printf("- subpool exhausted\n");
//     return NULL;
//   } else {
//     if (!block->next) {
//       //printf("- no next block\n");
//       goto notavbl;
//     } else {
//       //printf("- checking next block at offset %d\n", block->next);
//       block = (FreeBlock*)(sp->s + block->next);
//       goto retry;
//     }
//   }
// }

// void Subpool_free(Subpool* sp, void* ptr, int size) {
//   int offs = (int*)ptr - sp->s;
//   if (size & 1) size++;
//   //printf("freeing %d qwords at %p\n", size, ptr);
//   FreeBlock* block = (FreeBlock*)(sp->s + offs);
//   block->size = size;
//   block->next = sp->ffb;
//   sp->ffb = offs;
// }

// void Subpool_stat(Subpool* sp) {
//   FreeBlock* block = (FreeBlock*)(sp->s + sp->ffb);
//   int sum = 0, count = 0;
//   if (!block->size) {
//     // hit sentinel at end; subpool is full
//     //printf("subpool %p is full\n", sp);
//   } else {
//     while (1) {
//       sum += block->size;
//       count++;
//       if (!block->next) break;
//       block = (FreeBlock*)(sp->s + block->next);
//     }
//     //printf("%d free blocks with %d free qwords total\n", count, sum);
//   }
// }

// int main() {
//   Subpool* sp = Subpool_new(4096 / 4);
//   for (int i = 0; i < 4000; i++) {
//     void* p = Subpool_alloc(sp, 32);
//     if (!p) break;
//     if (i % 2 == 1) Subpool_free(sp, p, 32);
//     //printf("--- done iter %d\n", i + 1);
//   }
//   Subpool_stat(sp);
// }