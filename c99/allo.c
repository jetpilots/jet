// this stuff is similar to that paper I once read

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
  int size;
  int next; /* (+/- offset from &size to) next free block */
} FreeBlock;

typedef struct {
  int size;
  int ffb; // (+/- offset from &s[] to) first free block
  FreeBlock s[];
} Subpool;

// spsize must be num of qwords not bytes, minimum 2. typically start from
// 4KB/8 and go up to 1MB/8.
Subpool* Subpool_new(int spsize) {
  Subpool* sp = malloc(spsize * sizeof(void*));
  spsize -= 2; // one header; one sentinel
  sp->ffb = 0;
  sp->size = spsize; // always const
  sp->s[spsize] = (FreeBlock) {}; // sentinel
  sp->s[0].size = spsize; // setup the ffb (which is at idx 0 now)
  sp->s[0].next = 0;
  return sp;
}

void* Subpool_alloc(Subpool** spp, int size) {
  Subpool* sp = *spp;
  // size must be in 8B multiples, not bytes
  FreeBlock* block = sp->s + sp->ffb;
  int avbl;
  int* referer = &sp->ffb;
  //^ this is the pointer which you followed to get to the current block you
  // are checking. You start from sp->ffb, but when following a chain from a
  // block the referer is updated to the addr of the ->next of that block.

retry:
  avbl = block->size;

  if (avbl > size) {
    // big blocks will be instantly decimated this way even if exact matches
    // are present down the chain. what to do...
    int next = block->next;
    void* ret = block;
    *referer += size;
    block += size;
    block->size = avbl - size, block->next = next;
    return ret;
  } else if (avbl == size) {
    void* ret = block;
    *referer = block->next;
    return ret;
  } else if (!avbl) {
  notavbl:
    // subpool exhausted. let pool malloc a new one & alloc from there.
    if (size <= (sp->size * 2)) {
      int nsz = (sp->size + 2) * 2 - 2;
      // Subpool* nsp = Subpool_new(nsz);
      // void* ret = Subpool_alloc(&nsp, size);
      // *spp = nsp;
      // return ret;
      *spp = Subpool_new(nsz);
      return Subpool_alloc(spp, size);
    } // you should probably let Pool handle this case
    return NULL; // don't create next subpool with excessive size.
  } else {
    if (!block->next) {
      goto notavbl;
    } else {
      block = sp->s + block->next;
      referer = &block->next;
      goto retry;
    }
  }
}
#if __GNUC__ >= 3
#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif
void Subpool_free(Subpool* sp, void* ptr, int size) {
  int offs = (FreeBlock*)ptr - sp->s;
  FreeBlock* block = sp->s + offs;

  // TODO: optimisation if you find that the bext contiguous block is the
  // next ptr then coalesce them
  if (likely(sp->s + sp->ffb == block + size)) {
    // at the top of the stack. coalesce with the large contiguous chunk
    // instead of creating a new linked list entry.
    // this optim gives 10x improvement esp when you are asking or
    // increasingly larger objs after freeing. Recently freed objs are going
    // to be useless and they are going to lengthen the chain each time
    // (remember new stuff is shifted on to the freelist, not pushed)
    // for allocating the same size obj over and over, having this branch
    // causes about 5% overhead (mainly for computing the condition; NOT
    // branch penalty)
    FreeBlock* nextblock = sp->s + sp->ffb;
    block->size = size + nextblock->size;
    block->next = nextblock->next;
  } else {
    // not at top of stack. we'll shift this on the linked list, set ffb to
    // this block and set its next to the old ffb.
    block->size = size;
    block->next = sp->ffb;
  }
  sp->ffb = offs; // the new ffb is the freed block.
}

void Subpool_stat(Subpool* sp) {
  FreeBlock* block = sp->s + sp->ffb;
  int sum = 0, count = 0;
  if (!block->size) {
    // hit sentinel at end; subpool is full
  } else {
    while (1) {
      sum += block->size;
      count++;
      if (!block->next) break;
      block = sp->s + block->next;
    }
  }
}
#define thread_local

#include "../engine/jet/os/clock.h"
int main() {
  tic();
  Subpool* sp = Subpool_new(4096 / 8);
  for (int i = 0; i < 40000; i++) {
    void* p = Subpool_alloc(&sp, i + 16);
    if (!p) break;
    // if (i % 2 == 1)
    Subpool_free(sp, p, i + 16);
  }
  Subpool_stat(sp);
  toc();

  tic();
  for (int i = 0; i < 400000; i++) {
    void* p = malloc(8 * 32);
    if (!p) break;
    // if (i % 2 == 1)
    free(p);
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