typedef struct Pool {
  void* ref;
  UInt32 cap, capTotal; // BYTES
  Array(VPtr) ptrs;
  Array(UInt32) caps;
  UInt32 used, usedTotal; // used BYTES, unlike in Array!
} Pool;

monostatic void* Pool_alloc(Pool* self, size_t reqd) {
  void* ans = NULL;
  // printf("asked for %zu B\n", reqd);

  // This is a pool for single objects, not arrays or large strings.
  // dont ask for a big fat chunk larger than 16KB (or up to 256KB
  // depending on how much is already there) all at one time.
  if (unlikely(self->used + reqd > self->cap)) {
    if (likely(self->ref)) {
      Array_push(VPtr)(&self->ptrs, self->ref);
      Array_push(UInt32)(&self->caps, self->cap);
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
} SmallPtr;

// returns a "SmallPtr"
monostatic SmallPtr Pool_allocs(Pool* self, size_t reqd) {
  SmallPtr ans = {};

  // This is a pool for single objects, not arrays or large strings.
  // dont ask for a big fat chunk larger than 16KB (or up to 256KB
  // depending on how much is already there) all at one time.
  if (self->used + reqd > self->cap) {
    if (self->ref) Array_push(VPtr)(&self->ptrs, self->ref);
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

monostatic void* Pool_deref(Pool* self, SmallPtr sptr) {
  return sptr.id ? self->ptrs.ref[sptr.id - 1] + sptr.ptr
                 : self->ref + sptr.ptr;
}

monostatic void Pool_free(Pool* self) {
  // TODO: reset used here?
  if (self->cap) free(self->ref);
  for_to(i, self->ptrs.used) free(self->ptrs.ref[i]);
}

monostatic Pool gPool[1] = {};
monostatic Pool sPool[1] = {};

// #ifndef NDEBUG
#define NEW(T) T##_new_()
//(++_allocTotal_##T, (T*)Pool_alloc(gPool, sizeof(T)))
// Use nNEW for contiguous alloc of n objects of type T (for small n!)
// #define nNEW(T, n) (_allocTotal_##T += n, Pool_alloc(gPool, n *
// sizeof(T)))
#define NEWW(T, ...) jet_mem_copy(NEW(T), &(T) { __VA_ARGS__ }, sizeof(T))

// #else
// #define NEW(T) Pool_alloc(gPool, sizeof(T))
// Use nNEW for contiguous alloc of n objects of type T (for small n!)
// #define nNEW(T, n) Pool_alloc(gPool, n * sizeof(T))
// #endif