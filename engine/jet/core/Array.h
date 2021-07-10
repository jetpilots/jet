#define Array(T) Array_##T
#define Array_free(T) Array_free_##T
#define Array_get(T) Array_get_##T
#define Array_growTo(T) Array_growTo_##T
#define Array_resize(T) Array_resize_##T
#define Array_concatCArray(T) Array_concatCArray_##T
#define Array_concatArray(T) Array_concatArray_##T
#define Array_grow(T) Array_grow_##T
#define Array_push(T) Array_push_##T
#define Array_justPush(T) Array_justPush_##T
#define Array_clear(T) Array_clear_##T
#define Array_initWithCArray(T) Array_initWithCArray_##T
#define Array_pop(T) Array_pop_##T
#define Array_top(T) Array_top_##T
#define Array_empty(T) Array_empty_##T
#define Array_new(T) Array_new_##T
#define Array_make(T) Array_make_##T

// convenience for manual writing
#define PtrArray Array(VPtr)
// #define arr_free Array_free(VPtr)
// #define arr_growTo Array_growTo(VPtr)
// #define arr_concatCArray Array_concatCArray(VPtr)
// #define arr_concatArray Array_concatArray(VPtr)
// #define arr_grow Array_grow(VPtr)
#define arr_push Array_push(VPtr)
// #define arr_clear Array_clear(VPtr)
// #define arr_initWithCArray Array_initWithCArray(VPtr)
// #define arr_justPush Array_justPush(VPtr)
#define arr_pop Array_pop(VPtr)
#define arr_top Array_top(VPtr)
#define arr_empty Array_empty(VPtr)
#define arr_topAs(T, self) ((T)arr_top(self))

#define DECL_Array(T)                                                      \
  typedef struct Array(T) Array(T);                                        \
  monostatic Array(T) * Array_new(T)();                                    \
  monostatic void Array_free(T)(Array(T) * self);                          \
  monostatic void Array_growTo(T)(Array(T) * self, UInt32 size);           \
  monostatic void Array_resize(T)(Array(T) * self, UInt32 size);           \
  monostatic T Array_get(T)(Array(T) * self, UInt32 index);                \
  monostatic void Array_concatCArray(T)(                                   \
      Array(T) * self, T * cArray, int count);                             \
  monostatic void Array_concatArray(T)(Array(T) * self, Array(T) * other); \
  monostatic Array(T) * Array_make(T)(T arr[], int count);                 \
  monostatic void Array_clear(T)(Array(T) * self);                         \
  monostatic void Array_initWithCArray(T)(                                 \
      Array(T) * self, T * cArray, int count);                             \
  monostatic void Array_grow(T)(Array(T) * self);                          \
  monostatic void Array_justPush(T)(Array(T) * self, T node);              \
  monostatic void Array_push(T)(Array(T) * self, T node);                  \
  monostatic T Array_pop(T)(Array(T) * self);                              \
  monostatic T Array_top(T)(Array(T) * self);                              \
  monostatic bool Array_empty(T)(Array(T) * self);

// DECL_Array(VPtr);
// DECL_Array(UInt32);
// DECL_Array(Real64);
// DECL_Array(CString);
// DECL_Array(Number);

// MAKE_Array(UInt32);
// MAKE_Array(uint64_t);
// MAKE_Array(int64_t);
// MAKE_Array(int32_t);
// MAKE_Array(Number);
// MAKE_Array(float);
// make array for strings etc later

// Array_top(T) is only defined for value types to keep the number
// of instantiations (of the "template" Array) down. So void* represents
// object ptrs of all types. Cast them when you need to deref or do ->
// etc. self is used to get a void* as a T (usually a SomeType*)

// TODO: StaticArray type with size and array, StaticArray2D/3Detc.
// since self is not templated, it's your job to send items of the
// right size, or face the music
// ASSUMING SIZE IS 8. THAT MEANS NO FLOAT OR UINT32, only sizeof(void*)
// #define Array_concatCArray(T, Array, arr, count) \
//     Array_concat_cArray_(Array, arr, count * sizeof(T))
// TODO: the compiler should optimise away calls to concat if the
// original arrays can be used one after the other. e.g. concat two
// arrays then print it can be done by simply printing first then
// second, no memcpy involved.
// #define Array_concatArray(T, s1, s2) \
//     Array_concatArray_(s1, s2, sizeof(T))

#define MAKE_Array(T)                                                      \
  typedef struct Array(T) {                                                \
    T* restrict ref;                                                       \
    UInt32 used;                                                           \
    UInt32 cap;                                                            \
  }                                                                        \
  Array(T);                                                                \
  monostatic Array(T) * Array_new(T)(void) {                                   \
    return calloc(1, sizeof(Array(T)));                                    \
  }                                                                        \
  monostatic void Array_free(T)(Array(T) * self) {                         \
    if (self->cap) free(self->ref);                                        \
  }                                                                        \
  monostatic void Array_growTo(T)(Array(T) * self, UInt32 size) {          \
    self->cap = roundUp32(size);                                           \
    self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
    memset(                                                                \
        self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
  }                                                                        \
  monostatic void Array_resize(T)(Array(T) * self, UInt32 size) {          \
    if (size > self->cap) Array_growTo(T)(self, size);                     \
    self->used = size;                                                     \
  }                                                                        \
  monostatic T Array_get(T)(Array(T) * self, UInt32 index) {               \
    return self->ref[index - 1];                                           \
  }                                                                        \
  monostatic void Array_concatCArray(T)(                                   \
      Array(T) * self, T * cArray, int count) {                            \
    const UInt32 reqd = self->used + count;                                \
    if (reqd >= self->cap) Array_growTo(T)(self, reqd);                    \
    memcpy(self->ref + self->used, cArray, count * sizeof(T));             \
  }                                                                        \
  monostatic void Array_concatArray(T)(                                    \
      Array(T) * self, Array(T) * other) {                                 \
    Array_concatCArray(T)(self, other->ref, other->used);                  \
  }                                                                        \
  monostatic void Array_clear(T)(Array(T) * self) { self->used = 0; }      \
  monostatic void Array_initWithCArray(T)(                                 \
      Array(T) * self, T * cArray, int count) {                            \
    Array_clear(T)(self);                                                  \
    Array_concatCArray(T)(self, cArray, count);                            \
  } /* maybe this can be merged with growTo */                             \
  monostatic Array(T) * Array_make(T)(T arr[], int count) {                \
    Array(T)* a = Array_new(T)();                                          \
    Array_initWithCArray(T)(a, arr, count);                                \
    return a;                                                              \
  }                                                                        \
  monostatic void Array_grow(T)(Array(T) * self) {                         \
    self->cap = self->cap ? 2 * self->cap : 8;                             \
    self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
    memset(                                                                \
        self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
  }                                                                        \
  monostatic void Array_justPush(T)(Array(T) * self, T node) {             \
    self->ref[self->used++] = node; /* when you know that cap is enough */ \
  }                                                                        \
  monostatic void Array_push(T)(Array(T) * self, T node) {                 \
    if (self->used >= self->cap) Array_grow(T)(self);                      \
    Array_justPush(T)(self, node);                                         \
  }                                                                        \
  monostatic T Array_pop(T)(Array(T) * self) {                             \
    assert(self->used > 0);                                                \
    return self->ref[--self->used];                                        \
  }                                                                        \
  monostatic T Array_top(T)(Array(T) * self) {                             \
    return self->used ? self->ref[self->used - 1] : 0;                     \
  }                                                                        \
  monostatic bool Array_empty(T)(Array(T) * self) {                        \
    return self->used == 0;                                                \
  }

MAKE_Array(VPtr);
MAKE_Array(UInt32);
MAKE_Array(Real64);
MAKE_Array(Number);
MAKE_Array(CString);
