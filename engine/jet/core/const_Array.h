#define const_Array(T) const_Array_##T
#define const_Array_free(T) const_Array_free_##T
#define const_Array_get(T) const_Array_get_##T
#define const_Array_growTo(T) const_Array_growTo_##T
#define const_Array_resize(T) const_Array_resize_##T
#define const_Array_concatCArray(T) const_Array_concatCArray_##T
#define const_Array_concatArray(T) const_Array_concatArray_##T
#define const_Array_grow(T) const_Array_grow_##T
#define const_Array_push(T) const_Array_push_##T
#define const_Array_justPush(T) const_Array_justPush_##T
#define const_Array_clear(T) const_Array_clear_##T
#define const_Array_initWithCArray(T) const_Array_initWithCArray_##T
#define const_Array_pop(T) const_Array_pop_##T
#define const_Array_top(T) const_Array_top_##T
#define const_Array_empty(T) const_Array_empty_##T
#define const_Array_new(T) const_Array_new_##T
#define const_Array_make(T) const_Array_make_##T

// DECL_Array(const_Number);
// DECL_Array(const_CString);

#define MAKE_const_Array(T)                                                \
  struct const_Array(T) {                                                  \
    const T* const restrict ref;                                           \
    const UInt32 used;                                                     \
  };                                                                       \
  monostatic const_Array(T) * const_Array_new(T)() {                       \
    return calloc(1, sizeof(const_Array(T)));                              \
  }                                                                        \
  monostatic void const_Array_free(T)(const_Array(T) * self) {             \
    if (self->cap) free(self->ref);                                        \
  }                                                                        \
  monostatic T const_Array_get(T)(const_Array(T) * self, UInt32 index) {   \
    return self->ref[index];                                               \
  }                                                                        \
  monostatic const_Array(T) * const_Array_make(T)(T arr[], int count) {    \
    const_Array(T)* a = const_Array_new(T)();                              \
    const_Array(T) _a = { arr, count };                                    \
    *a = _a;                                                               \
    return a;                                                              \
  }                                                                        \
  monostatic T const_Array_top(T)(const_Array(T) * self) {                 \
    return self->used ? self->ref[self->used - 1] : 0;                     \
  }                                                                        \
  monostatic bool const_Array_empty(T)(const_Array(T) * self) {            \
    return self->used == 0;                                                \
  }

MAKE_const_Array(const_Number);
MAKE_const_Array(const_CString);