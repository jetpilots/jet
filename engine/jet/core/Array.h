#define array_t(T) array_##T
#define array_free(T) array_free_##T
#define array_get(T) array_get_##T
#define array_growto(T) array_growTo_##T
#define array_resize(T) array_resize_##T
#define array_concatCarray(T) array_concatCarray_##T
#define array_concatarray(T) array_concatarray_##T
#define array_grow(T) array_grow_##T
#define array_push(T) array_push_##T
#define array_justpush(T) array_justPush_##T
#define array_clear(T) array_clear_##T
#define array_initWithCarray(T) array_initWithCarray_##T
#define array_pop(T) array_pop_##T
#define array_top(T) array_top_##T
#define array_empty(T) array_empty_##T

// convenience for manual writing
#define ptrarray_t array_t(ptr_t)
// #define ptrarray_free array_free(ptr_t)
// #define ptrarray_growTo array_growto(ptr_t)
// #define ptrarray_concatCarray_t array_concatCarray(ptr_t)
// #define ptrarray_concatarray_t array_concatarray(ptr_t)
// #define ptrarray_grow array_grow(ptr_t)
#define ptrarray_push array_push(ptr_t)
// #define ptrarray_clear array_clear(ptr_t)
// #define ptrarray_initWithCarray_t array_initWithCarray_t(ptr_t)
// #define ptrarray_justPush array_justpush(ptr_t)
#define ptrarray_pop array_pop(ptr_t)
#define ptrarray_top array_top(ptr_t)
#define ptrarray_empty array_empty(ptr_t)
#define ptrarray_topAs(T, self) ((T)ptrarray_top(self))

#define MAKE_array_t(T)                                                        \
    typedef struct array_t(T) {                                                \
        T* ref;                                                                \
        uint32_t used;                                                         \
        uint32_t cap;                                                          \
    }                                                                          \
    array_t(T);                                                                \
    jet_static void array_free(T)(array_t(T) * self) {                         \
        if (self->cap) free(self->ref);                                        \
    }                                                                          \
    jet_static void array_growto(T)(array_t(T) * self, uint32_t size) {        \
        self->cap = roundUp32(size);                                           \
        self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
        memset(                                                                \
            self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
    }                                                                          \
    jet_static void array_resize(T)(array_t(T) * self, uint32_t size) {        \
        if (size > self->cap) array_growto(T)(self, size);                     \
        self->used = size;                                                     \
    }                                                                          \
    jet_static T array_get(T)(array_t(T) * self, uint32_t index) {             \
        return self->ref[index];                                               \
    }                                                                          \
    jet_static void array_concatCarray(T)(                                     \
        array_t(T) * self, T * carray, int count) {                            \
        const uint32_t reqd = self->used + count;                              \
        if (reqd >= self->cap) array_growto(T)(self, reqd);                    \
        memcpy(self->ref + self->used, carray, count * sizeof(T));             \
    }                                                                          \
    jet_static void array_concatarray(T)(                                      \
        array_t(T) * self, array_t(T) * other) {                               \
        array_concatCarray(T)(self, other->ref, other->used);                  \
    }                                                                          \
    jet_static void array_clear(T)(array_t(T) * self) { self->used = 0; }      \
    jet_static void array_initWithCarray_t(T)(                                 \
        array_t(T) * self, T * carray, int count) {                            \
        array_clear(T)(self);                                                  \
        array_concatCarray(T)(self, carray, count);                            \
    } /* maybe this can be merged with growTo */                               \
    jet_static void array_grow(T)(array_t(T) * self) {                         \
        self->cap = self->cap ? 2 * self->cap : 8;                             \
        self->ref = realloc(self->ref, sizeof(T) * self->cap);                 \
        memset(                                                                \
            self->ref + self->used, 0, sizeof(T) * (self->cap - self->used));  \
    }                                                                          \
    jet_static void array_justpush(T)(array_t(T) * self, T node) {             \
        self->ref[self->used++] = node; /* when you know that cap is enough */ \
    }                                                                          \
    jet_static void array_push(T)(array_t(T) * self, T node) {                 \
        if (self->used >= self->cap) array_grow(T)(self);                      \
        array_justpush(T)(self, node);                                         \
    }                                                                          \
    jet_static T array_pop(T)(array_t(T) * self) {                             \
        assert(self->used > 0);                                                \
        return self->ref[--self->used];                                        \
    }                                                                          \
    jet_static T array_top(T)(array_t(T) * self) {                             \
        return self->used ? self->ref[self->used - 1] : 0;                     \
    }                                                                          \
    jet_static bool array_empty(T)(array_t(T) * self) {                        \
        return self->used == 0;                                                \
    }

MAKE_array_t(ptr_t);
MAKE_array_t(uint32_t);
MAKE_array_t(real64_t);

// MAKE_array_t(uint32_t);
// MAKE_array_t(uint64_t);
// MAKE_array_t(int64_t);
// MAKE_array_t(int32_t);
// MAKE_array_t(Number);
// MAKE_array_t(float);
// make array for strings etc later

// array_top(T) is only defined for value types to keep the number
// of instantiations (of the "template" array_t) down. So void* represents
// object ptrs of all types. Cast them when you need to deref or do ->
// etc. self is used to get a void* as a T (usually a SomeType*)

// TODO: Staticarray_t type with size and array, Staticarray_t2D/3Detc.
// since self is not templated, it's your job to send items of the
// right size, or face the music
// ASSUMING SIZE IS 8. THAT MEANS NO FLOAT OR UINT32, only sizeof(void*)
// #define array_concatCarray(T, array_t, arr, count) \
//     array_concat_carray_(array_t, arr, count * sizeof(T))
// TODO: the compiler should optimise away calls to concat if the
// original arrays can be used one after the other. e.g. concat two
// arrays then print it can be done by simply printing first then
// second, no memcpy involved.
// #define array_concatarray(T, s1, s2) \
//     array_concatarray_(s1, s2, sizeof(T))