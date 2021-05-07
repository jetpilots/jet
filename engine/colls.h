#define SArray(T)                                                          \
  struct {                                                                 \
    T* start;                                                              \
    unsigned count;                                                        \
  }
#define SArray_for(T, v, a)                                                \
  for (T* _i_##v = a.start, *_e_##v = a.start + a.count; _i_##v < _e_##v;  \
       _i_##v++)
#define SArray_sizeof(T, a) (a.count * sizeof(T))
#define SArray_new(n, T, ...)                                              \
  { .start = (T[n]) { __VA_ARGS__ }, .count = n }

#define STensor(T, N)                                                      \
  struct {                                                                 \
    SArray(T);                                                             \
    unsigned shape[N];                                                     \
  }
#define STensor_for(T, v, a) SArray_for(T, v, a)
#define STensor_sizeof(T, a) SArray_sizeof(T, a)
#define STensor_new(n, T, N, ...)                                          \
  { .start = (T[n]) { __VA_ARGS__ }, .count = n }

//---

#define Array(T)                                                           \
  struct {                                                                 \
    T* start;                                                              \
    unsigned count, free;                                                  \
  }
#define Array_foreach(T, v, a)                                             \
  if ((a).count)                                                           \
    for (const T *_i_##v = (a).start, v = *_i_##v,                         \
                 const *const _e_##v = (a).start + (a).count;              \
         _i_##v < _e_##v; v = *++_i_##v)

#define Array_mforeach(T, v, a)                                            \
  if ((a).count)                                                           \
    for (T* _i_##v = (a).start, v = *_i_##v,                               \
            const* const _e_##v = (a).start + (a).count;                   \
         _i_##v < _e_##v; v = *++_i_##v)

#define Array_slots(arr) ((arr).count + (arr).free)
#define Array_sizeof(T, arr) (Array_slots(arr) * sizeof(T))
#define Array_new(n, T, ...)                                               \
  { .start = memdup((T[n]) { __VA_ARGS__ }, (n) * sizeof(T)), .count = (n) }
#define Array_newFromSArray(T, sarr)                                       \
  {                                                                        \
    .start = memdup((sarr).start, (sarr).count * sizeof(T)),               \
    .count = (sarr).count                                                  \
  }

//---

#define Tensor(T, N)                                                       \
  struct {                                                                 \
    Array(T);                                                              \
    unsigned shape[N];                                                     \
  }

// If you have Tensor for any T, then basic tensor ops should work for any T
#define Tensor_reshape(T, N, t, newShape)                                  \
  (!memcmp(t.shape, newShape, sizeof(unsigned) * N)) ? ;:;

void* memdup(const void* const buf, unsigned nbytes) {
  void* ret = malloc(nbytes);
  memcpy(ret, buf, nbytes);
  return ret;
}

int main() {
  Array(int) arr = Array_new(5, int, 9, 7, 8, 6, 5);
  SArray(int) sarr = SArray_new(5, int, 9, 7, 8, 6, 5);
  Array(int) arr2 = Array_newFromSArray(int, sarr);
}
