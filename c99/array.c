#ifndef _xa

#define _xa(T)                                                             \
  struct {                                                                 \
    int len;                                                               \
    T ref[];                                                               \
  }* restrict

static void* _xa__init(_xa(char) ret, int n) {
  ret->len = 0;
  // ret->cap = n; // you don't need cap if array is fixed size
  return ret;
}
#define _xa_size(T, n) (sizeof(T) * n + 2 * sizeof(int))
#define _xa_hnew(T, n) _xa__init(malloc(_xa_size(T, n)), n)
#define _xa_snew(T, n) _xa__init((char[_xa_size(T, n)]) {}, n)
#define _xa_pnew(T, n, at) _xa__init(at, n) // placement new
#define _xa_len(T, a) ((a)->len)
#define _xa_getv(T, a, i) ((a)->ref[i])
#define _xa_setv(T, a, i, v) ((a)->ref[i] = v)
#define _xa_geta(T, a, i) (&_xa_getv(a, i))
// array is fixed so you are responsible for ensuring safety of push
#define _xa_push(T, a, v) ((a)->ref[(a)->len++] = v)
#define _xa_pop(T, a) ((a)->ref[--(a)->len])
#define _xa_map(T, a, fn)                                                  \
  for (int i = 0; i < (a)->len; i++) fn((a)->ref[i]);
#define _xa_each(T, a, id, code)                                           \
  for (T* __id = (a)->ref, id = *__id; __id < (a)->ref + (a)->len;         \
       id = *__id++)                                                       \
  code
#define _xa_drop(T, a) _xa_map(a, T##_drop)
#define _xa_free(T, a) free(a)

#endif
