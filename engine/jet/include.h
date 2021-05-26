#ifndef NDEBUG
#define assert(e)                                                          \
  ((void)((e) ? ((void)0) : __assert(#e, __FILE__, __LINE__, __func__)))
#define __assert(e, file, line, func)                                      \
  ((void)printf("%s:%d: assertion failed: %s\n", file, line, func, e),     \
      abort())
#else
#define assert(e)
#endif

void qsort(void* __base, size_t __nel, size_t __width,
    int (*__compar)(const void*, const void*));
