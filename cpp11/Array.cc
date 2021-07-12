#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>

// ignores upper address space (with bit 47 set) so be careful
template <class T>
struct SmallArray {
  // TODO: swap for big endians
  uintptr_t ptr : 48, len : 12, cap : 4;
  // len can go up to 4095.
  // cap can go up to 15, but only up to 12 is useful. The allocated size is
  // 2^cap.
  SmallArray()
      : ptr(0)
      , len(0)
      , cap(0) { }
  operator T*() { return (T*)ptr; }
  // TODO: add unreachable if (cap>12)
  void grow2x() { ptr = (uintptr_t)realloc((void*)ptr, 1 << ++cap); }
  T& operator[](size_t idx) {
    T* ref = (T*)ptr;
    return ref[idx];
  }
  ~SmallArray() {
    if (ptr) free((void*)ptr);
  }
  SmallArray<T>& push(T item) {
    if (len >= cap) grow2x();
    T* ref = (T*)ptr;
    ref[len++] = item;
    return *this;
  }
};

// struct SmallString : public SmallArray<char> { };

struct SmallStringRef {
  SmallStringRef()
      : ptr(0)
      , len(0) {};
  SmallStringRef(const char* orig) { _setref(orig); };
  void operator=(const char* newref) { _setref(newref); }
  operator char*() { return (char*)ptr; }
  char& operator[](size_t idx) { return ((char*)ptr)[idx]; }
  uintptr_t ptr : 48, len : 16;
  int cmp(const char* other) { return strncmp((char*)ptr, other, len); }

  private:
  void _setref(const char* ref) {
    ptr = (uintptr_t)ref;
    size_t l = cstr_len(ref);
    len = l <= 0xffff ? l : 0;
  }
};

static const size_t sizeof_SmallArray = sizeof(SmallArray<int>);
static const size_t sizeof_SmallStringRef = sizeof(SmallStringRef);

int maikn() {
  SmallArray<int> sa;
  sa.push(5).push(8).push(9);
  for (int i = 0; i < sa.len; i++) printf("%d\n", sa[i]);
  SmallStringRef s = "this one is nice";
  printf("%d %s\n", s.len, (char*)s.ptr);
  return 0;
}
