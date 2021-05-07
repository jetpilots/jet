static const int sizes[] = { 0, 16, 32, 64, 128, 256, 512, 768, 1024, 1280,
  1536, 2048, 2560, 3072, 3584, 4096 };
typedef unsigned long long u64;
typedef struct SString {
  u64 ptr : 48, len : 12, cap : 4;
} SString;

char* sstr_ptr(SString s) { return (char*)s.ptr; }
int sstr_len(SString s) { return s.len; }
SString sstr_crefn(char* str, int len) {
  return (SString) { .ptr = (u64)str, .len = len };
}
SString sstr_cref(char* str) { return sstr_crefn(str, strlen(str)); }
void sstr_free(SString* s) {
  if (s->cap) free(s->ptr);
  *s = (SString) {};
}
SString sstr_new() {
  return (SString) { //
    .ptr = (u64)malloc(sizes[1]),
    .cap = 1
  };
}
void sstr_appendcn(SString* s, char* str, int len) {
  if (sizes[s->cap] <= len + s->len) sstr_reserve(len);
  memcpy(s->ptr + s->len, str, len + 1);
  s->len += len;
}
void sstr_appendc(SString* s, char* str, int len) {
  sstr_appendcn(s, str, strlen(str));
}
void sstr_append(SString* s, SString s2) {
  sstr_appendc(s, s2.ptr, s2.len);
}
void sstr_grow(SString* s) {
  s->cap++;
  s->ptr = (u64)realloc(s->ptr, sizes[s->cap]);
}

static int fits(int size) {
  for (int i = 1; i < 16; i++)
    if (sizes[i] >= size) return i;
  return 0;
}
SString sstr_cdup(char* str, int len) {
  int idx = fits(len + 1);
  return (SString) { //
    .ptr = (u64)memcpy(malloc(sizes[idx]), str, len + 1),
    .len = len,
    .cap = idx
  };
}
SString sstr_dup(SString s) {
  // if (s.cap)
  //     return (SString) { //
  //         .ptr = (u64)memcpy(malloc(sizes[s.cap]), s.ptr, s.len + 1),
  //         .len = s.len,
  //         .cap = s.cap
  //     };
  // else
  return sstr_cdup(s.ptr, s.len);
}