
monostatic CString cstr_palloc(size_t len) {
  return Pool_alloc(sPool, roundm8(len + 1));
}

monostatic CString cstr_malloc(size_t len) {
  return malloc(roundm8(len + 1));
}

monostatic bool cstr_eq(const CString a, const CString b) {
  return a == b || !strcmp(a, b);
}
monostatic bool cstr_eqi(const CString a, const CString b) {
  return a == b || !strcasecmp(a, b);
}

monostatic CString cstr_pndup(const CString str, size_t len) {
  CString ret = cstr_palloc(len);
  memcpy(ret, str, len); // sPool uses calloc, so no need to zero last
  return ret;
}

monostatic CString cstr_pclone(const CString str) {
  return cstr_pndup(str, strlen(str));
}

monostatic CString cstr_ndup(const char* str, size_t len) {
  CString ret = cstr_malloc(len);
  memcpy(ret, str, len); // sPool uses calloc, so no need to zero last
  ret[len] = 0;
  return ret;
}

monostatic CString cstr_clone(const char* str) {
  return cstr_ndup(str, strlen(str));
}

#define CString_len cstr_length
monostatic size_t cstr_length(const char* str) { return strlen(str); }
// monostatic CString cstr_clone(CString str) { return pstrdup(str); }
// monostatic CString cstr_sysClone(CString str) { return strdup(str); }
monostatic CString cstr_indexOf(CString str, char c) {
  return strchr(str, c);
}
monostatic size_t cstr_indexOfAny(CString str, CString chars) {
  return strcspn(str, chars);
}
monostatic size_t cstr_matchAny(CString str, CString chars) {
  return strspn(str, chars);
}
monostatic CString cstr_findString(CString str, CString substr) {
  return strstr(str, substr);
}
monostatic CString cstr_findChars(CString str, CString chars) {
  return strpbrk(str, chars);
}

monostatic double cstr_toDouble(CString str) { return strtod(str, NULL); }
monostatic long cstr_toLong(CString str) { return strtol(str, NULL, 0); }
monostatic long double cstr_toLongDouble(CString str) {
  return strtold(str, NULL);
}
monostatic long long cstr_toLongLong(CString str) {
  return strtoll(str, NULL, 0);
}
monostatic unsigned long long cstr_toULongLong(CString str) {
  return strtoull(str, NULL, 0);
}

monostatic CString cstr_noext_ip(CString str) {
  const size_t len = strlen(str);
  CString s = str; // pstrndup(str, len);
  CString sc = s + len;
  while (sc > s && *sc != '.') sc--;
  if (sc >= s) *sc = '\0';
  return s;
}

monostatic CString cstr_base(CString str, char sep, size_t slen) {
  if (!slen)
    return str; // you should pass in the len. len 0 is actually
                // valid since basename for 'mod' is 'mod' itself,
                // and self would have caused a call to strlen
                // below. so len 0 now means really just return what
                // came in.
  CString s = str;
  CString sc = s + slen;
  while (sc > s && sc[-1] != sep) sc--;
  if (sc >= s) s = sc;
  return s;
}

monostatic CString cstr_dir_ip(CString str) {
  const size_t len = strlen(str);
  CString s = str; // pstrndup(str, len);
  CString sc = s + len;
  while (sc > s && *sc != '/') sc--;
  if (*sc == '/') ++sc;
  *sc = '\0';
  return s;
}

monostatic CString cstr_upper_ip(CString str) {
  CString s = str; // pstrdup(str);
  CString sc = s - 1;
  while (*++sc)
    if (*sc >= 'a' && *sc <= 'z') *sc -= 32;
  return s;
}

// in place
monostatic void cstr_tr_ip_len(
    CString str, const char oldc, const char newc, const size_t length) {
  CString sc = str - 1;
  CString end = length ? str + length : (CString)0xFFFFFFFFFFFFFFFF;
  while (*++sc && sc < end)
    if (*sc == oldc) *sc = newc;
}

monostatic CString cstr_tr_ip(
    CString str, const char oldc, const char newc) {
  size_t len = strlen(str);
  CString s = str; // pstrndup(str, len);
  cstr_tr_ip_len(s, oldc, newc, len);
  return s;
}

monostatic CString cstr_nthField(CString str, int len, char sep, int nth) {
  return NULL;
}

monostatic int cstr_countFields(CString str, int len, char sep) {
  return 0;
}

// caller sends target as stack array or NULL
monostatic CString* cstr_getAllOccurences(
    CString str, int len, char sep, int* count) {
  // result will be malloced & realloced
  return 0;
}

monostatic int cstr_getSomeOccurences(
    CString str, int len, char sep, CString* result, int limit) {
  // result buf is expected from caller
  return 0;
}

// #include "jet/_ext/strcasecmp.h"

#define cstr_endsWith(str, lenstr, suffix, lensuffix)                      \
  !strncmp(str + lenstr - lensuffix, suffix, lensuffix)

#define cstr_startsWith(str, prefix, lenprefix)                            \
  !strncmp(str, prefix, lenprefix)

monostatic ulong leven(char* s1, char* s2, ulong s1len, ulong s2len) {
#define NCBUF 64
  ulong x, y, lastdiag, olddiag;
  ulong _c[NCBUF], *column = _c;

  // *** this should be based on max or min length, not s1len
  if (s1len > NCBUF) column = malloc(s1len);
  for (y = 1; y <= s1len; y++) column[y] = y;
  for (x = 1; x <= s2len; x++) {
    column[0] = x;
    for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
      olddiag = column[y];
      column[y] = min3ul(column[y] + 1, column[y - 1] + 1,
          lastdiag + (s1[y - 1] == s2[x - 1] ? 0 : 1));
      lastdiag = olddiag;
    }
  }
  if (s1len > NCBUF) free(column);
  return column[s1len];
#undef NCBUF
}

// #include <stdio.h>
// #include <string.h>
// #include <stdlib.h>
// #include <stdarg.h>

// assuming the compiler has generated a format string and list of args
// if you want a new heap string, call
// cstr_interp_h(int size, const CString fmt, ...)
// if you want a stack string, call
// cstr_interp_s(int size, CStringbuf, const CStringfmt,...)
// for buf, supply an array literal (C99): (char[n]){}
// where n is the known size (also passed in size).
// constructing the buf this way fills it with zeros, which is a useless
// cost, but it avoids the alloc, giving you a buffer in the caller's frame.

// if you just want to print the string, just use printf or fprintf. You
// don't need any wrappers if you can generate the format string and you
// have the arguments.

/// Create a string on the heap using the format string and provided
/// arguments. Follows the usual printf format. Size can be provided if
/// known, if you guess too low, the buffer will incur a resize. It's better
/// to set size=0 if you don't have a better guess at all.
monostatic CString cstr_interp_h(int size, const CString fmt, ...) {
  va_list args;

  // TODO: mark branch unlikely
  // Seems like this is superflous, since the lowball case will be handled
  // later anyway.
  // if (!size) {
  //     va_start(args, fmt);
  //     char tmp[8];
  //     size = vsnprintf(tmp, 8, fmt, args);
  //     va_end(args);
  //     printf("no size, calculated %d\n", size);
  // }

  CString buf = malloc(size); // TODO: use jet allocator
  va_start(args, fmt);
  int l = vsnprintf(buf, size, fmt, args);
  va_end(args);

  // TODO: mark branch unlikely
  if (l > size) {
    l++;
    buf = realloc(buf, l);
    va_start(args, fmt);
    vsnprintf(buf, l, fmt, args);
    va_end(args);
    printf("*** %s: size %d too small, resized to %d\n", __func__, size, l);
  }

  return buf;
}

/// Creates a string on the (caller's) stack frame with the given size,
/// using the format string and provided arguments. Follows the usual printf
/// style. In contrast to `cstr_interp_h`, the buffer cannot be resized, so
/// your guess is crucial. If you guess too low, the string will be
/// truncated. If size is not a constant expression, you may end up with a
/// compilation error (or C99 VLA).
#define cstr_interp_s(size, fmt, ...)                                      \
  __cstr_interp__s(size, (char[size]) {}, fmt, __VA_ARGS__)

monostatic CString __cstr_interp__s(
    int size, CString buf, const CString fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int l = vsnprintf(buf, size - 2, fmt, args);
  va_end(args);
  buf[size - 1] = 0;
  if (l > size) printf("*** %s: size %d, needed %d\n", __func__, size, l);
  return buf;
}

monostatic int cstr__test() {
  CString fmt = "%s is the name of this city with %d people\n";
  CString val = cstr_interp_h(41, fmt, "zurche", 300500);
  // CString v = gets("buf:");
  puts(val);
  puts(cstr_interp_s(48, fmt, "rew", 4323));
  return 0;
}