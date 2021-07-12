#include "List.hh"

class CString {
  char* str;
  operator char*() { return str; }
  operator void*() { return (void*)str; }

  // operator const void*() { return (const void*)str; }

  public:
  // typedef char* CString;

  CString palloc(size_t len) { return Pool_alloc(sPool, roundm8(len + 1)); }

  CString malloc(size_t len) { return malloc(roundm8(len + 1)); }

  CString pndup(const CString str, size_t len) {
    CString ret = palloc(len);
    jet_mem_copy(
      ret, str, len); // sPool uses calloc, so no need to zero last
    return ret;
  }

  CString pclone(const CString str) { return pndup(str, cstr_len(str)); }

  CString ndup(const CString str, size_t len) {
    CString ret = malloc(len);
    jet_mem_copy(
      ret, str, len); // sPool uses calloc, so no need to zero last
    ret[len] = 0;
    return ret;
  }

  CString clone(const CString str) { return ndup(str, cstr_len(str)); }

  size_t length(CString str) { return cstr_len(str); }
  //   CString  clone(CString str) { return pstrdup(str); }
  //   CString  sysClone(CString str) { return strdup(str); }
  CString indexOf(CString str, char c) { return strchr(str, c); }
  size_t indexOfAny(CString str, CString chars) {
    return strcspn(str, chars);
  }
  size_t matchAny(CString str, CString chars) { return strspn(str, chars); }
  CString findString(CString str, CString substr) {
    return strstr(str, substr);
  }
  CString findChars(CString str, CString chars) {
    return strpbrk(str, chars);
  }

  double toDouble(CString str) { return strtod(str, NULL); }
  long toLong(CString str) { return strtol(str, NULL, 0); }
  long double toLongDouble(CString str) { return strtold(str, NULL); }
  long long toLongLong(CString str) { return strtoll(str, NULL, 0); }
  unsigned long long toULongLong(CString str) {
    return strtoull(str, NULL, 0);
  }

  CString noext(CString str) {
    const size_t len = cstr_len(str);
    CString s = pstrndup(str, len);
    CString sc = s + len;
    while (sc > s && *sc != '.') sc--;
    if (sc >= s) *sc = '\0';
    return s;
  }

  CString base(CString str, char sep, size_t slen) {
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

  CString dir(CString str) {
    const size_t len = cstr_len(str);
    CString s = pstrndup(str, len);
    CString sc = s + len;
    while (sc > s && *sc != '/') sc--;
    if (sc >= s) *sc = '\0';
    return s;
  }

  CString upper(CString str) {
    CString s = pstrdup(str);
    CString sc = s - 1;
    while (*++sc)
      if (*sc >= 'a' && *sc <= 'z') *sc -= 32;
    return s;
  }

  // in place
  void tr_ip(
    CString str, const char oldc, const char newc, const size_t length) {
    CString sc = str - 1;
    CString end = length ? str + length : (CString)0xFFFFFFFFFFFFFFFF;
    while (*++sc && sc < end)
      if (*sc == oldc) *sc = newc;
  }

  CString tr(CString str, const char oldc, const char newc) {
    size_t len = cstr_len(str);
    CString s = pstrndup(str, len);
    tr_ip(s, oldc, newc, len);
    return s;
  }

  CString nthField(CString str, int len, char sep, int nth) { return NULL; }

  int countFields(CString str, int len, char sep) { return 0; }

  // caller sends target as stack array or NULL
  CString* getAllOccurences(CString str, int len, char sep, int* count) {
    // result will be malloced & realloced
    return 0;
  }

  int getSomeOccurences(
    CString str, int len, char sep, CString* result, int limit) {
    // result buf is expected from caller
    return 0;
  }

#include "jet/_ext/strcasecmp.h"

#define endsWith(str, lenstr, suffix, lensuffix)                           \
  !strncmp(str + lenstr - lensuffix, suffix, lensuffix)

#define startsWith(str, prefix, lenprefix) !strncmp(str, prefix, lenprefix)

  ulong leven(char* s1, char* s2, ulong s1len, ulong s2len) {
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
  // strinterp_h(int size, const CString fmt, ...)
  // if you want a stack string, call
  // strinterp_s(int size, CStringbuf, const CStringfmt,...)
  // for buf, supply an array literal (C99): (char[n]){}
  // where n is the known size (also passed in size).
  // constructing the buf this way fills it with zeros, which is a useless
  // cost, but it avoids the alloc, giving you a buffer in the caller's
  // frame.

  // if you just want to print the string, just use printf or fprintf. You
  // don't need any wrappers if you can generate the format string and you
  // have the arguments.

  /// Create a string on the heap using the format string and provided
  /// arguments. Follows the usual printf format. Size can be provided if
  /// known, if you guess too low, the buffer will incur a resize. It's
  /// better to set size=0 if you don't have a better guess at all.
  CString strinterp_h(int size, const CString fmt, ...) {
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
      printf(
        "*** %s: size %d too small, resized to %d\n", __func__, size, l);
    }

    return buf;
  }

/// Creates a string on the (caller's) stack frame with the given size,
/// using the format string and provided arguments. Follows the usual printf
/// style. In contrast to `strinterp_h`, the buffer cannot be resized, so
/// your guess is crucial. If you guess too low, the string will be
/// truncated. If size is not a constant expression, you may end up with a
/// compilation error (or C99 VLA).
#define strinterp_s(size, fmt, ...)                                        \
  __strinterp__s(size, (char[size]) {}, fmt, __VA_ARGS__)

  CString __strinterp__s(int size, CString buf, const CString fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int l = vsnprintf(buf, size, fmt, args);
    va_end(args);
    if (l > size) printf("*** %s: size %d, needed %d\n", __func__, size, l);
    return buf;
  }

  int _test() {
    CString fmt = "%s is the name of this city with %d people\n";
    CString val = strinterp_h(41, fmt, "zurche", 300500);
    // CString v = gets("buf:");
    puts(val);
    puts(strinterp_s(48, fmt, "rew", 4323));
    return 0;
  }
};
