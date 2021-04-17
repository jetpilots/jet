typedef char* cstr_t;

jet_static cstr_t cstr_palloc(size_t len) {
    return Pool_alloc(sPool, roundm8(len + 1));
}

jet_static cstr_t cstr_malloc(size_t len) { return malloc(roundm8(len + 1)); }

jet_static cstr_t cstr_pndup(const cstr_t str, size_t len) {
    cstr_t ret = cstr_palloc(len);
    memcpy(ret, str, len); // sPool uses calloc, so no need to zero last
    return ret;
}

jet_static cstr_t cstr_pclone(const cstr_t str) {
    return cstr_pndup(str, strlen(str));
}

jet_static cstr_t cstr_ndup(const char* str, size_t len) {
    cstr_t ret = cstr_malloc(len);
    memcpy(ret, str, len); // sPool uses calloc, so no need to zero last
    ret[len] = 0;
    return ret;
}

jet_static cstr_t cstr_clone(const char* str) {
    return cstr_ndup(str, strlen(str));
}

jet_static size_t cstr_length(const char* str) { return strlen(str); }
// jet_static cstr_t cstr_clone(cstr_t str) { return pstrdup(str); }
// jet_static cstr_t cstr_sysClone(cstr_t str) { return strdup(str); }
jet_static cstr_t cstr_indexOf(cstr_t str, char c) { return strchr(str, c); }
jet_static size_t cstr_indexOfAny(cstr_t str, cstr_t chars) {
    return strcspn(str, chars);
}
jet_static size_t cstr_matchAny(cstr_t str, cstr_t chars) {
    return strspn(str, chars);
}
jet_static cstr_t cstr_findString(cstr_t str, cstr_t substr) {
    return strstr(str, substr);
}
jet_static cstr_t cstr_findChars(cstr_t str, cstr_t chars) {
    return strpbrk(str, chars);
}

jet_static double cstr_toDouble(cstr_t str) { return strtod(str, NULL); }
jet_static long cstr_toLong(cstr_t str) { return strtol(str, NULL, 0); }
jet_static long double cstr_toLongDouble(cstr_t str) {
    return strtold(str, NULL);
}
jet_static long long cstr_toLongLong(cstr_t str) {
    return strtoll(str, NULL, 0);
}
jet_static unsigned long long cstr_toULongLong(cstr_t str) {
    return strtoull(str, NULL, 0);
}

jet_static cstr_t cstr_noext(cstr_t str) {
    const size_t len = strlen(str);
    cstr_t s = str; // pstrndup(str, len);
    cstr_t sc = s + len;
    while (sc > s && *sc != '.') sc--;
    if (sc >= s) *sc = '\0';
    return s;
}

jet_static cstr_t cstr_base(cstr_t str, char sep, size_t slen) {
    if (!slen) return str;
    cstr_t s = str;
    cstr_t sc = s + slen;
    while (sc > s && sc[-1] != sep) sc--;
    if (sc >= s) s = sc;
    return s;
}

jet_static cstr_t cstr_dir_ip(cstr_t str, size_t len) {
    cstr_t s = str;
    cstr_t sc = s + len;
    while (sc > s && *sc != '/') sc--;
    if (sc >= s) *sc = 0;
    return s;
}

jet_static cstr_t cstr_toupper_ip(cstr_t str) {
    cstr_t s = str; // pstrdup(str);
    cstr_t sc = s - 1;
    while (*++sc)
        if (*sc >= 'a' && *sc <= 'z') *sc -= 32;
    return s;
}

// in place
jet_static void cstr_tr_ip(
    cstr_t str, const char oldc, const char newc, const size_t length) {
    cstr_t sc = str - 1;
    cstr_t end = length ? str + length : (cstr_t)0xFFFFFFFFFFFFFFFF;
    while (*++sc && sc < end)
        if (*sc == oldc) *sc = newc;
}

jet_static cstr_t cstr_tr(cstr_t str, const char oldc, const char newc) {
    size_t len = strlen(str);
    cstr_t s = str; // pstrndup(str, len);
    cstr_tr_ip(s, oldc, newc, len);
    return s;
}

jet_static cstr_t cstr_nthField(cstr_t str, int len, char sep, int nth) {
    return NULL;
}

jet_static int cstr_countFields(cstr_t str, int len, char sep) { return 0; }

// caller sends target as stack array or NULL
jet_static cstr_t* cstr_getAllOccurences(
    cstr_t str, int len, char sep, int* count) {
    // result will be malloced & realloced
    return 0;
}

jet_static int cstr_getSomeOccurences(
    cstr_t str, int len, char sep, cstr_t* result, int limit) {
    // result buf is expected from caller
    return 0;
}

#include "jet/_ext/strcasecmp.h"

#define cstr_endsWith(str, lenstr, suffix, lensuffix)                          \
    !strncmp(str + lenstr - lensuffix, suffix, lensuffix)

#define cstr_startsWith(str, prefix, lenprefix) !strncmp(str, prefix, lenprefix)

jet_static ulong cstr_levenstein(char* s1, char* s2, ulong s1len, ulong s2len) {
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
// strinterp_h(int size, const cstr_t fmt, ...)
// if you want a stack string, call
// strinterp_s(int size, cstringbuf, const cstringfmt,...)
// for buf, supply an array literal (C99): (char[n]){}
// where n is the known size (also passed in size).
// constructing the buf this way fills it with zeros, which is a useless cost,
// but it avoids the alloc, giving you a buffer in the caller's frame.

// if you just want to print the string, just use printf or fprintf. You don't
// need any wrappers if you can generate the format string and you have the
// arguments.

/// Create a string on the heap using the format string and provided arguments.
/// Follows the usual printf format. Size can be provided if known, if you guess
/// too low, the buffer will incur a resize. It's better to set size=0 if you
/// don't have a better guess at all.
jet_static cstr_t cstr_heap_interp(int size, const cstr_t fmt, ...) {
    va_list args;

    cstr_t buf = malloc(size);
    va_start(args, fmt);
    int l = vsnprintf(buf, size, fmt, args);
    va_end(args);

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

/// Creates a string on the (caller's) stack frame with the given size, using
/// the format string and provided arguments. Follows the usual printf style. In
/// contrast to `strinterp_h`, the buffer cannot be resized, so your guess is
/// crucial. If you guess too low, the string will be truncated. If size is not
/// a constant expression, you may end up with a compilation error (or C99 VLA).
#define cstr_stack_interp(size, fmt, ...)                                      \
    cstr__stack_interp(size, (char[size]) {}, fmt, __VA_ARGS__)

jet_static cstr_t cstr__stack_interp(
    int size, cstr_t buf, const cstr_t fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int l = vsnprintf(buf, size, fmt, args);
    va_end(args);
    if (l > size) printf("*** %s: size %d, needed %d\n", __func__, size, l);
    return buf;
}

jet_static int cstr__test() {
    cstr_t fmt = "%s is the name of this city with %d people\n";
    cstr_t val = strinterp_h(41, fmt, "zurche", 300500);
    // cstr_t v = gets("buf:");
    puts(val);
    puts(strinterp_s(48, fmt, "rew", 4323));
    return 0;
}