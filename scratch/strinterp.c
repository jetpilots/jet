#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

// assuming the compiler has generated a format string and list of args
// if you want a new heap string, call
// strinterp_h(int size, const char* fmt, ...)
// if you want a stack string, call
// strinterp_s(int size, char*buf, const char*fmt,...)
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
char* strinterp_h(int size, const char* fmt, ...) {
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

    char* buf = malloc(size); // TODO: use jet allocator
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

/// Creates a string on the (caller's) stack frame with the given size, using
/// the format string and provided arguments. Follows the usual printf style. In
/// contrast to `strinterp_h`, the buffer cannot be resized, so your guess is
/// crucial. If you guess too low, the string will be truncated. If size is not
/// a constant expression, you may end up with a compilation error (or C99 VLA).
#define strinterp_s(size, fmt, ...)                                            \
    __strinterp__s(size, (char[size]) {}, fmt, __VA_ARGS__)

char* __strinterp__s(int size, char* buf, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int l = vsnprintf(buf, size, fmt, args);
    va_end(args);
    if (l > size) printf("*** %s: size %d, needed %d\n", __func__, size, l);
    return buf;
}

int main() {
    char* fmt = "%s is the name of this city with %d people\n";
    char* val = strinterp_h(41, fmt, "zurche", 300500);
    // char* v = gets("buf:");
    puts(val);
    puts(strinterp_s(48, fmt, "rew", 4323));
}