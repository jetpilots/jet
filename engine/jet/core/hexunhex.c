#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: write many more string ops like this: inplaceable

// outbuf must have 2*inlen+1 chars. in and out can alias, then it works in
// place. (this function canInplace).
static void hex(const void* in, char* out, size_t inlen) {
  static const char* const dec2char = "0123456789abcdef";
  const char* inp = in + inlen - 1;
  const char* const instart = in;
  out += 2 * inlen;
  *out-- = '\0';
  for (; inp >= instart; inp--) {
    *out-- = dec2char[*inp & 0x0F];
    *out-- = dec2char[(*inp & 0xF0) >> 4];
  }
}

// by default all strings should be generated as CString, but when you need
// a String you can make one by wrapping the CString
#define STRST(s) ((String) { .ref = s, .size = strlen(s) }) // maybe
// or else just let funcs have 2 args for a string, a len arg following each
// str then pass the len
#define STRARG(s) (s), strlen(s)

// `outbuf` must have `inlen/2 + 1` chars, `inlen` must be even
// inp and out can alias, then it works in place. (function canInplace)
static void unhex(const char* inp, char* out, size_t inlen) {
  static const char char2dec[256] = { //
    ['0'] = 0x00,
    ['1'] = 0x01,
    ['2'] = 0x02,
    ['3'] = 0x03,
    ['4'] = 0x04,
    ['5'] = 0x05,
    ['6'] = 0x06,
    ['7'] = 0x07,
    ['8'] = 0x08,
    ['9'] = 0x09,
    ['a'] = 0x0a,
    ['b'] = 0x0b,
    ['c'] = 0x0c,
    ['d'] = 0x0d,
    ['e'] = 0x0e,
    ['f'] = 0x0f,
    ['A'] = 0x0A,
    ['B'] = 0x0B,
    ['C'] = 0x0C,
    ['D'] = 0x0D,
    ['E'] = 0x0E,
    ['F'] = 0x0F
  };

  // const char* inp = in;
  const char* const inend = inp + inlen;
  for (; inp < inend; inp += 2) {
    *out++ = (char2dec[inp[0]] << 4) + char2dec[inp[1]];
  }
  *out = '\0';
}

// trying out gcc computed gotos
size_t strlen(const char* str) {
  const char* orig = str;
  while (*str) str++;
  return str - orig;
  size_t sz = 0;
  static void* addrs[2] = { &&ret, &&loop };
  int c;
loop:
  c = !!*str++;
  sz += c;
  goto* addrs[c];
ret:
  return sz;
}

#include "../modules/clock.h"

int main() {
  // this is a mutable string on the stack!!! well fixed size but ok
  char f[] = "what"; // a nice day? yeah. this is on the stack, mutable";
  static char sf[] = "this is in .bss, mutable! fixed size still";
  const char* x
      = "this string goes in .data, and its a bad idea to mutate it";
  char buf[256];
  hex(f, buf, sizeof(f) - 1);
  // can work in place!
  printf("%s\n%s\n", f, buf);
  unhex(buf, f, CString_length(buf));
  unhex(buf, buf, CString_length(buf));

  printf("%s\n", buf);
  hex(buf, buf, CString_length(buf));
  printf("%s\n", buf);
  printf("%zu\n", CString_length(f));

  char* fs = malloc(256 * 1024 * 1024);
  memset_pattern16(fs, "0123456789abcdef", 256 * 1024 * 1024);
  fs[256 * 1024 * 1024 - 1] = '\0';
  fs[256 * 1024 * 1024 - 2] = '\0';

  size_t l = CString_length(fs);
  JET_TIMEIT(unhex(fs, fs, l));
  JET_TIMEIT(hex(fs, fs, l / 2));
}

// char b = (*inp & 0xF0) >> 4;
// *out++ = (b <= 9) ? b + '0' : (b - 10) + 'A';
// b = *inp & 0x0F;
// *out++ = (b <= 9) ? b + '0' : (b - 10) + 'A';

// if everything is 8-byte aligned and 8-B padded (even strings in Jet btw)
// you can just unroll this here 4x. also the one below

// char n1, n2;
// char c = *inp;
// n1 = hex2dec[c];
// if (c >= '0' && c <= '9')
//     n1 = c - '0';
// else if (c >= 'A' && c <= 'F')
//     n1 = c - 'A' + 10;
// else if (c >= 'a' && c <= 'f')
//     n1 = c - 'a' + 10;
// else
//     n1 = '\0';

//         c = *++inp;
// n2=hex2dec[c];
// if (c >= '0' && c <= '9')
//     n2 = c - '0';
// else if (c >= 'A' && c <= 'F')
//     n2 = c - 'A' + 10;
// else if (c >= 'a' && c <= 'f')
//     n2 = c - 'a' + 10;
// else
//     n2 = '\0';

// ++inp;

// *out++ = n1 << 4 + n2;