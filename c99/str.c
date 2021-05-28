#include <stdio.h>

typedef char* CStr;
typedef const char* CStrRo; // read-only but can reassign
typedef const char CStrX[]; // const, not reassigned, never touched
typedef char* FStr; // has len in first 4B
typedef const char* FStrRo;
typedef const char FStrX[];

#define strn(n)                                                            \
  struct {                                                                 \
    const int l;                                                           \
    char s[n];                                                             \
  }

typedef struct {
  char* const str;
  const unsigned int len;
} str_fix; // non-resizable; passed & stored by value.

typedef struct {
  char* str;
  unsigned int len, cap;
} str_dyn; // resizable; passed & stored by reference.

// "small versions"
typedef struct {
  const unsigned long long ptr : 48, len : 16;
} sstr_fix; // non-resizable; passed & stored by value. 65KB max

typedef struct {
  unsigned long long ptr : 48, len : 12, cap : 4;
} sstr_dyn; // resizable; passed & stored by reference. 4KB max

int strl(char s[]) { return *(int*)(s - sizeof(int)); }
char* St(char* lit, int n) {
  *(int*)(lit - sizeof(int)) = n;
  return lit;
}
#define St(x) St((char[]) { x }, sizeof(x "") - 1)
// #define STR(x) (str) { .l = sizeof(x) - 1, .s = x };
#define STRLIT(x, sl) ((char[]) { sl x } + sizeof(int))
// ^ use it as STRLIT("Quick Brown Fox", "\x0F\x00\x00\x00")
// ^ but this cant be static

void str_print(char s[]) { fwrite(s, strl(s), 1, stdout); }

void printhexf(unsigned int i) {
  union {
    struct {
      unsigned char a, b, c, d;
    };
    unsigned int i;
  } u = { .i = i };
  printf("\\x%02x\\x%02x\\x%02x\\x%02x\n", u.a, u.b, u.c, u.d);
}

int main(int argc, char* argv[]) {
  // static str s = STR("Quick Brown Foxy");
  // str_print(s);
  char ss[] = "Nojuifyvt dyko";
  char* const p = (char[]) { "\x3\xe\0\0"
                             "POK" }
      + 4;
  char* p2 = (char[]) { "KOIUHUYG" };
  printf("%p %p %s %s %d\n", p, p2, p, p2, strl(p));
  printhexf(3587);

  char* st = St("wpwbangu");
  printf("%s %d\n", st, strl(st));

  CStr mx = (char[]) { "whatever" };
  puts(mx);
  mx[3] = '|';
  puts(mx);
  mx = "78";
  puts(mx);
}