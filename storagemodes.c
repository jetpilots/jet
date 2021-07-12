#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct T {
  int a, b, c;
};
typedef struct T T;
struct T_R {
  int a, b, c, r;
};
typedef struct T_R T_R;
#define TMPSZ 512

T* new_T(T* where) {
  T t = { 1, 2, 3 };
  *where = t;
  return where;
}

// #define CONST // var is unchanged or !isVar
// #define PCONST // var is not reassigned

// extern int f();
// extern T* t();

int main() {

  // switch storageMode
  // stack
#define _STACK_VAR_(cst, T, C, x) cst J_(T, C) x##__buf[1];
  // heap
#define _HEAP_VAR_(cst, T, C, x)                                           \
  cst J_(T, C)* const x##__buf = malloc(sizeof(struct J_(T, C)));
  // static. cst only if canEval
#define _STAT_VAR_(cst, T, C, x) static cst J_(T, C) x##__buf[1];
  // refc -- only makes sense on heap
#define _REFC_VAR_(cst, T, C, x)                                           \
  cst J(T, C)* const x##__buf = malloc(sizeof(struct J_(T, C)));
// mixed: strings only
#define _MIXED_VAR_(cst, T, C, x) cst T x##__buf[TMPSZ];
  // }
  // in monolithic mode they will all be defined same as _HEAP_VAR_

  // finally
  // CONST T* PCONST x = x__buf;

// but they won't work esp for const ones -- how will you init them?
// instead check these which will appear on the RHS directly
#define _N_STACK_VAR_(T, C)                                                \
  (T[1]) { }
#define _N_STACK_ARR_(T, C, n, ...)                                        \
  (T[n]) { __VA_ARGS__ }
#define _N_STACK_ARR0_(T, C, n, ...) _N_STACK_ARRN_(T, C, n)
  // heap
#define _N_HEAP_VAR_(T, C) malloc(sizeof(T))
#define _N_HEAP_ARR_(T, C, n, ...)                                         \
  jet_memcpy(malloc(sizeof(T) * n), _N_STACK_ARR_(T, C, n, __VA_ARGS__),   \
    sizeof(T) * n)
#define _N_HEAP_ARR0_(T, C, n, ...)                                        \
  jet_memcpy(calloc(sizeof(T), n), _N_STACK_ARR_(T, C, n, __VA_ARGS__),    \
    sizeof(T) * n)
  // static
#define _N_STAT_VAR_(T, C)                                                 \
  (&T) { } // not possible
  // refc -- only makes sense on heap
#define _N_REFC_VAR_(T, C) malloc(sizeof(struct J_(T##_R, C)));
// mixed: strings only
#define _N_MIXED_VAR_(T, C) cst T x##__buf[TMPSZ];
  // }
  // like so:
  // CONST T* PCONST x = _N_STACK_VAR(T, C);

  const T* const t1 = new_T(_N_STACK_VAR_(T, i));
  T* const t2 = new_T(_N_STACK_VAR_(T, i));
  t2->a = 8;
  t2->b = 9;
  t2->c = 7;
  double* vec = _N_HEAP_ARR_(double, , 4, 88, 66, 44, 22);

  printf("%p %d %d %d\n", t1, t1->a, t1->b, t1->c);
  printf("%p %d %d %d\n", t2, t2->a, t2->b, t2->c);
  printf("%f %f %f %f\n", vec[0], vec[1], vec[2], vec[3]);

  // mixed
  // int req_len = f();
  // if (req_len > TMPSZ) x = malloc(req_len);
  // if (x != x__buf) free(x);

  // T* a;
  // // exprs like this
  // a = t();
  // // become
  // a = t(a);

  // // and
  // a = f(g(x))
  //     // becomes
  //     _tmp1
  //     = a = f(g(x, _tmp1), a)
}