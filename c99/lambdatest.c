typedef struct lam1 {
  int (*const fn)(struct lam1* l, int);
  int cap1, cap2; // can't be inside fn coz they need to be freed
} lam1;

extern int ext;

int anon1(lam1* l, int x) { return l->cap1 * l->cap2 * ext * x; }

lam1 newlam1(int cap1, int cap2) {
  return (lam1) { .cap1 = cap1, .cap2 = cap2, .fn = anon1 };
}

// void lam1_drop(lam1*l){
//   int_drop(l->cap1);
//   ...
// }

// extern
int smain() {
  int x = 89, f = 7009;
  lam1 lam = newlam1(x, f);
  int ret = lam.fn(&lam, 11);
  return ret;
}

typedef enum ExprKind { tkPlus, tkMinus, tkMul, tkDiv, tkPow } ExprKind;

extern double pow(double a, double b);
double plus(double a, double b) { return a + b; }
double minus(double a, double b) { return a - b; }
double mul(double a, double b) { return a * b; }
double div(double a, double b) { return a / b; }
double mpow(double a, double n) { return pow(a, n); }

double eval_sw(double a, double b, ExprKind what) {
  switch (what) {
  case tkDiv: return div(a, b);
  case tkPlus: return plus(a, b);
  case tkMinus: return minus(a, b);
  case tkMul: return mul(a, b);
  case tkPow: return mpow(a, b);
  }
}

double (*const fns[])(double a, double b) = { plus, minus, mul, div, mpow };

double eval_fp(double a, double b, ExprKind what) {
  return fns[what](a, b);
}

extern double smth_opt(double a, double opt);
double smth(double a) { return smth_opt(a, 22); }
