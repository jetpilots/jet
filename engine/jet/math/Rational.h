#include <stdio.h>
#include <math.h>
typedef struct {
    long long num, den;
} Frac;

Frac real2frac(double real) {
    if (0.5 < real && real < 1.0) real = 1.0 / real;
    long long whole = (long long)real;
    Frac ret = { 0, 0 };
    double fract = real - whole;

    double test = fract, tol = 1e-20;
    while (test - tol == test) {
        tol *= 10;
        // printf("%.17f %.17f\n", tol, test);
    }
    // tol *= 10;

    if (fract > 0.5) {
        ret = real2frac(fract);
    } else {
        double den = 1.0 / fract;
        double ffr = den;
        long long num = 1;
        // while (fabs(den - round(den)) > 1e-14) {
        while (fabs(num / round(den) - fract) >= tol) {
            den += ffr;
            num++;
            // printf("%.17f %.17f\n", fract, num / round(den));
            // printf("%.17f %.17f\n", den, round(den));
        }
        ret = (Frac) { num, round(den) };
    }
    ret.num += ret.den * whole;
    return ret;
}

int main() {

    //   long long N = 11;
    // for (  long long i = 1; i < 5; i++) {
    //     for (  long long j = 1; j < 5; j++) {
    //         printf("%d/%d = %.2f\n", i, j, i * 1.0 / j);
    //     }
    //     puts("--");
    // }
    // long long vn = 1234651, vd = 847563471;
    long long vn = 22, vd = 7;
    // double v = 0.00145670624353748251; //
    double v = 4.99; // 4159265359;
    Frac f = real2frac(v);
    printf("%.17g = %lld/%lld (%.17g)\n", v, f.num, f.den, f.num * 1.0 / f.den);
    printf("%.17g = %lld/%lld (%.17g)\n", v, vn, vd, vn * 1.0 / vd);
}