#include <stdio.h>
#include <math.h>
#define countof(x) (sizeof(x) / sizeof(x[0]))

void printarrf(const double* const x, int n) {
    n--;
    printf("[");
    for (int i = 0; i < n; i++) printf("%g, ", x[i]);
    printf("%g]\n", x[n]);
}

void printarrd(int* x, int n) {
    n--;
    printf("[");
    for (int i = 0; i < n; i++) printf("%d, ", x[i]);
    printf("%d]\n", x[n]);
}

// int funca()[3] { return (int[]) { 1, 2, 3 }; }

// struct SA {int count;double x[6];};
int main() {
    double m = 3.2;
    const double x[] = { 4, 5, 6, 7, 4 + sin(m), cos(1.5) };
    // struct SA sx = { 4, {5, 6, 7, 4 + sin(m), cos(1.5)} };
    double xnew[countof(x)];
    for (int i = 0; i < countof(x); i++) xnew[i] = x[i] / 2.0;
    // double* xp = x;
    printarrf(x, countof(x));
    printarrf(xnew, countof(x));
    printarrd((int[]) { 3, 4, 6 }, 3);
}

// var x as Number[] = [   3, 4, 5, sin(x) ]
// -----------------------------------------------
// Number _1[]  = { 3, 4, 5, sin(x) };
// Array_initWith_cArray(Number)(x, _1, countof(_1));

// x = [ 1, 2,    cos(x) ]
// -----------------------------------------------
// Number _2[]    = { 1, 2, cos(x) };
// Array_initWith_cArray(Number)(_x, countof(_x));
