#include "jet_base.h"
#include "jet_clock.h"

#define N 1000
int main() {
    SpVector sp[1] = {};
    SpVector sp2[1] = {};

    SpVector_init(sp, 6, 20);
    SpVector_set(sp, (UInt32[]) { 3, 5, 6, 11, 13, 16 },
        (Real64[]) { 12, 15, 23.5, 456.22, 11.2, 100.12 });
    SpVector_print(sp);
    SpVector_print0(sp);

    SpVector_init(sp2, 5, 20);
    SpVector_set(sp2, (UInt32[]) { 3, 5, 7, 13, 16 },
        (Real64[]) { 12, 15, 23.5, 456., 100.12 });
    SpVector_print(sp2);
    SpVector_print0(sp2);

    Real64 dot = SpVector_dotproduct(sp2, sp);
    printf("%g\n", dot);
}