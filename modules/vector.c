#include "jet_base.h"
#include "jet_clock.h"

#define N 1000
int main() {
    srand(time(0));
    Vector m0[1] = {}, mr[1] = {};
    jet_Vector_resize(m0, N);
    jet_Vector_resize(mr, N);
    Vector ls[1] = {};
    // Real64LinRange r = { 1, 8, 0.12837453 };
    // Vector_linspace_step(1, 8, 0.1231232154, ls);
    // Vector_log10space(1, 10000, 5, ls);
    Vector_logspace(1, 100, 18, ls);
    Vector_print_prec(ls, 6);
    return 0;
    Vector_fillzero(m0);
    if (mr->used < 24) Vector_print(mr);
    if (m0->used < 24) Vector_print(m0);
    for_to(il, 10) {
        Vector_fillrandoms(mr);
        jet_clock_Time t0 = jet_clock_getTime();
        Real64 min0 = Vector_min(m0);
        jet_clock_Time t1 = jet_clock_getTime();
        Real64 minr = Vector_min(mr);
        jet_clock_Time t2 = jet_clock_getTime();
        Real64 min0_ = Vector_minbf(m0);
        jet_clock_Time t3 = jet_clock_getTime();
        Real64 minr_ = Vector_minbf(mr);
        jet_clock_Time t4 = jet_clock_getTime();
        printf("%5g %5g %5g %5g\n", //
            jet_clock_getSpanTimeNano(t0, t1) / 1e3,
            jet_clock_getSpanTimeNano(t1, t2) / 1e3,
            jet_clock_getSpanTimeNano(t2, t3) / 1e3,
            jet_clock_getSpanTimeNano(t3, t4) / 1e3);
    }
}