// typedef double double;
typedef array_t(double) vector_t;
#define vector_resize array_resize(double)

#define vector_foreachptr(p, vec)                                              \
    for (double *p = (vec)->ref, *_e = p + (vec)->used; p < _e; p++)

// NEeds some improvements: vector_t doesnt require calloc or memset to 0, and
// needs a func growToExactly where it does not check roundUp32. Vectors may be
// used for push pop but often they will be init'd and used with a known size.
// Also need a vector_clone func -> no call concatArray on an empty new
// vector_t.

// void vector_growToSizeOf(vector_t* self, vector_t* other)
// {
//     vector_resize(self, other->cap)
// }
// vector_t* vec, op: [+-*/%]=, double num
/// Perform a binop `op` (+=, -=, *=, /=) on scalar `num` and vector `vec`
/// in-place (in `vec`). Same thing as calling the _out version with aliased
/// `vec` and `out`, but this may be faster.
#define vector__mutop1(vec, op, num)                                           \
    for (double *_v = (vec)->ref, *_ve = _v + (vec)->used; _v < _ve; _v++)     \
        *_v op(num);

/// Perform a binop `op` (+=, -=, *=, /=) on vector `vec2` and vector `vec`
/// in-place (in `vec`). Same thing as calling the _out version with aliased
/// `vec` and `out`, but this may be faster.
#define vector__mutopv(vec, op, vec2)                                          \
    for (double *_v = (vec)->ref, *_ve = _v + (vec)->used, *_v2 = (vec2)->ref; \
         _v < _ve; _v++, _v2++)                                                \
        *_v op* _v2;

// vector_t* vec, op: +-*/%, double num, vector_t* out

/// Perform a binop `op` (+, -, *, /) on scalar `num` and vector `vec` and store
/// result in vector `out`. `out` must be preallocated by the caller and of the
/// same size as `vec`. Inplaceable (`out` can alias `vec`).
#define vector__mutop1_out(vec, op, num, out)                                  \
    for (double *_v = (vec)->ref, *_ve = _v + (vec)->used, *_o = (out)->ref;   \
         _v < _ve; _v++, _o++)                                                 \
        *_o = *_v op(num);

/// Perform a binop `op` (+, -, *, /) on vector `vec2` and vector `vec` and
/// store result in vector `out`. `out` must be preallocated by the caller and
/// of the same size as `vec`. Inplaceable (`out` can alias `vec`).
#define vector__mutopv_out(vec, op, vec2, out)                                 \
    for (double *_v = (vec)->ref, *_ve = _v + (vec)->used, *_o = (out)->ref,   \
                *_v2 = (vec2)->ref;                                            \
         _v < _ve; _v++, _o++, _v2++)                                          \
        *_o = *_v op * _v2;

void vector_add1(vector_t* vec, double num) { vector__mutop1(vec, +=, num); }
void vector_sub1(vector_t* vec, double num) { vector__mutop1(vec, -=, num); }
void vector_mul1(vector_t* vec, double num) { vector__mutop1(vec, *=, num); }
void vector_div1(vector_t* vec, double num) { vector__mutop1(vec, /=, num); }
// void vector_mod1(vector_t* vec, double num) { vector__mutop1(vec, %=,
// (int)num);
// }

void vector_add1_out(vector_t* vec, double num, vector_t* out) {
    vector__mutop1_out(vec, +=, num, out);
}
void vector_sub1_out(vector_t* vec, double num, vector_t* out) {
    vector__mutop1_out(vec, -=, num, out);
}
void vector_mul1_out(vector_t* vec, double num, vector_t* out) {
    vector__mutop1_out(vec, *=, num, out);
}
void vector_div1_out(vector_t* vec, double num, vector_t* out) {
    vector__mutop1_out(vec, /=, num, out);
}
// void vector_mod_out(vector_t* vec, double num, vector_t* out)
// {
//     vector__mutop1_out(vec, %=, num, out);
// }

void vector_bwddiff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    // this is a read-after-write dep when out aliases vec. i.e.
    // you are using something that is not in the direction of traversal
    // relative to the var being written to.
    // for (int i = 1; i < vec->used; i++) out[i] = vec[i] - vec[i - 1];
    // write it by reversing the traversal
    for (int i = vec->used - 1; i > 0; i--)
        out->ref[i] = vec->ref[i] - vec->ref[i - 1];
    out->ref[0] = out->ref[1];

    // the naive user would just write it going from 0 to used. But that would
    // be a lost opportunity to inplace. That would be a function that could
    // possibly be inplaced ending up not doing so, and thus creating a
    // temporary. The thing is you leave it to the users' skill ultimately.
    // TODO: at least issue a warning (when applicable) that such functions
    // could be improved by traversing in the reverse order.
}
// Better do dy=fwddiff(y);dx=fwddiff(x);dydx=dy/dx;
// /// You have to send the diff of the denominator i.e. to get dy by dx
// you have to send y and dx, not y and x. If you're calculating diffs of
// multiple vars you can reuse the dx without recomputing it in each diff
// call. void fwddiff_by(vector_t* vec, vector_t* by, vector_t* out)
// {
//     for (int i = 1; i < vec->used; i++) out[i] = (vec[i] - vec[i - 1]) /
//     by[i]; out[0] = out[1];
// }

//
void vector_fwddiff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    for (int i = 0; i < vec->used - 1; i++)
        out->ref[i] = vec->ref[i + 1] - vec->ref[i];
    out->ref[vec->used] = out->ref[vec->used - 1];
}

// the functions are wriiten (by the user) as if vec & out do not alias. they
// are read by the compiler as if vec & out DO alias and then it checks if
// array deps prevent inplacing. if so then out must be same-sized as vec but
// NEED NOT be cloned! it is only reading from vec.of course if there are parts
// it doesnt read then they should be carried over yes, thats when cloning is
// needed. maybe func should mark whether that is the case.
void vector_ctrdiff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    double prev = vec->ref[0];
    for (int i = 1; i < vec->used - 1; i++) {
        // you need to remove the read-after-write dep. It doesnt go away
        // just by creating temps if you are still reading after write.
        // here since vec is out, you read i-1 after having set i, so the
        // problem is still the same.
        // double oldf = vec[i + 1], oldb = vec[i - 1];
        // out[i] = next - prev;

        // solve it by making write-after-read. Take the var which is being
        // read-after-written and put only that var into a temp.
        out->ref[i] = vec->ref[i + 1] - prev;
        prev = vec->ref[i];
    }
    out->ref[vec->used] = out->ref[vec->used - 1];
    out->ref[0] = out->ref[1];
}

// second-order central difference
// this gives d2y, to get d2y/dx2 you should do d2y/(dx^2)
void vector_ctr2diff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    double prev = vec->ref[0];
    for (int i = 1; i < vec->used - 1; i++) {
        // you need to remove the read-after-write dep. It doesnt go away
        // just by creating temps if you are still reading after write.
        // here since vec is out, you read i-1 after having set i, so the
        // problem is still the same.
        // double oldf = vec[i + 1], oldb = vec[i - 1];
        // out[i] = next - prev;

        // solve it by making write-after-read. Take the var which is being
        // read-after-written and put only that var into a temp.
        out->ref[i] = vec->ref[i + 1] - 2 * vec->ref[i] + prev;
        prev = vec->ref[i];
    }
    out->ref[vec->used] = out->ref[vec->used - 1];
    out->ref[0] = out->ref[1];
}

void vector_fwd2diff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    for (int i = 0; i < vec->used - 2; i++)
        out->ref[i] = vec->ref[i + 2] - 2 * vec->ref[i + 1] + vec->ref[i];
    out->ref[vec->used] = out->ref[vec->used - 1] = out->ref[vec->used - 2];
}

void vector_bwd2diff(vector_t* vec, vector_t* out) {
    vector_resize(out, vec->used);
    for (int i = vec->used - 1; i > 1; i--)
        out->ref[i] = vec->ref[i] - 2 * vec->ref[i - 1] + vec->ref[i - 2];
    out->ref[0] = out->ref[1] = out->ref[2];
}

/// Use trapezoidal rule to integrate dy. If you want to integrate over a grid
/// dx, first do dy*dx (elemwise multiply) and then send that into
/// integrate(...).
double vector_integrate(vector_t* vec) {
    double ret = 0;
    for_to(i, vec->used - 1) ret += (vec->ref[i] + vec->ref[i + 1]) / 2.0;
    return ret;
}
double vector_sum(vector_t* vec) {
    double ret = 0;
    vector_foreachptr(v, vec) ret += *v;
    return ret;
}
double vector_product(vector_t* vec) {
    double ret = 1;
    vector_foreachptr(v, vec) ret *= *v;
    return ret;
}
double vector_mean(vector_t* vec) {
    // double ret = 0;
    return vec->used ? vector_sum(vec) / vec->used : 0;
}
double vector_min(vector_t* vec) {
    double ret = __DBL_MAX__;
    vector_foreachptr(v, vec) if (*v < ret) ret = *v;
    return vec->used ? ret : 0;
}
double vector_max(vector_t* vec) {
    double ret = -__DBL_MAX__;
    vector_foreachptr(v, vec) if (*v > ret) ret = *v;
    return vec->used ? ret : 0;
}
int Real64_compare(const void* a_, const void* b_) {
    double a = *(double*)a_;
    double b = *(double*)b_;
    return a < b ? -1 : a > b ? 1 : 0;
}
void vector_print_prec(vector_t* vec, int prec) {
    printf("[");
    for_to(i, vec->used - 1) printf("%.*g, ", prec + 1, vec->ref[i]);
    if (vec->used) printf("%.*g", prec + 1, vec->ref[vec->used - 1]);
    printf("]\n");
}
void vector_print(vector_t* vec) { vector_print_prec(vec, 15); }
void vector_qsort(vector_t* vec) {
    qsort(vec->ref, vec->used, sizeof(double), Real64_compare);
}
/// Branch-free max and min, if you have really unpredictable data. Test this at
/// some point... yeah it has really horrible performance at -O0, and at -O3 it
/// has the same performance as branching
double vector_minbf(vector_t* vec) {
    double ret = __DBL_MAX__;
    vector_foreachptr(v, vec) ret = fmin(ret, *v);
    return vec->used ? ret : 0;
}
double vector_maxbf(vector_t* vec) {
    double ret = -__DBL_MAX__;
    vector_foreachptr(v, vec) ret = fmax(ret, *v);
    return vec->used ? ret : 0;
}
#define isnonzero(v) ((v) != 0.0)
#define isnonzero_tol(v, tol) (fabs(v) < tol)

double vector_meannz(vector_t* vec) {
    double sum = 0;
    UInt32 cnt = 0;
    vector_foreachptr(v, vec) if (isnonzero(*v)) {
        sum += *v;
        cnt++;
    }
    return cnt ? sum / cnt : 0;
}
double vector_countnz(vector_t* vec) {
    UInt32 cnt = 0;
    vector_foreachptr(v, vec) if (isnonzero(*v)) cnt++;
    return cnt;
}
double vector_minnz(vector_t* vec) {
    double ret = __DBL_MAX__;
    vector_foreachptr(v, vec) if (*v < ret && isnonzero(*v)) ret = *v;
    return vec->used ? ret : 0;
}
double vector_maxnz(vector_t* vec) {
    double ret = -__DBL_MAX__;
    vector_foreachptr(v, vec) if (*v > ret && isnonzero(*v)) ret = *v;
    return vec->used ? ret : 0;
}

double vector_meannz_tol(vector_t* vec, double tol) {
    double sum = 0;
    UInt32 cnt = 0;
    vector_foreachptr(v, vec) if (isnonzero_tol(*v, tol)) {
        sum += *v;
        cnt++;
    }
    return cnt ? sum / cnt : 0;
}
double vector_countnz_tol(vector_t* vec, double tol) {
    UInt32 cnt = 0;
    vector_foreachptr(v, vec) if (isnonzero_tol(*v, tol)) cnt++;
    return cnt;
}
double vector_minnz_tol(vector_t* vec, double tol) {
    double ret = __DBL_MAX__;
    vector_foreachptr(v, vec) if (*v < ret && isnonzero_tol(*v, tol)) ret = *v;
    return vec->used ? ret : 0;
}
double vector_maxnz_tol(vector_t* vec, double tol) {
    double ret = -__DBL_MAX__;
    vector_foreachptr(v, vec) if (*v > ret && isnonzero_tol(*v, tol)) ret = *v;
    return vec->used ? ret : 0;
}

/// this is the rms with the mean, right?
double vector_stddev(vector_t* vec) {
    double ret = 0;
    return ret;
}
/// RMS with a given vector
double sq(double num) { return num * num; }
double vector_rms(vector_t* vec, vector_t* vec2) {
    double ret = 0;
    for_to(i, vec->used) ret += sq(vec->ref[i] - vec2->ref[i]);
    return sqrt(ret);
}
double vector_rms1(vector_t* vec, double num) {
    double ret = 0;
    for_to(i, vec->used) ret += sq(vec->ref[i] - num);
    return sqrt(ret);
}
bool vector_inbounds(vector_t* vec, UInt32 idx) { return idx < vec->used; }
void vector_fillval(vector_t* vec, double val) {
    vector_foreachptr(v, vec)* v = val;
}
void vector_fillzero(vector_t* vec) { vector_fillval(vec, 0); }

#define vector_applyfn(vec, out, func)                                         \
    for_to(_i, (vec)->used)(out)->ref[_i] = func((vec)->ref[_i]);

double exp10(double x) {
    static const double p10[] = { 1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10,
        1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3,
        1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15 };
    double n, y = modf(x, &n);
    if (fabs(n) < 16) {
        if (!y) return p10[(int)n + 15];
        y = exp2(3.32192809488736234787031942948939 * y);
        return y * p10[(int)n + 15];
    }
    return pow(10.0, x);
}

void vector_cbrt(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, cbrt);
}
void vector_sqrt(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, sqrt);
}
void vector_log2(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, log2);
}
void vector_log(vector_t* vec, vector_t* out) { vector_applyfn(vec, out, log); }
void vector_log10(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, log10);
}
void vector_exp10(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, exp10);
}
void vector_exp2(vector_t* vec, vector_t* out) {
    vector_applyfn(vec, out, exp2);
}
void vector_exp(vector_t* vec, vector_t* out) { vector_applyfn(vec, out, exp); }
// void log_base(vector_t* vec, vector_t* out) { vector_applyfn(vec,out,logb); }
/// Should just call it `dot` it is the dot product
double vector_sumproduct(vector_t* vec, vector_t* vec2) {
    double ret = 0;
    for_to(i, vec->used) ret += vec->ref[i] * vec2->ref[i];
    return ret;
}
/// This might be slightly more efficient than calling sumproduct(vec,vec).
double vector_mag2(vector_t* vec) {
    double ret = 0;
    for_to(i, vec->used) ret += vec->ref[i] * vec->ref[i];
    return ret;
}
/// magnitude: same as RMS with itself but maybe more efficient
double vector_mag(vector_t* vec) { return sqrt(vector_mag2(vec)); }
void vector_cumsumproduct(vector_t* vec, vector_t* out) { }
void vector_cumsum(vector_t* vec, vector_t* out) { }

// typedef struct {
//     double start, end, step; // count is computed
// } Real64LinRange;

// void vector_linspace(double start, double end, vector_t* out)
// {
//     UInt32 count = 1 + floor(end - start);
//     vector_resize(out, count);
//     double cumsum1 = start;
//     for_to(i, count)
//     {
//         out->ref[i] = cumsum1;
//         cumsum1 += 1;
//     };
// }

void vector__linspace_step_count(
    double start, double end, double step, UInt32 count, vector_t* out) {
    vector_resize(out, count);
    double cumsum1 = start;
    for_to(i, count - 1) {
        out->ref[i] = cumsum1;
        cumsum1 += step;
    };
    out->ref[count - 1] = end;
}
void vector_linspace_count(
    double start, double end, UInt32 count, vector_t* out) {
    double step = (end - start) / (count - 1);
    vector__linspace_step_count(start, end, step, count, out);
}
void vector_linspace_step(
    double start, double end, double step, vector_t* out) {
    UInt32 count = 1 + floor((end - start) / step);
    vector__linspace_step_count(start, end, step, count, out);
}
void vector_linspace(double start, double end, vector_t* out) {
    vector_linspace_step(start, end, 1, out);
}

void vector_log10space(double start, double end, UInt32 count, vector_t* out) {
    vector_linspace_count(log10(start), log10(end), count, out);
    vector_exp10(out, out);
}

void vector_log2space(double start, double end, UInt32 count, vector_t* out) {
    vector_linspace_count(log2(start), log2(end), count, out);
    vector_exp2(out, out);
}
void vector_logspace(double start, double end, UInt32 count, vector_t* out) {
    vector_linspace_count(log(start), log(end), count, out);
    vector_exp(out, out);
}
void vector_fillrandoms(vector_t* vec) {
    vector_foreachptr(v, vec)* v = randf();
}

/// Uses binary search to locate the given number in the vector.
bool vector_binsearch(vector_t* vec, double num) { return 0; }
bool vector_binsearch_tol(vector_t* vec, double num) { return 0; }

vector_t* vector_smake() { return NULL; }
vector_t* vector_hmake() { return NULL; }
vector_t* vector_smake_fromCArray() { return NULL; }
vector_t* vector_hmake_fromCArray() { return NULL; }
vector_t* vector_hmake_clone() { return NULL; }

// TODO: upwind, linearUpwind, QUICK, whatnot