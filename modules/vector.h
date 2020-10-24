// typedef double Real64;
typedef jet_Array(Real64) Vector;
#define jet_Vector_resize jet_Array_resize(Real64)

#define jet_Vector_foreachptr(p, vec)                                          \
    for (Real64* p = (vec)->ref, *_e = p + (vec)->used; p < _e; p++)

// NEeds some improvements: Vector doesnt require calloc or memset to 0, and
// needs a func growToExactly where it does not check roundUp32. Vectors may be
// used for push pop but often they will be init'd and used with a known size.
// Also need a Vector_clone func -> no call concatArray on an empty new Vector.

// void jet_Vector_growToSizeOf(Vector* self, Vector* other)
// {
//     jet_Vector_resize(self, other->cap)
// }
// Vector* vec, op: [+-*/%]=, Real64 num
/// Perform a binop `op` (+=, -=, *=, /=) on scalar `num` and vector `vec`
/// in-place (in `vec`). Same thing as calling the _out version with aliased
/// `vec` and `out`, but this may be faster.
#define Vector__mutop1(vec, op, num)                                           \
    for (Real64* _v = (vec)->ref, *_ve = _v + (vec)->used; _v < _ve; _v++)     \
        *_v op(num);

/// Perform a binop `op` (+=, -=, *=, /=) on vector `vec2` and vector `vec`
/// in-place (in `vec`). Same thing as calling the _out version with aliased
/// `vec` and `out`, but this may be faster.
#define Vector__mutopv(vec, op, vec2)                                          \
    for (Real64* _v = (vec)->ref, *_ve = _v + (vec)->used, *_v2 = (vec2)->ref; \
         _v < _ve; _v++, _v2++)                                                \
        *_v op* _v2;

// Vector* vec, op: +-*/%, Real64 num, Vector* out

/// Perform a binop `op` (+, -, *, /) on scalar `num` and vector `vec` and store
/// result in vector `out`. `out` must be preallocated by the caller and of the
/// same size as `vec`. Inplaceable (`out` can alias `vec`).
#define Vector__mutop1_out(vec, op, num, out)                                  \
    for (Real64* _v = (vec)->ref, *_ve = _v + (vec)->used, *_o = (out)->ref;   \
         _v < _ve; _v++, _o++)                                                 \
        *_o = *_v op(num);

/// Perform a binop `op` (+, -, *, /) on vector `vec2` and vector `vec` and
/// store result in vector `out`. `out` must be preallocated by the caller and
/// of the same size as `vec`. Inplaceable (`out` can alias `vec`).
#define Vector__mutopv_out(vec, op, vec2, out)                                 \
    for (Real64* _v = (vec)->ref, *_ve = _v + (vec)->used, *_o = (out)->ref,   \
                 *_v2 = (vec2)->ref;                                           \
         _v < _ve; _v++, _o++, _v2++)                                          \
        *_o = *_v op * _v2;

void Vector_add1(Vector* vec, Real64 num) { Vector__mutop1(vec, +=, num); }
void Vector_sub1(Vector* vec, Real64 num) { Vector__mutop1(vec, -=, num); }
void Vector_mul1(Vector* vec, Real64 num) { Vector__mutop1(vec, *=, num); }
void Vector_div1(Vector* vec, Real64 num) { Vector__mutop1(vec, /=, num); }
// void Vector_mod1(Vector* vec, Real64 num) { Vector__mutop1(vec, %=,
// (int)num);
// }

void Vector_add1_out(Vector* vec, Real64 num, Vector* out)
{
    Vector__mutop1_out(vec, +=, num, out);
}
void Vector_sub1_out(Vector* vec, Real64 num, Vector* out)
{
    Vector__mutop1_out(vec, -=, num, out);
}
void Vector_mul1_out(Vector* vec, Real64 num, Vector* out)
{
    Vector__mutop1_out(vec, *=, num, out);
}
void Vector_div1_out(Vector* vec, Real64 num, Vector* out)
{
    Vector__mutop1_out(vec, /=, num, out);
}
// void Vector_mod_out(Vector* vec, Real64 num, Vector* out)
// {
//     Vector__mutop1_out(vec, %=, num, out);
// }

void Vector_bwddiff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
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
// call. void fwddiff_by(Vector* vec, Vector* by, Vector* out)
// {
//     for (int i = 1; i < vec->used; i++) out[i] = (vec[i] - vec[i - 1]) /
//     by[i]; out[0] = out[1];
// }

//
void Vector_fwddiff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
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
void Vector_ctrdiff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
    Real64 prev = vec->ref[0];
    for (int i = 1; i < vec->used - 1; i++) {
        // you need to remove the read-after-write dep. It doesnt go away
        // just by creating temps if you are still reading after write.
        // here since vec is out, you read i-1 after having set i, so the
        // problem is still the same.
        // Real64 oldf = vec[i + 1], oldb = vec[i - 1];
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
void Vector_ctr2diff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
    Real64 prev = vec->ref[0];
    for (int i = 1; i < vec->used - 1; i++) {
        // you need to remove the read-after-write dep. It doesnt go away
        // just by creating temps if you are still reading after write.
        // here since vec is out, you read i-1 after having set i, so the
        // problem is still the same.
        // Real64 oldf = vec[i + 1], oldb = vec[i - 1];
        // out[i] = next - prev;

        // solve it by making write-after-read. Take the var which is being
        // read-after-written and put only that var into a temp.
        out->ref[i] = vec->ref[i + 1] - 2 * vec->ref[i] + prev;
        prev = vec->ref[i];
    }
    out->ref[vec->used] = out->ref[vec->used - 1];
    out->ref[0] = out->ref[1];
}

void Vector_fwd2diff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
    for (int i = 0; i < vec->used - 2; i++)
        out->ref[i] = vec->ref[i + 2] - 2 * vec->ref[i + 1] + vec->ref[i];
    out->ref[vec->used] = out->ref[vec->used - 1] = out->ref[vec->used - 2];
}

void Vector_bwd2diff(Vector* vec, Vector* out)
{
    jet_Vector_resize(out, vec->used);
    for (int i = vec->used - 1; i > 1; i--)
        out->ref[i] = vec->ref[i] - 2 * vec->ref[i - 1] + vec->ref[i - 2];
    out->ref[0] = out->ref[1] = out->ref[2];
}

Real64 Vector_sum(Vector* vec)
{
    Real64 ret = 0;
    jet_Vector_foreachptr(v, vec) ret += *v;
    return ret;
}
Real64 Vector_product(Vector* vec)
{
    Real64 ret = 1;
    jet_Vector_foreachptr(v, vec) ret *= *v;
    return ret;
}
Real64 Vector_mean(Vector* vec)
{
    // Real64 ret = 0;
    return vec->used ? Vector_sum(vec) / vec->used : 0;
}
Real64 Vector_min(Vector* vec)
{
    Real64 ret = __DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v < ret) ret = *v;
    return vec->used ? ret : 0;
}
Real64 Vector_max(Vector* vec)
{
    Real64 ret = -__DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v > ret) ret = *v;
    return vec->used ? ret : 0;
}
int Real64_compare(const void* a_, const void* b_)
{
    Real64 a = *(Real64*)a_;
    Real64 b = *(Real64*)b_;
    return a < b ? -1 : a > b ? 1 : 0;
}
void Vector_print_prec(Vector* vec, int prec)
{
    printf("[");
    for_to(i, vec->used - 1) printf("%.*g, ", prec + 1, vec->ref[i]);
    if (vec->used) printf("%.*g", prec + 1, vec->ref[vec->used - 1]);
    printf("]\n");
}
void Vector_print(Vector* vec) { Vector_print_prec(vec, 15); }
void Vector_qsort(Vector* vec)
{
    qsort(vec->ref, vec->used, sizeof(Real64), Real64_compare);
}
/// Branch-free max and min, if you have really unpredictable data. Test this at
/// some point... yeah it has really horrible performance at -O0, and at -O3 it
/// has the same performance as branching
Real64 Vector_minbf(Vector* vec)
{
    Real64 ret = __DBL_MAX__;
    jet_Vector_foreachptr(v, vec) ret = fmin(ret, *v);
    return vec->used ? ret : 0;
}
Real64 Vector_maxbf(Vector* vec)
{
    Real64 ret = -__DBL_MAX__;
    jet_Vector_foreachptr(v, vec) ret = fmax(ret, *v);
    return vec->used ? ret : 0;
}
#define isnonzero(v) ((v) != 0.0)
#define isnonzero_tol(v, tol) (fabs(v) < tol)

Real64 Vector_meannz(Vector* vec)
{
    Real64 sum = 0;
    UInt32 cnt = 0;
    jet_Vector_foreachptr(v, vec) if (isnonzero(*v))
    {
        sum += *v;
        cnt++;
    }
    return cnt ? sum / cnt : 0;
}
Real64 Vector_countnz(Vector* vec)
{
    UInt32 cnt = 0;
    jet_Vector_foreachptr(v, vec) if (isnonzero(*v)) cnt++;
    return cnt;
}
Real64 Vector_minnz(Vector* vec)
{
    Real64 ret = __DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v < ret && isnonzero(*v)) ret = *v;
    return vec->used ? ret : 0;
}
Real64 Vector_maxnz(Vector* vec)
{
    Real64 ret = -__DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v > ret && isnonzero(*v)) ret = *v;
    return vec->used ? ret : 0;
}

Real64 Vector_meannz_tol(Vector* vec, Real64 tol)
{
    Real64 sum = 0;
    UInt32 cnt = 0;
    jet_Vector_foreachptr(v, vec) if (isnonzero_tol(*v, tol))
    {
        sum += *v;
        cnt++;
    }
    return cnt ? sum / cnt : 0;
}
Real64 Vector_countnz_tol(Vector* vec, Real64 tol)
{
    UInt32 cnt = 0;
    jet_Vector_foreachptr(v, vec) if (isnonzero_tol(*v, tol)) cnt++;
    return cnt;
}
Real64 Vector_minnz_tol(Vector* vec, Real64 tol)
{
    Real64 ret = __DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v < ret && isnonzero_tol(*v, tol)) ret
        = *v;
    return vec->used ? ret : 0;
}
Real64 Vector_maxnz_tol(Vector* vec, Real64 tol)
{
    Real64 ret = -__DBL_MAX__;
    jet_Vector_foreachptr(v, vec) if (*v > ret && isnonzero_tol(*v, tol)) ret
        = *v;
    return vec->used ? ret : 0;
}

/// this is the rms with the mean, right?
Real64 Vector_stddev(Vector* vec)
{
    Real64 ret = 0;
    return ret;
}
/// RMS with a given vector
Real64 sq(Real64 num) { return num * num; }
Real64 Vector_rms(Vector* vec, Vector* vec2)
{
    Real64 ret = 0;
    for_to(i, vec->used) ret += sq(vec->ref[i] - vec2->ref[i]);
    return sqrt(ret);
}
Real64 Vector_rms1(Vector* vec, Real64 num)
{
    Real64 ret = 0;
    for_to(i, vec->used) ret += sq(vec->ref[i] - num);
    return sqrt(ret);
}
bool Vector_inbounds(Vector* vec, UInt32 idx) { return idx < vec->used; }
void Vector_fillval(Vector* vec, Real64 val)
{
    jet_Vector_foreachptr(v, vec)* v = val;
}
void Vector_fillzero(Vector* vec) { Vector_fillval(vec, 0); }

#define Vector_applyfn(vec, out, func)                                         \
    for_to(_i, (vec)->used)(out)->ref[_i] = func((vec)->ref[_i]);

double exp10(double x)
{
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

void Vector_cbrt(Vector* vec, Vector* out) { Vector_applyfn(vec, out, cbrt); }
void Vector_sqrt(Vector* vec, Vector* out) { Vector_applyfn(vec, out, sqrt); }
void Vector_log2(Vector* vec, Vector* out) { Vector_applyfn(vec, out, log2); }
void Vector_log(Vector* vec, Vector* out) { Vector_applyfn(vec, out, log); }
void Vector_log10(Vector* vec, Vector* out) { Vector_applyfn(vec, out, log10); }
void Vector_exp10(Vector* vec, Vector* out) { Vector_applyfn(vec, out, exp10); }
void Vector_exp2(Vector* vec, Vector* out) { Vector_applyfn(vec, out, exp2); }
void Vector_exp(Vector* vec, Vector* out) { Vector_applyfn(vec, out, exp); }
// void log_base(Vector* vec, Vector* out) { Vector_applyfn(vec,out,logb); }
/// Should just call it `dot` it is the dot product
Real64 Vector_sumproduct(Vector* vec, Vector* vec2)
{
    Real64 ret = 0;
    for_to(i, vec->used) ret += vec->ref[i] * vec2->ref[i];
    return ret;
}
/// This might be slightly more efficient than calling sumproduct(vec,vec).
Real64 Vector_mag2(Vector* vec)
{
    Real64 ret = 0;
    for_to(i, vec->used) ret += vec->ref[i] * vec->ref[i];
    return ret;
}
/// magnitude: same as RMS with itself but maybe more efficient
Real64 Vector_mag(Vector* vec) { return sqrt(Vector_mag2(vec)); }
void Vector_cumsumproduct(Vector* vec, Vector* out) { }
void Vector_cumsum(Vector* vec, Vector* out) { }

// typedef struct {
//     Real64 start, end, step; // count is computed
// } Real64LinRange;

// void Vector_linspace(Real64 start, Real64 end, Vector* out)
// {
//     UInt32 count = 1 + floor(end - start);
//     jet_Vector_resize(out, count);
//     Real64 cumsum1 = start;
//     for_to(i, count)
//     {
//         out->ref[i] = cumsum1;
//         cumsum1 += 1;
//     };
// }

void Vector__linspace_step_count(
    Real64 start, Real64 end, Real64 step, UInt32 count, Vector* out)
{
    jet_Vector_resize(out, count);
    Real64 cumsum1 = start;
    for_to(i, count - 1)
    {
        out->ref[i] = cumsum1;
        cumsum1 += step;
    };
    out->ref[count - 1] = end;
}
void Vector_linspace_count(Real64 start, Real64 end, UInt32 count, Vector* out)
{
    Real64 step = (end - start) / (count - 1);
    Vector__linspace_step_count(start, end, step, count, out);
}
void Vector_linspace_step(Real64 start, Real64 end, Real64 step, Vector* out)
{
    UInt32 count = 1 + floor((end - start) / step);
    Vector__linspace_step_count(start, end, step, count, out);
}
void Vector_linspace(Real64 start, Real64 end, Vector* out)
{
    Vector_linspace_step(start, end, 1, out);
}

void Vector_log10space(Real64 start, Real64 end, UInt32 count, Vector* out)
{
    Vector_linspace_count(log10(start), log10(end), count, out);
    Vector_exp10(out, out);
}

void Vector_log2space(Real64 start, Real64 end, UInt32 count, Vector* out)
{
    Vector_linspace_count(log2(start), log2(end), count, out);
    Vector_exp2(out, out);
}
void Vector_logspace(Real64 start, Real64 end, UInt32 count, Vector* out)
{
    Vector_linspace_count(log(start), log(end), count, out);
    Vector_exp(out, out);
}
void Vector_fillrandoms(Vector* vec)
{
    jet_Vector_foreachptr(v, vec)* v = jet_randf();
}

/// Uses binary search to locate the given number in the vector.
bool Vector_binsearch(Vector* vec, Real64 num) { }
bool Vector_binsearch_tol(Vector* vec, Real64 num) { }

Vector* Vector_smake() { }
Vector* Vector_hmake() { }
Vector* Vector_smake_fromCArray() { }
Vector* Vector_hmake_fromCArray() { }
Vector* Vector_hmake_clone() { }

// TODO: upwind, linearUpwind, QUICK, whatnot