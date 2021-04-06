#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "assert.h"
#include "math.h"
#include "limits.h"
#include <float.h>

#include "../base.h"
MKSTAT(IntRange)
MKSTAT(RealRange)

Int signed_saturated_add(Int x, Int y, int* didOverflow) {
    // determine the lower or upper bound of the result
    Int ret = (x < 0) ? INT64_MIN : INT64_MAX;
    // this is always well defined:
    // if x < 0 this adds a positive value to INT64_MIN
    // if x > 0 this subtracts a positive value from INT64_MAX
    Int comp = ret - x;
    // the condition is equivalent to
    // ((x < 0) && (y > comp)) || ((x >=0) && (y <= comp))
    if ((x < 0) == (y > comp))
        ret = x + y;
    else
        *didOverflow = 1;
    return ret;
}
Int signed_saturated_sub(Int x, Int y, int* didOverflow) {
    // CAREFUL: negating INT64_MAX overflows
    return signed_saturated_add(x, -y, didOverflow);
}

Int signed_saturated_mul(Int x, Int y, int* didOverflow) {
    // determine the lower or upper bound of the result
    bool oppsgn = x < 0 != y < 0;
    Int ret = (oppsgn) ? INT64_MIN + 1 : INT64_MAX;
    // printf("x = %lld\n", x);
    // printf("y = %lld\n", y);
    // printf("ret = %lld\n", ret);
    Int comp = llabs(x ? ret / x : ret);
    // printf("comp = %lld\n", comp);

    // if ((x < 0) == (y > comp))
    if ((y < comp))
        ret = x * y;
    else
        *didOverflow = 1;
    return ret;
}

// static bool canoverflow_add(Int a, Int x)
// {
//     return ((x > 0) && (a > INT64_MAX - x))
//         /* `a + x` would overflow */
//         or ((x < 0) && (a < INT64_MIN - x))
//         /* `a + x` would underflow */;
// }

// static bool canoverflow_sub(Int a, Int x)
// {
//     return ((x < 0) && (a > INT64_MAX + x))
//         /* `a - x` would overflow */
//         or ((x > 0) && (a < INT64_MIN + x))
//         /* `a - x` would underflow */;
// }

// static bool canoverflow_mul(Int a, Int x)
// { // There may be a need to check for -1 for two's complement machines.
//     // If one number is -1 and another is INT_MIN, multiplying them we get
//     // abs(INT_MIN) which is 1 higher than INT_MAX
//     return x
//         and ((a > INT64_MAX / x)
//             /* `a * x` would overflow */
//             or ((a < INT64_MIN / x))
//             /* `a * x` would underflow */
//             or ((a == -1) && (x == INT64_MIN))
//             /* `a * x` can overflow */
//             or ((x == -1) && (a == INT64_MIN)));
//     /* `a * x` (or `a / x`) can overflow */
// }

// API:
// IntRange_del: delete
// IntRange_ins: insert
// IntRange_add: apply a + on the entire interval
// IntRange_sub: apply -
// IntRange_mul: apply *
// IntRange_div: apply /
// IntRange_pow: apply ^
// IntRange_mod: apply %
// IntRange_rcp: reciprocal
// IntRange_neg: negate
// IntRange_has: check value in range

typedef struct IntRange {
    Int lo, hi;
    struct IntRange* next;
} IntRange;

typedef struct RealRange {
    Real lo, hi;
    struct RealRange* next;
} RealRange;
// typedef PtrIist IntRange;

static Int max1(Int a, Int b) { return a > b ? a : b; }

static Int min1(Int a, Int b) { return a < b ? a : b; }

static Real fmax1(Real a, Real b) { return a > b ? a : b; }

static Real fmin1(Real a, Real b) { return a < b ? a : b; }

// static IntRange Interval_add(IntRange range, IntRange other)
// {
//     return (IntRange) { .lo = range.lo + other.lo, .hi = range.hi + other.hi
//     };
// }

#define IntRange_desc(a)                                                       \
    {                                                                          \
        printf("%s:%d: %s = ", __FILE__, __LINE__, #a);                        \
        IntRange_print(a);                                                     \
    }
#define RealRange_desc(a)                                                      \
    {                                                                          \
        printf("%s:%d: %s = ", __FILE__, __LINE__, #a);                        \
        RealRange_print(a);                                                    \
    }
static void IntRange_print1(IntRange* range) {
    printf("%lld:%lld\n", range->lo, range->hi);
}
static void RealRange_print1(RealRange* range) {
    printf("%g:%g\n", range->lo, range->hi);
}

static void IntRange_print(IntRange* range) {
    putc('[', stdout);
    while (range) {
        printf("%lld:%lld", range->lo, range->hi);
        range = range->next;
        if (range) printf(", ");
    }
    puts("]");
}
static void RealRange_print(RealRange* range) {
    putc('[', stdout);
    while (range) {
        printf("%g:%g", range->lo, range->hi);
        range = range->next;
        if (range) printf(", ");
    }
    puts("]");
}
static void IntRange_jins(IntRange* range, IntRange* other);
static void RealRange_jins(RealRange* range, RealRange* other);
static void IntRange_rev(IntRange* range) {
    // printf("reversing ");
    // IntRange_print(range);
    IntRange* orig = range;
    while (range && range->next) range = range->next;
    IntRange tmp = *range;
    range = orig;
    while (range) {
        // IntRange* tmp = range->next->next;
        // range->next->next = range;
        // range->next = tmp;
        IntRange_jins(&tmp, range);
        range = range->next;
    }
    *orig = tmp;
}
static void RealRange_rev(RealRange* range) {
    // printf("reversing ");
    // IntRange_print(range);
    RealRange* orig = range;
    while (range && range->next) range = range->next;
    RealRange tmp = *range;
    range = orig;
    while (range) {
        // IntRange* tmp = range->next->next;
        // range->next->next = range;
        // range->next = tmp;
        RealRange_jins(&tmp, range);
        range = range->next;
    }
    *orig = tmp;
}
static void IntRange_snap(IntRange* range) {
    // return;
    // printf("snapping ");
    // IntRange_print(range);
    while (range) {
        while (range->next && range->lo <= range->next->lo
            && range->hi + 1 >= range->next->lo) {
            range->hi = max1(range->hi, range->next->hi);
            range->lo = min1(range->lo, range->next->lo);
            range->next = range->next->next;
        }
        range = range->next;
    }
}
// if you had a func called nextnum like this:
//     nextnum(num as Int) := num + 1
//     nextnum(num as Real) := nextafter(num, DBL_MAX)
// then you could have one snap function, and since it's probably the only
// one that differs in IntRange and RealRange, the whole thing could be
// templated. actually it can be done straight in C++ and also with C macros.
static void RealRange_snap(RealRange* range) {
    // return;
    // printf("snapping ");
    // IntRange_print(range);
    while (range) {
        while (range->next && range->lo <= range->next->lo
            && nextafter(range->hi, DBL_MAX) >= range->next->lo) {
            range->hi = fmax1(range->hi, range->next->hi);
            range->lo = fmin1(range->lo, range->next->lo);
            range->next = range->next->next;
        }
        range = range->next;
    }
}
static void IntRange_add(IntRange* range, IntRange* other) {
    IntRange* orig = range;
    while (range) {
        int* stat = (int[]) { 0 };
        range->lo = signed_saturated_add(range->lo, other->lo, stat);
        range->hi = signed_saturated_add(range->hi, other->hi, stat);
        // range->lo += other->lo;
        // range->hi += other->hi;
        if (*stat) printf("add overflowed\n");
        range = range->next;
    }
    IntRange_snap(orig);
    // do need to snap, since the added value can be large enough to cause hi to
    // reach over the next lo
}
static void RealRange_add(RealRange* range, RealRange* other) {
    RealRange* orig = range;
    while (range) {
        range->lo += other->lo;
        range->hi += other->hi;

        range = range->next;
    }
    RealRange_snap(orig);
}

static void IntRange_sub(IntRange* range, IntRange* other) {
    IntRange flipped = { .lo = -other->hi, .hi = -other->lo };
    IntRange_add(range, &flipped);
}

static void RealRange_sub(RealRange* range, RealRange* other) {
    RealRange flipped = { .lo = -other->hi, .hi = -other->lo };
    RealRange_add(range, &flipped);
}

static void minmax(int n, Int arr[], Int* min, Int* max) {
    Int mmin = INT64_MAX, mmax = INT64_MIN;
    for (int i = 0; i < n; i++) {
        mmin = min1(mmin, arr[i]);
        mmax = max1(mmax, arr[i]);
    }
    *min = mmin;
    *max = mmax;
}

static void fminmax(int n, Real arr[], Real* min, Real* max) {
    Real mmin = DBL_MAX, mmax = -DBL_MAX; // FIXME
    for (int i = 0; i < n; i++) {
        mmin = fmin1(mmin, arr[i]);
        mmax = fmax1(mmax, arr[i]);
    }
    *min = mmin;
    *max = mmax;
}

static void IntRange_mul(IntRange* range, IntRange* other) {
    IntRange* orig = range;
    Int a, b, c, d; // minv, maxv;
    while (range) {
        int* stat = (int[]) { 0 };
        // TODO: if (stat)... at every step
        a = signed_saturated_mul(range->lo, other->lo, stat);
        b = signed_saturated_mul(range->hi, other->lo, stat);
        c = signed_saturated_mul(range->lo, other->hi, stat);
        //    if (stat) ...
        d = signed_saturated_mul(range->hi, other->hi, stat);
        if (*stat) printf("mul overflowed\n");
        minmax(4, (Int[]) { a, b, c, d }, &(range->lo), &(range->hi));

        range = range->next;
    }
    if (other->lo <= 0 == other->hi <= 0) {
        // reverse the list
        IntRange_rev(orig);
    }
    IntRange_snap(orig);
    // range->lo=minv,
}

static void RealRange_mul(RealRange* range, RealRange* other) {
    RealRange* orig = range;
    Real a, b, c, d;
    // RealRange_print1(range);
    // RealRange_print1(other);
    while (range) {
        a = range->lo * other->lo;
        b = range->hi * other->lo;
        c = range->lo * other->hi;
        d = range->hi * other->hi;
        // printf("-- %g %g %g %g\n", a, b, c, d);
        fminmax(4, (Real[]) { a, b, c, d }, &(range->lo), &(range->hi));
        // RealRange_print1(range);
        range = range->next;
    }

    if (other->lo <= 0 && other->hi <= 0) RealRange_rev(orig);
    RealRange_snap(orig);
}

// #define mapf(range, other, func)                                                \
//     while (range) {                                                             \
//         func(range->item, other);                                               \
//         range = range->next;                                                     \
//     }

// static int Interval_contains(  IntRange*   range,   Int value)
// {
//     return range->lo <= value && value <= range->hi;
// }
// static void IntRange_print(  IntRange*   range)
// {
//     printf("%lld:%lld", range->lo, range->hi);
// }

// returns the pointer to the IIST ITEM containing the value.
// if you want the interval, you should use ->item of the returned ptr.
static IntRange* IntRange_find(IntRange* range, Int value) {
    // if you can keep the list sorted, you can just check the extremities first
    // guess you must sort, its needed for coalescing if you want to avoid
    // O(N^2)
    while (range) {
        if (range->lo > value)
            return NULL;
        else if (value <= range->hi)
            return range;
        range = range->next;
    }
    return NULL;
}

static RealRange* RealRange_find(RealRange* range, Real value) {
    // if you can keep the list sorted, you can just check the extremities first
    // guess you must sort, its needed for coalescing if you want to avoid
    // O(N^2)
    while (range) {
        if (range->lo > value)
            return NULL;
        else if (value <= range->hi)
            return range;
        range = range->next;
    }
    return NULL;
}

static void IntRange_ins1(IntRange* range, Int value) {
    while (range && range->next && range->next->lo < value) range = range->next;
    IntRange_print1(range);

    // range->next = PtrIist_with_next(other, range->next);
}

static IntRange* IntRange_new(Int lo, Int hi, IntRange* next) {
    IntRange* I = NEW(IntRange);
    I->hi = hi;
    I->lo = lo;
    I->next = next;
    return I;
}

static RealRange* RealRange_new(Real lo, Real hi, RealRange* next) {
    RealRange* range = NEW(RealRange);
    range->hi = hi;
    range->lo = lo;
    range->next = next;
    return range;
}

static RealRange* RealRange_new1(Real value, RealRange* next) {
    return RealRange_new(value, value, next);
}

static RealRange* RealRange_new0() { return RealRange_new(0.0, 0.0, NULL); }

static RealRange* RealRange_clone1(RealRange* other) {
    return RealRange_new(other->lo, other->hi, other->next);
}

static IntRange* IntRange_new1(Int value, IntRange* next) {
    return IntRange_new(value, value, next);
}

static IntRange* IntRange_new0() { return IntRange_new(0, 0, NULL); }

static IntRange* IntRange_clone1(IntRange* other) {
    return IntRange_new(other->lo, other->hi, other->next);
}

static IntRange* IntRange_clone(IntRange* other) {
    // deep clone all items in chain
    IntRange* Io = IntRange_clone1(other);
    IntRange* I = Io;
    while (I->next) {
        I->next = IntRange_clone1(I->next);
        I = I->next;
    }
    return Io;
}
// function clone(other as IntRange) as IntRange
//     clone = clone1(other)
//     var i as IntRange = clone
//     while i.next != nil
//         i.next = clone1(i.next)
//         i = i.next
//     end while
// end function

static RealRange* RealRange_clone(RealRange* other) {
    // deep clone all items in chain
    RealRange* Fo = RealRange_clone1(other);
    RealRange* F = Fo;
    while (F->next) {
        F->next = RealRange_clone1(F->next);
        F = F->next;
    }
    return Fo;
}

static void IntRange_jins(IntRange* range, IntRange* other) {
    // you'll need to make a copy of other, since the next ptr is embeddded
    // if (!*intvp) {
    //     *intvp = IntRange_new(other->lo, other->hi, NULL);
    //     // (*intvp)->lo = other->lo;
    //     // (*intvp)->hi = other->hi;
    //     return;
    // }
    // IntRange* range = *intvp;
    // IntRange_desc(range);
    IntRange* orig = range;
    while (range && range->next && range->next->lo < other->lo)
        range = range->next;
    if (range == orig && other->lo < range->lo) {
        range->next = IntRange_clone1(range);
        range->lo = other->lo;
        range->hi = other->hi;
    } else
        range->next = IntRange_new(other->lo, other->hi, range->next);
    // IntRange_desc(range);
    // IntRange_snap(range);
    // IntRange_desc(range);

    // range->next = PtrIist_with_next(other, range->next);
}

static void RealRange_jins(RealRange* range, RealRange* other) {
    RealRange* orig = range;
    while (range && range->next && range->next->lo < other->lo)
        range = range->next;
    if (range == orig && other->lo < range->lo) {
        range->next = RealRange_clone1(range);
        range->lo = other->lo;
        range->hi = other->hi;
    } else {
        range->next = RealRange_new(other->lo, other->hi, range->next);
    }
}

// TODO RealRange_compl [7:8, 90:99]->[-inf:6, 9:89, 100:inf] same no of
// subranges
//     or 1 more.
//     or 1 less modify in place and add ins
//         / del 1 if needed.
//           // lo0=-inf
//           nhi
//     = lo - 1 next nlo = hi
//     + 1 TODO RealRange_intersect

//           then you can tell exactly what range is problematic eg log on
//       [-35:-20, -5:inf]
//           ->err [-35:-20, -5 0] is problem region but you cann do it also by
//               RealRange_del the valid region.

// jins: "Just insert". Otherwise ins will call snap each time.
static void IntRange_ins(IntRange* range, IntRange* other) {
    IntRange_jins(range, other);
    IntRange_snap(range);
}
static void RealRange_ins(RealRange* range, RealRange* other) {
    RealRange_jins(range, other);
    RealRange_snap(range);
}

static void IntRange_del1(IntRange* range, Int value) {
    // Get the particular subinterval which contains value
    // printf("deleting %lld from ", value);
    // IntRange_print(range);

    IntRange* iv = IntRange_find(range, value);
    if (!iv) return; // value isn't in the list at all? get out
    if (iv->lo == value && iv->hi == value)
        // value is in a subinterval by itself
        *iv = *iv->next;
    // the old IntRange will go out of scope and should be freed
    // TODO: dealloc(old iv)
    else if (iv->lo == value)
        iv->lo++; // value is the lower limit? increase it
    else if (iv->hi == value)
        iv->hi--; // value is the upper limit? decrease it
    else {
        // value is in betewen. You'll have to split this interval.
        IntRange* newI = IntRange_new(value + 1, iv->hi, iv->next);
        // NEW(IntRange);
        // *newI = (IntRange) { .lo = value + 1, .hi = iv->hi, .next = iv->next
        // };
        iv->hi = value - 1;
        iv->next = newI; // PtrList_with_next(newI, iv->next);
    }
    // printf("after deletion ");
    // IntRange_print(range);
}

static void RealRange_del1(RealRange* range, Real value) {
    // Get the particular subinterval which contains value
    // printf("deleting %lld from ", value);
    // IntRange_print(range);

    RealRange* fiv = RealRange_find(range, value);
    if (!fiv) return; // value isn't in the list at all? get out
    if (fiv->lo == value && fiv->hi == value)
        // value is in a subinterval by itself
        *fiv = *fiv->next;
    // the old IntRange will go out of scope and should be freed
    // TODO: dealloc(old iv)
    else if (fiv->lo == value)
        fiv->lo = nextafter(fiv->lo, DBL_MAX);
    // value is the lower limit? increase it
    else if (fiv->hi == value)
        fiv->hi = nextafter(fiv->hi, -DBL_MAX);
    // value is the upper limit? decrease it
    else {
        // value is in betewen. You'll have to split this interval.
        RealRange* newF
            = RealRange_new(nextafter(value, DBL_MAX), fiv->hi, fiv->next);
        // NEW(IntRange);
        // *newI = (IntRange) { .lo = value + 1, .hi = iv->hi, .next = iv->next
        // };
        fiv->hi = nextafter(value, -DBL_MAX);
        fiv->next = newF; // PtrList_with_next(newI, iv->next);
    }
    // printf("after deletion ");
    // IntRange_print(range);
}

static void IntRange_del(IntRange* range, IntRange* other) { }

#define RealRange_haz(F, v)                                                    \
    printf("%s %s %g\n", #F, RealRange_find(F, v) ? "has" : "doesn't have", v);

#define IntRange_haz(I, v)                                                     \
    printf("%s %s %d\n", #I, IntRange_find(I, v) ? "has" : "doesn't have", v);

IntRange* parse(const char* str) {
    IntRange* range = NULL;
    // const char* buf = "[-2:2, 5:6, 7:9, 8:10]";
    char* ptr = strchr(str, '[');
    if (!ptr) {
        printf("parser error\n");
        return NULL;
    } else {
        ptr++;
    }
    Int a, b;
    while (ptr && *ptr) {
        a = strtoll(ptr, &ptr, 0);
        if (*ptr != ':') {
            printf("parser error: expecting ':'\n");
            break;
        }
        ptr++;
        b = strtoll(ptr, &ptr, 0);

        // printf("found %lld:%lld\n", a, b);
        if (!range) {
            range = IntRange_new(a, b, NULL);
        } else {
            IntRange tmp = { .lo = a, .hi = b };
            IntRange_jins(range, &tmp);
        }

        while (*ptr == ' ') ptr++;
        if (*ptr == ']') break;
        if (*ptr == ',')
            ptr++;
        else {
            printf("parser error: unexpected '%c'\n", *ptr);
            break;
        }
    }
    // printf("parsed: ");
    // IntRange_print(range);

    IntRange_snap(range);
    // printf("snapped: ");
    // IntRange_print(range);
    return range;
}

char* convertBase(Int n, int k, char* a) {
    int j, i = 0, sign = 0;
    if (n == 0) a[i--] = '0';
    if (n < 0) {
        sign = -1;
        n = -n;
    }
    while (n > 0) {
        j = n % k;
        printf("%2d %c\n", j, j < 10 ? j + '0' : j - 10 + 'A');
        if (j < 10)
            a[i] = j + '0';
        else
            a[i] = j - 10 + 'A';
        n = n / k;
        i--;
    }
    if (sign == -1) a[i--] = '-';
    // a[i] = 0;
    return a + i + 1;
}

int main() {
    // printf("%.55g ->\n  %.55g\n", 0.0, nextafter(10.0e213, DBL_MAX));
    IntRange_print(parse("[-2:2, 5:6, 7:9, 8:10]"));
    // char* smp = "Y2p0IJ32e8E7";
    // uint64_t smpul = strtoull(smp, NULL, 36);
    // printf("%llu %llu\n", smpul, UINT64_MAX - smpul);

    // char buf[128] = {};
    // // memset(buf, '.', 127);
    // char* p = convertBase(INT64_MIN + 1, 36, buf + 126);
    // printf("%p %p %p\n", p, buf, buf + 127);
    // printf("%s\n", p);

    RealRange a[1] = { { .lo = 3.3, .hi = 5.1 } };
    RealRange b[1] = { { .lo = -5.4, .hi = -3.1 } };
    RealRange c[1] = { { .lo = -3.14, .hi = -3.04 } };
    RealRange d[1] = { { .lo = 10.03, .hi = 11.1234 } };
    RealRange e[1] = { { .lo = 19, .hi = 40.5 } };
    RealRange f[1] = { { .lo = 7.5, .hi = 10.03 } };
    RealRange* range = RealRange_new0();

    RealRange_jins(range, a);
    RealRange_jins(range, b);
    RealRange_jins(range, c);
    RealRange_jins(range, d);
    RealRange_jins(range, e);
    RealRange_jins(range, f);
    RealRange_snap(range); // TODO: probably move snap into ins or user will
                           // forget to call it every time. but this is good for
                           // perf if you call it after inserting all you want.

    // IntRange_haz(a, 6);
    RealRange_desc(range);
    RealRange_haz(range, 7.0);

    RealRange_del1(range, 7);
    RealRange_desc(range);
    RealRange_haz(range, 7.0);
    RealRange_mul(range, c);
    RealRange_desc(range);
    // IntRange_add(range, f);
    // IntRange_desc(range);
    // IntRange_snap(range);
    // IntRange_desc(range);

    // Int i = 1UL << 62, j = 1UL << 62;
    // printf("%d\n", canoverflow_add(i, j));
    // printf("%d\n", canoverflow_mul(i, j));
    // printf("%d\n", canoverflow_sub(i, j));
    // printf("%lld %lld %lld %lld\n", i, j, i + j, signed_saturated_add(i, j));

    // for (Int i = 1; i < INT64_MAX; i *= 2) {
    //     printf("%lld %20lld %20lld %20lld\n", 512LL, i, i * 512,
    //         signed_saturated_mul(i, 512));
    //     if (canoverflow_mul(512, i)) {
    //         printf("./%s:%d:%d: runtime error: %lld * %lld has overflowed\n",
    //             __FILE__, 315, 61, 512LL, i);
    //         break;
    //     }
    // }
    // printf("%d\n", not not IntRange_which(range, 12));
    // printf("%d\n", not not IntRange_which(range, 7));
    // printf("%d\n", not not IntRange_which(range, 3));
    return 0;
}