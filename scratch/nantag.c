/* 64-bit tagged NaN demo.
 *
 * 63          51 48
 * v           v  v
 * SEEEEEEEEEEEMMMMMMMMMMMMMMMMMM...
 *             ~~~~
 */
#include <stdint.h>
#include <assert.h>
#include <math.h>

/*
 * Our box type.
 */
typedef union {
    double dbl;
    uint64_t raw;
} box_t;

/*
 * NaN value means the box holds a pointer.
 */
#define box_is_pointer(b) (isnan(b.dbl))

/*
 * To turn the pointer into a NaN value, we set all the exponent bits and
 * (at least) one mantissa bit.
 */
#define box_set_pointer(b, v) (b.raw = (uint64_t)(v) | 0x7ff8000000000000UL)

/*
 * To turn the NaN value back into a pointer, we mask off all the top bits.
 * Note that this doesn't work if the 48th bit of the pointer is set, since
 * x64 requires that the top 16 bits be copies of the 48th bit.
 */
#define box_get_pointer(b) ((void*)(b.raw & 0x0000ffffffffffffUL))

/*
 * For double values, we just set/get the double field directly.
 */
#define box_set_double(b, v) (b.dbl = v)
#define box_get_double(b) (b.dbl)

int main(void) {
    /* Basic sanity checks */
    assert(sizeof(double) == 8);
    assert(sizeof(void*) == 8);
    assert(sizeof(box_t) == 8);

    box_t b;
    int x = 42, *px = &x;
    double y = 3.14159;
    void* z = (void*)0x0000123456789abc;

    /* Read/write pointer */
    box_set_pointer(b, px);
    assert(box_is_pointer(b));
    assert(box_get_pointer(b) == px);

    /* Read/write double */
    box_set_double(b, y);
    assert(!box_is_pointer(b));
    assert(box_get_double(b) == y);

    /* Read/write weird pointer */
    box_set_pointer(b, z);
    assert(box_is_pointer(b));
    assert(box_get_pointer(b) == z);

    return 0;
}