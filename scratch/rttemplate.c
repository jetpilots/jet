#define CAT(n, ...) CAT##n(__VA_ARGS__)
// #define CAT15(i1, ...) i1
// #define CAT3(i1, i2, ...) JN(3D_##i1, JN(i2, __VA_ARGS__))
#define JN_(x, y) x##_##y
#define JN(x, y) JN_(x, y)
#define CATNAME(name, n, ...) JN(name, CAT(n, __VA_ARGS__))
#define FixedArray(N, ...) CATNAME(FixedArray, N, __VA_ARGS__)

#define CAT2(i1, ...) JN(2D_##i1, i2)
#define CAT3(i1, i2, ...) JN(3D_##i1, JN(i2, __VA_ARGS__))
#define CAT4(i1, i2, i3, ...) JN(4D_##i1, JN(i2, JN(i3, __VA_ARGS__)))
#define CAT5(i1, i2, i3, i4, ...)                                              \
    JN(5D_##i1, JN(i2, JN(i3, JN(i4, __VA_ARGS__))))
#define CAT6(i1, i2, i3, i4, i5, ...)                                          \
    JN(6D_##i1, JN(i2, JN(i3, JN(i4, JN(i5, __VA_ARGS__)))))
#define CAT7(i1, i2, i3, i4, i5, i6, ...)                                      \
    JN(7D_##i1, JN(i2, JN(i3, JN(i4, JN(i5, JN(i6, __VA_ARGS__))))))
#define CAT8(i1, i2, i3, i4, i5, i6, i7, ...)                                  \
    JN(8D_##i1, JN(i2, JN(i3, JN(i4, JN(i5, JN(i6, JN(i7, __VA_ARGS__)))))))
#define CAT9(i1, i2, i3, i4, i5, i6, i7, i8, ...)                              \
    JN(9D_##i1,                                                                \
        JN(i2, JN(i3, JN(i4, JN(i5, JN(i6, JN(i7, JN(i8, __VA_ARGS__))))))))
#define CAT10(i1, i2, i3, i4, i5, i6, i7, i8, i9, ...)                         \
    JN(10D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4, JN(i5, JN(i6, JN(i7, JN(i8, JN(i9, __VA_ARGS__)))))))))
#define CAT11(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, ...)                    \
    JN(11D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4,                                                         \
                    JN(i5,                                                     \
                        JN(i6,                                                 \
                            JN(i7, JN(i8, JN(i9, JN(i10, __VA_ARGS__))))))))))
#define CAT12(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, ...)               \
    JN(12D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4,                                                         \
                    JN(i5,                                                     \
                        JN(i6,                                                 \
                            JN(i7,                                             \
                                JN(i8,                                         \
                                    JN(i9,                                     \
                                        JN(i10, JN(i11, __VA_ARGS__)))))))))))
#define CAT13(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, ...)          \
    JN(13D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4,                                                         \
                    JN(i5,                                                     \
                        JN(i6,                                                 \
                            JN(i7,                                             \
                                JN(i8,                                         \
                                    JN(i9,                                     \
                                        JN(i10,                                \
                                            JN(i11,                            \
                                                JN(i12,                        \
                                                    __VA_ARGS__))))))))))))
#define CAT14(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, ...)     \
    JN(14D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4,                                                         \
                    JN(i5,                                                     \
                        JN(i6,                                                 \
                            JN(i7,                                             \
                                JN(i8,                                         \
                                    JN(i9,                                     \
                                        JN(i10,                                \
                                            JN(i11,                            \
                                                JN(i12,                        \
                                                    JN(i13,                    \
                                                        __VA_ARGS__)))))))))))))
#define CAT15(                                                                 \
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, ...)          \
    JN(15D_##i1,                                                               \
        JN(i2,                                                                 \
            JN(i3,                                                             \
                JN(i4,                                                         \
                    JN(i5,                                                     \
                        JN(i6,                                                 \
                            JN(i7,                                             \
                                JN(i8,                                         \
                                    JN(i9,                                     \
                                        JN(i10,                                \
                                            JN(i11,                            \
                                                JN(i12,                        \
                                                    JN(i13,                    \
                                                        JN(i14,                \
                                                            __VA_ARGS__))))))))))))))

#define CTSUM2(i1, ...) (i1 + __VA_ARGS__)
#define CTSUM3(i1, i2, ...) (i1 + i2 + __VA_ARGS__)
#define CTSUM4(i1, i2, i3, ...) (i1 + i2 + i3 + __VA_ARGS__)
#define CTSUM5(i1, i2, i3, i4, ...) (i1 + i2 + i3 + i4 + __VA_ARGS__)
#define CTSUM6(i1, i2, i3, i4, i5, ...) (i1 + i2 + i3 + i4 + i5 + __VA_ARGS__)
#define CTSUM7(i1, i2, i3, i4, i5, i6, ...)                                    \
    (i1 + i2 + i3 + i4 + i5 + i6 + __VA_ARGS__)
#define CTSUM8(i1, i2, i3, i4, i5, i6, i7, ...)                                \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + __VA_ARGS__)
#define CTSUM9(i1, i2, i3, i4, i5, i6, i7, i8, ...)                            \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + __VA_ARGS__)
#define CTSUM10(i1, i2, i3, i4, i5, i6, i7, i8, i9, ...)                       \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + __VA_ARGS__)
#define CTSUM11(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, ...)                  \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + __VA_ARGS__)
#define CTSUM12(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, ...)             \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + __VA_ARGS__)
#define CTSUM13(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, ...)        \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12 + __VA_ARGS__)
#define CTSUM14(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, ...)   \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12 + i13        \
        + __VA_ARGS__)
#define CTSUM15(                                                               \
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, ...)          \
    (i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12 + i13 + i14  \
        + __VA_ARGS__)
#define CTSUM(N, i1, ...) CTSUM##N(i1, __VA_ARGS__)

#define CTMUL(N, i1, ...) CTMUL##N(i1, __VA_ARGS__)
#define CTMUL2(i1, ...) (i1 * __VA_ARGS__)
#define CTMUL3(i1, i2, ...) (i1 * i2 * __VA_ARGS__)
#define CTMUL4(i1, i2, i3, ...) (i1 * i2 * i3 * __VA_ARGS__)
#define CTMUL5(i1, i2, i3, i4, ...) (i1 * i2 * i3 * i4 * __VA_ARGS__)
#define CTMUL6(i1, i2, i3, i4, i5, ...) (i1 * i2 * i3 * i4 * i5 * __VA_ARGS__)
#define CTMUL7(i1, i2, i3, i4, i5, i6, ...)                                    \
    (i1 * i2 * i3 * i4 * i5 * i6 * __VA_ARGS__)
#define CTMUL8(i1, i2, i3, i4, i5, i6, i7, ...)                                \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * __VA_ARGS__)
#define CTMUL9(i1, i2, i3, i4, i5, i6, i7, i8, ...)                            \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * __VA_ARGS__)
#define CTMUL10(i1, i2, i3, i4, i5, i6, i7, i8, i9, ...)                       \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * __VA_ARGS__)
#define CTMUL11(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, ...)                  \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * i10 * __VA_ARGS__)
#define CTMUL12(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, ...)             \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * i10 * i11 * __VA_ARGS__)
#define CTMUL13(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, ...)        \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * i10 * i11 * i12 * __VA_ARGS__)
#define CTMUL14(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, ...)   \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * i10 * i11 * i12 * i13        \
        * __VA_ARGS__)
#define CTMUL15(                                                               \
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, ...)          \
    (i1 * i2 * i3 * i4 * i5 * i6 * i7 * i8 * i9 * i10 * i11 * i12 * i13 * i14  \
        * __VA_ARGS__)

#define CTCOM(N, ...) CTCOM##N(__VA_ARGS__)
#define CTCOM2(i1, ...)                                                        \
    { i1, __VA_ARGS__ }
#define CTCOM3(i1, i2, ...)                                                    \
    { i1, i2, __VA_ARGS__ }
#define CTCOM4(i1, i2, i3, ...)                                                \
    { i1, i2, i3, __VA_ARGS__ }
#define CTCOM5(i1, i2, i3, i4, ...)                                            \
    { i1, i2, i3, i4, __VA_ARGS__ }
#define CTCOM6(i1, i2, i3, i4, i5, ...)                                        \
    { i1, i2, i3, i4, i5, __VA_ARGS__ }
#define CTCOM7(i1, i2, i3, i4, i5, i6, ...)                                    \
    { i1, i2, i3, i4, i5, i6, __VA_ARGS__ }
#define CTCOM8(i1, i2, i3, i4, i5, i6, i7, ...)                                \
    { i1, i2, i3, i4, i5, i6, i7, __VA_ARGS__ }
#define CTCOM9(i1, i2, i3, i4, i5, i6, i7, i8, ...)                            \
    { i1, i2, i3, i4, i5, i6, i7, i8, __VA_ARGS__ }
#define CTCOM10(i1, i2, i3, i4, i5, i6, i7, i8, i9, ...)                       \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, __VA_ARGS__ }
#define CTCOM11(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, ...)                  \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, __VA_ARGS__ }
#define CTCOM12(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, ...)             \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, __VA_ARGS__ }
#define CTCOM13(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, ...)        \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, __VA_ARGS__ }
#define CTCOM14(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, ...)   \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, __VA_ARGS__ }
#define CTCOM15(                                                               \
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, ...)          \
    { i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, __VA_ARGS__ }

// FixedArray<dims, n1, n2, n3 ... ndims>
// TODO:maybe rename _totsize_ and _shape_ with the generated prefix
#define MAKE_Array(N, ...)                                                     \
    static const size_t _totsize_ = CTMUL(N, __VA_ARGS__);                     \
    static const size_t _shape_[N] = CTCOM(N, __VA_ARGS__);                    \
    typedef struct {                                                           \
        void (*heapalloc)(void (*allocator)(size_t));                          \
        UInt32 (*sub2ind)(UInt32[]);                                           \
        void (*ind2sub)(UInt32, UInt32[]);                                     \
    } JN(__vtbl, FixedArray(N, __VA_ARGS__));                                  \
    typedef struct {                                                           \
        double ref[_totsize_];                                                 \
        JN(__vtbl, FixedArray(N, __VA_ARGS__)) * vtable;                       \
    } FixedArray(N, __VA_ARGS__);                                              \
    UInt32 JN(__fn_sub2ind, FixedArray(N, __VA_ARGS__))(UInt32 sub[]) {        \
        UInt32 ind = 0;                                                        \
        for (int i = 0; i < N; i++) ind += ;                                   \
    }                                                                          \
    void JN(__fn_ind2sub, FixedArray(N, __VA_ARGS__))(                         \
        UInt32 ind, UInt32 sub[]) {                                            \
        for (int i = 0; i < N; i++) sub[i] += ;                                \
    }

MAKE_Array(3, 12, 16, 18)

    MAKE_Array(6, 12, 112, 16, 32, 18, 2)

#ifdef asdasdasdasdsd
#include <stdio.h>
        void* compileArray(int ndims, int sizes[]) {
    // returns pointer to a `Class`, which you can instantiate by calling its
    // .new method. Like ObjC allows you to get a pointer to a Class (without
    // templating).
    FILE* tgt;
    fprintf(tgt, "#include \"rttemplate.i\"\nMAKE_Array(%d", ndims);
    for (int i = 0; i < ndims; i++) fprintf(tgt, ", %d", sizes[i]);
    fprintf(tgt, ")\n");
}
#endif

// actually if you can generate C, just generate the array def yourself! why
// bother with macros?

// its much better to generate with C. You can just generate the functions
// ind2sub sub2ind as macros for given N and shape. There will be no indirect
// call.

// WHAT IS THERE TO GENERATE if everything can be defined as macros? Define the
// ind2sub and sub2ind macros taking int N, static const int[] shape, int [] sub
// and get idx, etc. the "array" is just a raw buffer of size
// sizeof(T)*prod(shape) and if shape is known it can go on stack, static, heap,
// ....
// if shape is not known at compile time it reduces to a normal array that you
// alloc on heap (and keep a size).
// you can even define macros sub2ind_r and sub2ind_c so that you have both row
// major and col major storage possible.
