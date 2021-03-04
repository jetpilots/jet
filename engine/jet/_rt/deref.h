
#ifndef NONULLCHECK
// this is only for idents, not for funcs etc.: those will get evaluated
// twice if you use DEREF. Have to find another way for them (e.g.
// a.pop().xyz ) luckily if we stay away from methods and chaining we should
// be fine. P is for pointer to something, S is string, N is number (default
// values)
#define DEREF1(x, y) (x ? x->y : NULL)
#define DEREF2(x, y, z) (x && x->y ? x->y->z : NULL)
#define DEREF3(x, y, z, a) (x && x->y && x->y->z ? x->y->z->a : NULL)
#define DEREF4(x, y, z, a, b)                                                  \
    (x && x->y && x->y->z && x->y->z->a ? x->y->z->a->b : NULL)

#else
// fast path, all guns blazing, no protection
#define DEREF1(x, y) x->y
#define DEREF2(x, y, z) x->y->z
#define DEREF3(x, y, z, a) x->y->z->a
#define DEREF4(x, y, z, a, b) x->y->z->a->b

#endif