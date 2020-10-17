struct ASTVar {
    int f;
};

struct ASTFunc {
    int f;
};

struct ASTExpr {
    // union {
    ASTVar& _t;
    // ASTFunc& sr;
    // };
    ASTExpr(ASTVar& t)
        : _t(t)
    {
    }
    ASTExpr(ASTFunc& r)
        : _t((ASTVar&)r)
    {
    }
    ASTVar& var() { return _t; }
    ASTFunc& func() { return (ASTFunc&)_t; }
};
// expr.var().lint();
// expr.func().getBlockSize();

// ops.push(token);
// p = rpn.pop();

ASTVar& fn()
{
    ASTVar* v = 0;
    return (ASTVar&)v;
}
#include <iostream>
#include <stdio.h>

int main()
{
    ASTVar t;
    ASTFunc r;
    ASTExpr m1(t);
    ASTExpr m2(r);
    ASTVar& v = fn();
    // printf("%p\n", &v);
    // std::cout << v.f;
}
