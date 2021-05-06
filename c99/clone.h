static Var* Var_clone(Var* var);
static Expr* Expr_clone(Expr* expr);

#define List_clone(T) T##List_clone
#define MKLIST_CLONE(T)                                                        \
    static List(T) * T##List_clone(List(T) * list) {                           \
        List(T)* new = NULL, ** last = &new;                                   \
        foreach (T*, l, list) { last = PtrList_append(last, T##_clone(l)); }   \
        return new;                                                            \
    }

MKLIST_CLONE(Expr)
MKLIST_CLONE(Var)

static Expr* Expr_clone(Expr* expr) {
    Expr* new = NEW(Expr);
    *new = *expr;
    switch (new->kind) {
    case tkFunctionCall:
    case tkSubscript:
    case tkFunctionCallResolved:
    case tkSubscriptResolved: new->left = Expr_clone(new->left); break;
    case tkString:
    case tkRawString: new->vars = List_clone(Expr)(new->vars);
    default:
        if (!new->unary&& new->left) new->left = Expr_clone(new->left);
        if (new->right) new->right = Expr_clone(new->right);
    }
    return new;
}

static TypeSpec* TypeSpec_clone(TypeSpec* spec) {
    TypeSpec* new = NEW(TypeSpec);
    *new = *spec;
    return new;
}

static Var* Var_clone(Var* var) {
    Var* new = NEW(Var);
    *new = *var;
    if (new->init) new->init = Expr_clone(new->init);
    return new;
}

static Scope* Scope_clone(Scope* scope) {
    Scope* new = NEW(Scope);
    *new = *scope;
    new->locals = List_clone(Var)(new->locals);
    new->stmts = List_clone(Expr)(new->stmts);
    return new;
}

static Func* Func_clone(Func* func) {
    Func* new = NEW(Func);
    *new = *func;
    func->args = List_clone(Var)(func->args);
    func->body = Scope_clone(func->body);
    return new;
}

static Type* Type_clone(Type* type) {
    Type* new = NEW(Type);
    *new = *type;
    // type->params = List_clone(Var, type->params); // TODO
    type->body = Scope_clone(type->body);
    return new;
}