static Var* Var_clone(Var* var);
static Expr* Expr_clone(Expr* expr);

#define List_clone(T) T##List_clone
#define MKLIST_CLONE(T)                                                    \
  static List(T) * T##List_clone(List(T) * list) {                         \
    List(T)* new = NULL, ** last = &new;                                   \
    foreach (T*, l, list) { last = li_push(last, T##_clone(l)); }          \
    return new;                                                            \
  }

MKLIST_CLONE(Expr)
MKLIST_CLONE(Var)
#define CLONE(T, v) memcpy(NEW(T), v, sizeof(T))

static Expr* Expr_clone(Expr* expr) {
  Expr* new = CLONE(Expr, expr);
  switch (new->kind) {
  case tkFuncCall:
  case tkSubscript:
  case tkFuncCallR:
  case tkSubscriptR: new->left = Expr_clone(new->left); break;
  case tkString:
  case tkRawString: new->vars = List_clone(Expr)(new->vars);
  default:
    if (!new->unary&& new->left) new->left = Expr_clone(new->left);
    if (new->right) new->right = Expr_clone(new->right);
  }
  return new;
}

static TypeSpec* spec_clone(TypeSpec* spec) {
  TypeSpec* new = CLONE(TypeSpec, spec);
  return new;
}

static Var* Var_clone(Var* var) {
  Var* new = CLONE(Var, var);
  if (new->init) new->init = Expr_clone(new->init);
  return new;
}

static Scope* scope_clone(Scope* scope) {
  Scope* new = NEW(Scope);
  *new = *scope;
  new->locals = List_clone(Var)(new->locals);
  new->stmts = List_clone(Expr)(new->stmts);
  return new;
}

static Func* func_clone(Func* func) {
  Func* new = NEW(Func);
  *new = *func;
  func->args = List_clone(Var)(func->args);
  func->body = scope_clone(func->body);
  return new;
}

static Type* type_clone(Type* type) {
  Type* new = NEW(Type);
  *new = *type;
  // type->params = List_clone(Var, type->params); // TODO
  type->body = scope_clone(type->body);
  return new;
}