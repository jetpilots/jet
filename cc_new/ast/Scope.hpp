struct Scope {
    List<Var&> vars;
    List<Expr&> stmts;
    Scope* parent;
    Var* var(const char* name) {
        for (Var& v : vars)
            if (v.name.eqi(name)) return &v;
        return parent ? parent->var(name) : nullptr;
    }
};