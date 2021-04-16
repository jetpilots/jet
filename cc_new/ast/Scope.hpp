struct Scope {
    List<Var&> vars;
    List<Expr&> stmts;
    Scope* parent;
    Var* getVar(const char* name) {
        for (Var& v : vars)
            if (!strcasecmp(v.name, name)) return &v;
        return parent ? parent->var(name) : nullptr;
    }
};