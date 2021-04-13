struct CodeGenC99 {
    template <typename T>
    void gen(Module& mod, List<T&>& items) {
        for (T& item : items) gen(mod, item);
    }
    void gen(Module& mod) {
        gen(mod, mod.types);
        gen(mod, mod.funcs);
        gen(mod, mod.scope, nullptr);
    }
    void gen(Module& mod, Var& var) { }
    void gen(Module& mod, Type& type) {
        gen(mod, type.vars), gen(mod, type.funcs);
    }
    void gen(Module& mod, Func& func) { gen(mod, func.body, &func); }
    void gen(Module& mod, Scope& scope, Func* func) {
        gen(mod, scope.vars);
        for (Expr& stmt : scope.stmts) gen(mod, stmt, nullptr, scope, func);
    }
    void gen(Module& mod, Expr& expr, Expr* parent, Scope& scope, Func* func) {
    }
};