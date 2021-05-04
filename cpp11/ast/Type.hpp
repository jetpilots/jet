struct Type {
    const char* name;
    Scope* body;
    // List<Func&> funcs;
    SourceLoc loc;
    bool used, _enum, external, analysed;
    Var* getVar(const char* name) {
        return body ? body->getVar(name) : nullptr;
    }
    size_t calcSizeUsage() {
        size_t size = 0, sum = 0;
        if (!body) return 0;
        for (Var& var : body->vars) { // all variables must be resolved
            size = var.typeInfo.size();
            assert(size);
            sum += size;
        }
        return sum;
    }
    bool isEnum() { return _enum; }
};
static_assert(sizeof(Type) == 24);
