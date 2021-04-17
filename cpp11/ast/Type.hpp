struct Type {
    const char* name;
    Scope* body;
    // List<Func&> funcs;
    SourceLoc loc;
    bool used, isEnum, external;
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
};
static_assert(sizeof(Type) == 24);
