struct Func {
    SmallString name;
    Scope* body;
    List<Var&> args;
    Var* ret;
    List<Func&> callers, callees;
    char* selector;
    SourceLoc loc;

    bool used, throws, recursive, mutator, exported, reflects, external,
        elemental, intrinsic, defaultCtor, analysed, calledAsync, calledInLoop,
        statement;

    size_t calcSizeUsage() {
        size_t size = 0, sum = 0;
        for (Var& arg : args) {
            // all variables must be resolved before calling this
            size = arg.typeInfo.size();
            assert(size);
            // if (arg->used)
            sum += size;
        }
        if (body) sum += body.calcSizeUsage();
        return sum;
    }
    Func() { }
    Func(char* name, char* returnType = nullptr, char* arg1Type = nullptr) {
        // ASTFunc* func = NEW(ASTFunc);
        name = name;
        external = true;
        if (returnType) {
            ret = new Var();
            ret->_typename = returnType;
            // func->returnSpec = NEW(ASTTypeSpec);
            // func->returnSpec->name = retType;
        }
        if (arg1Type) {
            Var* arg = new Var();
            arg->name = "arg1";
            arg->_typename = arg1Type;
            // arg->type = NEW(ASTTypeSpec);
            args.push(*arg);
        }
        // return func;
    }
};