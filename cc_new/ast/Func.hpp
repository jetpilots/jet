struct Func {
    SmallString name;
    Scope body;
    List<Var&> args;
    Var* ret;
    List<Func&> callers, callees;
    char* selector;
    SourceLoc loc;

    bool used, throws, recursive, mutator, exported, reflects, external,
        elemental, intrinsic, defaultCtor, analysed, calledAsync, calledInLoop,
        statement;
};