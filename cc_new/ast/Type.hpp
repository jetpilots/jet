struct Type {
    SmallString name;
    Scope body;
    List<Func&> funcs;
    bool used, isEnum, external;
};