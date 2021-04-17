struct Import {
    const char *name, *alias;
    Module* mod;
    SourceLoc loc;
    bool used;
};
