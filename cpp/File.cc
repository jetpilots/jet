class File {
    CString path;
    CString basename;
    bool opened;
    FILE* file;
    CString slurp() { }
};