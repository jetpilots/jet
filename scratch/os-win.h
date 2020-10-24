int touch(char* strFilename) {
    BOOL bRet = FALSE;
    HANDLE hFile = CreateFile(strFilename, GENERIC_WRITE, FILE_SHARE_READ, NULL,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile != INVALID_HANDLE_VALUE) {
        FILETIME ft;
        SYSTEMTIME st;
        GetSystemTime(&st); // gets current time
        SystemTimeToFileTime(&st, &ft); // converts to file time format
        bRet = SetFileTime(hFile, NULL, NULL, &ft);
        CloseHandle(hFile);
    }
    return 1;
}

You need the following to have make - like functionality - reading timestamps
    - updating timestamps - reading and hashing a file to md5 - multithreading
