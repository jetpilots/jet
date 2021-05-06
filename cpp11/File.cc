#include <cstdio>

typedef unsigned char Byte;
#ifdef WINDOWS
#define EOL "\r\n"
#else
#define EOL "\n"
#endif

class File {
    File(const char* path)
        : _path(path) { }
    ~File() { _drop(); }

    const char *_path, *_basename;
    bool _isopen, // is the file open? ...
        _oread, _owrite, _oappend; // ... for these specific modes?
    bool _lastread, _lastwrote; // used to decide when to call rewind()
    FILE* _file;

    void _drop() { _isopen ? fclose(_file) : 0; }
    void _reopen(const char* mode) {
        _file = freopen(NULL, mode, _file);
        _lastread = false;
        _lastwrote = false;
    }
    void _reopenw() { _drop(), _reopen("wb+"); }
    void _reopenr() { _drop(), _reopen("rb+"); }
    void _reopena() { _drop(), _reopen("ab+"); }

    // In the open function, you have to set _oread, _owrite, _oappend
    // carefully. Note that w creates the file and r requires the file to exist.
    // Regardless of how it is opened, both _oread and _owrite will be set. (so
    // you can even remove those flags). o_append may be set and if it is, you
    // have to reopen for a write call. Read calls will not be affected.

    // WHY separate write() and append() calls? the call actually is the same
    // for both. The only difference is during fopen. what if I do append() a
    // few times and then write()?

    // Array<Byte> slurp() { }
    // Array<Byte> readln() { }
    Array<Byte> read(size_t bytes = __SIZE_MAX__) {
        // If I have just written a file and read it, I expect to read from the
        // start of the last written piece, not from start of file. Or do I?
        // It's anyway very weird to be doing R/W on the same file.
        if (_lastwrote) rewind(_file), _lastwrote = false;
        _lastread = true;
    }
    // void writeln(const char* buf, size_t size) {
    //     write(buf, size);
    //     write(EOL, sizeof(EOL) - 1);
    // }

    void write(const char* buf, size_t size, const char* end = EOL,
        size_t endsize = sizeof(EOL) - 1) {

        if (!_owrite) _reopenw();
        if (_lastread)
            fseek(_file, SEEK_END, 0), _lastread = false; // FIXME: fseek args
        size_t written;
        if ((written = fwrite(buf, size, 1, _file)) != size) { ; }
        if ((written = fwrite(end, endsize, 1, _file)) != endsize) { ; }
        _lastwrote = true;
    }
    void write(Array<Byte> arr) { write(arr.buf, arr.len); }
    // void writeln(Array<Byte> arr) { writeln(arr.buf, arr.len); }

    void append(void* buf, size_t size) { }
    void append(Array<Byte> arr) { write(arr.buf, arr.len); }
};
