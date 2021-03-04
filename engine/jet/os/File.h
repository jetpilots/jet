// file and folder functions for stats and I/O including folder watching with
// native OS APIs

typedef struct {
    const char *name, *target;
    const unsigned long long size : 64;
#ifndef JET_MAC
    long long created, modified, accessed;
#else
    long long created, modified, accessed, added;
#endif
    int type, flags;
    // if you make them const they cant be directly modified
    // if you make them bitfields no addresses can be taken
    const unsigned short links : 16, uid : 16, gid : 16;
    const struct {
        const unsigned char name : 1, type : 1, gid : 1, links : 1, perms : 1,
            size : 1, time : 1, uid : 1;
    } has;
    const struct {
        const unsigned char directory : 1, file : 1, link : 1, fifo : 1,
            socket : 1, readable : 1, writable : 1, executable : 1;
    } is; // get const char* groupString;
          // get const char* userString;
} File;
static const int s = sizeof(File);

int main() {
    File f = { .is.readable = 1, .has.name = 1, .name = "bang", .size = 34 };
    f.size = 33;
    unsigned long long* p = &f.size;
    *p = 33;
}
/*-****************************************
 *  File functions
 ******************************************/
#if defined(_MSC_VER)
typedef struct __stat64 stat_t;
typedef int mode_t;
#elif defined(__MINGW32__) && defined(__MSVCRT__)
typedef struct _stati64 stat_t;
#else
typedef struct stat stat_t;
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)                                  \
    || defined(__MSVCRT__) /* windows support */
#define PATH_SEP '\\'
#define STRDUP(s) _strdup(s)
#else
#define PATH_SEP '/'
#include <libgen.h>
#define STRDUP(s) strdup(s)
#endif

#include <stdlib.h> /* malloc, realloc, free */
#include <stdio.h> /* fprintf */
#include <time.h> /* clock_t, clock, CLOCKS_PER_SEC, nanosleep */
#include <errno.h>
#include <assert.h>

#if defined(_WIN32)
#include <sys/utime.h> /* utime */
#include <io.h> /* _chmod */
#else
#include <unistd.h> /* chown, stat */
#if PLATFORM_POSIX_VERSION < 200809L || !defined(st_mtime)
#include <utime.h> /* utime */
#else
#include <fcntl.h> /* AT_FDCWD */
#include <sys/stat.h> /* utimensat */
#endif
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MSVCRT__)
#include <direct.h> /* needed for _mkdir in windows */
#endif

#if defined(__linux__)                                                         \
    || (PLATFORM_POSIX_VERSION                                                 \
        >= 200112L) /* opendir, readdir require POSIX.1-2001 */
#include <dirent.h> /* opendir, readdir */
#include <string.h> /* strerror, memcpy */
#endif /* #ifdef _WIN32 */
/*-*************************************
 *  Functions
 ***************************************/

int util_stat(const char* filename, stat_t* statbuf) {
#if defined(_MSC_VER)
    return !_stat64(filename, statbuf);
#elif defined(__MINGW32__) && defined(__MSVCRT__)
    return !_stati64(filename, statbuf);
#else
    return !stat(filename, statbuf);
#endif
}

int util_isRegularFile(const char* infilename) {
    stat_t statbuf;
    return util_stat(infilename, &statbuf) && util_isRegularFileStat(&statbuf);
}

int util_isRegularFileStat(const stat_t* statbuf) {
#if defined(_MSC_VER)
    return (statbuf->st_mode & S_IFREG) != 0;
#else
    return S_ISREG(statbuf->st_mode) != 0;
#endif
}

/* like chmod, but avoid changing permission of /dev/null */
int util_chmod(
    char const* filename, const stat_t* statbuf, mode_t permissions) {
    stat_t localStatBuf;
    if (statbuf == NULL) {
        if (!util_stat(filename, &localStatBuf)) return 0;
        statbuf = &localStatBuf;
    }
    if (!util_isRegularFileStat(statbuf))
        return 0; /* pretend success, but don't change anything */
    return chmod(filename, permissions);
}

int util_setFileStat(const char* filename, const stat_t* statbuf) {
    int res = 0;

    stat_t curStatBuf;
    if (!util_stat(filename, &curStatBuf)
        || !util_isRegularFileStat(&curStatBuf))
        return -1;

        /* set access and modification times */
        /* We check that st_mtime is a macro here in order to give us confidence
         * that struct stat has a struct timespec st_mtim member. We need this
         * check because there are some platforms that claim to be POSIX 2008
         * compliant but which do not have st_mtim... */
#if (PLATFORM_POSIX_VERSION >= 200809L) && defined(st_mtime)
    {
        /* (atime, mtime) */
        struct timespec timebuf[2] = { { 0, UTIME_NOW } };
        timebuf[1] = statbuf->st_mtim;
        res += utimensat(AT_FDCWD, filename, timebuf, 0);
    }
#else
    {
        struct utimbuf timebuf;
        timebuf.actime = time(NULL);
        timebuf.modtime = statbuf->st_mtime;
        res += utime(filename, &timebuf);
    }
#endif

#if !defined(_WIN32)
    res += chown(
        filename, statbuf->st_uid, statbuf->st_gid); /* Copy ownership */
#endif

    res += util_chmod(filename, &curStatBuf,
        statbuf->st_mode & 07777); /* Copy file permissions */

    errno = 0;
    return -res; /* number of errors is returned */
}

int util_isDirectory(const char* infilename) {
    stat_t statbuf;
    return util_stat(infilename, &statbuf) && util_isDirectoryStat(&statbuf);
}

int util_isDirectoryStat(const stat_t* statbuf) {
#if defined(_MSC_VER)
    return (statbuf->st_mode & _S_IFDIR) != 0;
#else
    return S_ISDIR(statbuf->st_mode) != 0;
#endif
}

int util_isSameFile(const char* fName1, const char* fName2) {
    assert(fName1 != NULL);
    assert(fName2 != NULL);
#if defined(_MSC_VER) || defined(_WIN32)
    /* note : Visual does not support file identification by inode.
     *        inode does not work on Windows, even with a posix layer, like
     * msys2. The following work-around is limited to detecting exact name
     * repetition only, aka `filename` is considered different from
     * `subdir/../filename` */
    return !strcmp(fName1, fName2);
#else
    {
        stat_t file1Stat;
        stat_t file2Stat;
        return util_stat(fName1, &file1Stat) && util_stat(fName2, &file2Stat)
            && (file1Stat.st_dev == file2Stat.st_dev)
            && (file1Stat.st_ino == file2Stat.st_ino);
    }
#endif
}

/* util_isFIFO : distinguish named pipes */
int util_isFIFO(const char* infilename) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    stat_t statbuf;
    if (util_stat(infilename, &statbuf) && util_isFIFOStat(&statbuf)) return 1;
#endif
    (void)infilename;
    return 0;
}

/* util_isFIFO : distinguish named pipes */
int util_isFIFOStat(const stat_t* statbuf) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    if (S_ISFIFO(statbuf->st_mode)) return 1;
#endif
    (void)statbuf;
    return 0;
}

int util_isLink(const char* infilename) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    stat_t statbuf;
    int const r = lstat(infilename, &statbuf);
    if (!r && S_ISLNK(statbuf.st_mode)) return 1;
#endif
    (void)infilename;
    return 0;
}

U64 util_getFileSize(const char* infilename) {
    stat_t statbuf;
    if (!util_stat(infilename, &statbuf)) return util_FILESIZE_UNKNOWN;
    return util_getFileSizeStat(&statbuf);
}

U64 util_getFileSizeStat(const stat_t* statbuf) {
    if (!util_isRegularFileStat(statbuf)) return util_FILESIZE_UNKNOWN;
#if defined(_MSC_VER)
    if (!(statbuf->st_mode & S_IFREG)) return util_FILESIZE_UNKNOWN;
#elif defined(__MINGW32__) && defined(__MSVCRT__)
    if (!(statbuf->st_mode & S_IFREG)) return util_FILESIZE_UNKNOWN;
#else
    if (!S_ISREG(statbuf->st_mode)) return util_FILESIZE_UNKNOWN;
#endif
    return (U64)statbuf->st_size;
}

/* condition : @file must be valid, and not have reached its end.
 * @return : length of line written into @buf, ended with `\0` instead of '\n',
 *           or 0, if there is no new line */
static size_t readLineFromFile(char* buf, size_t len, FILE* file) {
    assert(!feof(file));
    /* Work around Cygwin problem when len == 1 it returns NULL. */
    if (len <= 1) return 0;
    CONTROL(fgets(buf, (int)len, file));
    {
        size_t linelen = CString_length(buf);
        if (!linelen) return 0;
        if (buf[linelen - 1] == '\n') linelen--;
        buf[linelen] = '\0';
        return linelen + 1;
    }
}

/* Conditions :
 *   size of @inputFileName file must be < @dstCapacity
 *   @dst must be initialized
 * @return : nb of lines
 *       or -1 if there's an error
 */
static int readLinesFromFile(
    void* dst, size_t dstCapacity, const char* inputFileName) {
    int nbFiles = 0;
    size_t pos = 0;
    char* const buf = (char*)dst;
    FILE* const inputFile = fopen(inputFileName, "r");

    assert(dst != NULL);

    if (!inputFile) {
        if (g_utilDisplayLevel >= 1) perror("zstd:util:readLinesFromFile");
        return -1;
    }

    while (!feof(inputFile)) {
        size_t const lineLength
            = readLineFromFile(buf + pos, dstCapacity - pos, inputFile);
        if (lineLength == 0) break;
        assert(pos + lineLength < dstCapacity);
        pos += lineLength;
        ++nbFiles;
    }

    CONTROL(fclose(inputFile) == 0);

    return nbFiles;
}

/*Utility function to get file extension from file */
const char* util_getFileExtension(const char* infilename) {
    const char* extension = strrchr(infilename, '.');
    if (!extension || extension == infilename) return "";
    return extension;
}

#define DIR_DEFAULT_MODE 0755
static mode_t getDirMode(const char* dirName) {
    stat_t st;
    if (!util_stat(dirName, &st)) {
        util_DISPLAY(
            "zstd: failed to get DIR stats %s: %s\n", dirName, strerror(errno));
        return DIR_DEFAULT_MODE;
    }
    if (!util_isDirectoryStat(&st)) {
        util_DISPLAY("zstd: expected directory: %s\n", dirName);
        return DIR_DEFAULT_MODE;
    }
    return st.st_mode;
}

static int makeDir(const char* dir, mode_t mode) {
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MSVCRT__)
    int ret = _mkdir(dir);
    (void)mode;
#else
    int ret = mkdir(dir, mode);
#endif
    if (ret != 0) {
        if (errno == EEXIST) return 0;
        util_DISPLAY(
            "zstd: failed to create DIR %s: %s\n", dir, strerror(errno));
    }
    return ret;
}