/*
 * Copyright (c) 2016-2020, Przemyslaw Skibinski, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under both the BSD-style license (found in the
 * LICENSE file in the root directory of this source tree) and the GPLv2 (found
 * in the COPYING file in the root directory of this source tree).
 * You may select, at your option, one of the above-listed licenses.
 */

/*-****************************************
 *  Dependencies
 ******************************************/

#include "platform.h" /* PLATFORM_POSIX_VERSION, ZSTD_NANOSLEEP_SUPPORT, ZSTD_SETPRIORITY_SUPPORT */
#include <stddef.h> /* size_t, ptrdiff_t */
#include <sys/types.h> /* stat, utime */
#include <sys/stat.h> /* stat, chmod */
#include "../lib/common/mem.h" /* U64 */

/*-************************************************************
 * Avoid fseek()'s 2GiB barrier with MSVC, macOS, *BSD, MinGW
 ***************************************************************/
#if defined(_MSC_VER) && (_MSC_VER >= 1400)
#define jet_util_fseek _fseeki64
#elif !defined(__64BIT__)                                                      \
    && (PLATFORM_POSIX_VERSION                                                 \
        >= 200112L) /* No point defining Large file for 64 bit */
#define jet_util_fseek fseeko
#elif defined(__MINGW32__) && defined(__MSVCRT__) && !defined(__STRICT_ANSI__) \
    && !defined(__NO_MINGW_LFS)
#define jet_util_fseek fseeko64
#else
#define jet_util_fseek fseek
#endif

/*-*************************************************
 *  Sleep & priority functions: Windows - Posix - others
 ***************************************************/
#if defined(_WIN32)
#include <windows.h>
#define SET_REALTIME_PRIORITY                                                  \
    SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS)
#define jet_util_sleep(s) Sleep(1000 * s)
#define jet_util_sleepMilli(milli) Sleep(milli)

#elif PLATFORM_POSIX_VERSION > 0 /* Unix-like operating system */
#include <unistd.h> /* sleep */
#define jet_util_sleep(s) sleep(s)
#if ZSTD_NANOSLEEP_SUPPORT /* necessarily defined in platform.h */
#define jet_util_sleepMilli(milli)                                             \
    {                                                                          \
        struct timespec t;                                                     \
        t.tv_sec = 0;                                                          \
        t.tv_nsec = milli * 1000000ULL;                                        \
        nanosleep(&t, NULL);                                                   \
    }
#else
#define jet_util_sleepMilli(milli) /* disabled */
#endif
#if ZSTD_SETPRIORITY_SUPPORT
#include <sys/resource.h> /* setpriority */
#define SET_REALTIME_PRIORITY setpriority(PRIO_PROCESS, 0, -20)
#else
#define SET_REALTIME_PRIORITY /* disabled */
#endif

#else /* unknown non-unix operating systen */
#define jet_util_sleep(s) /* disabled */
#define jet_util_sleepMilli(milli) /* disabled */
#define SET_REALTIME_PRIORITY /* disabled */
#endif

/*-****************************************
 *  Compiler specifics
 ******************************************/
#if defined(__INTEL_COMPILER)
#pragma warning(                                                               \
    disable : 177) /* disable: message #177: function was declared but never   \
                      referenced, useful with jet_util_STATIC */
#endif
#if defined(__GNUC__)
#define jet_util_STATIC static __attribute__((unused))
#elif defined(__cplusplus)                                                     \
    || (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 */)
#define jet_util_STATIC static inline
#elif defined(_MSC_VER)
#define jet_util_STATIC static __inline
#else
#define jet_util_STATIC                                                        \
    static /* this version may generate warnings for unused static functions;  \
              disable the relevant warning */
#endif

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

int jet_util_stat(const char* filename, stat_t* statbuf) {
#if defined(_MSC_VER)
    return !_stat64(filename, statbuf);
#elif defined(__MINGW32__) && defined(__MSVCRT__)
    return !_stati64(filename, statbuf);
#else
    return !stat(filename, statbuf);
#endif
}

int jet_util_isRegularFile(const char* infilename) {
    stat_t statbuf;
    return jet_util_stat(infilename, &statbuf)
        && jet_util_isRegularFileStat(&statbuf);
}

int jet_util_isRegularFileStat(const stat_t* statbuf) {
#if defined(_MSC_VER)
    return (statbuf->st_mode & S_IFREG) != 0;
#else
    return S_ISREG(statbuf->st_mode) != 0;
#endif
}

/* like chmod, but avoid changing permission of /dev/null */
int jet_util_chmod(
    char const* filename, const stat_t* statbuf, mode_t permissions) {
    stat_t localStatBuf;
    if (statbuf == NULL) {
        if (!jet_util_stat(filename, &localStatBuf)) return 0;
        statbuf = &localStatBuf;
    }
    if (!jet_util_isRegularFileStat(statbuf))
        return 0; /* pretend success, but don't change anything */
    return chmod(filename, permissions);
}

int jet_util_setFileStat(const char* filename, const stat_t* statbuf) {
    int res = 0;

    stat_t curStatBuf;
    if (!jet_util_stat(filename, &curStatBuf)
        || !jet_util_isRegularFileStat(&curStatBuf))
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

    res += jet_util_chmod(filename, &curStatBuf,
        statbuf->st_mode & 07777); /* Copy file permissions */

    errno = 0;
    return -res; /* number of errors is returned */
}

int jet_util_isDirectory(const char* infilename) {
    stat_t statbuf;
    return jet_util_stat(infilename, &statbuf)
        && jet_util_isDirectoryStat(&statbuf);
}

int jet_util_isDirectoryStat(const stat_t* statbuf) {
#if defined(_MSC_VER)
    return (statbuf->st_mode & _S_IFDIR) != 0;
#else
    return S_ISDIR(statbuf->st_mode) != 0;
#endif
}

int jet_util_isSameFile(const char* fName1, const char* fName2) {
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
        return jet_util_stat(fName1, &file1Stat)
            && jet_util_stat(fName2, &file2Stat)
            && (file1Stat.st_dev == file2Stat.st_dev)
            && (file1Stat.st_ino == file2Stat.st_ino);
    }
#endif
}

/* jet_util_isFIFO : distinguish named pipes */
int jet_util_isFIFO(const char* infilename) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    stat_t statbuf;
    if (jet_util_stat(infilename, &statbuf) && jet_util_isFIFOStat(&statbuf))
        return 1;
#endif
    (void)infilename;
    return 0;
}

/* jet_util_isFIFO : distinguish named pipes */
int jet_util_isFIFOStat(const stat_t* statbuf) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    if (S_ISFIFO(statbuf->st_mode)) return 1;
#endif
    (void)statbuf;
    return 0;
}

int jet_util_isLink(const char* infilename) {
/* macro guards, as defined in : https://linux.die.net/man/2/lstat */
#if PLATFORM_POSIX_VERSION >= 200112L
    stat_t statbuf;
    int const r = lstat(infilename, &statbuf);
    if (!r && S_ISLNK(statbuf.st_mode)) return 1;
#endif
    (void)infilename;
    return 0;
}

U64 jet_util_getFileSize(const char* infilename) {
    stat_t statbuf;
    if (!jet_util_stat(infilename, &statbuf)) return jet_util_FILESIZE_UNKNOWN;
    return jet_util_getFileSizeStat(&statbuf);
}

U64 jet_util_getFileSizeStat(const stat_t* statbuf) {
    if (!jet_util_isRegularFileStat(statbuf)) return jet_util_FILESIZE_UNKNOWN;
#if defined(_MSC_VER)
    if (!(statbuf->st_mode & S_IFREG)) return jet_util_FILESIZE_UNKNOWN;
#elif defined(__MINGW32__) && defined(__MSVCRT__)
    if (!(statbuf->st_mode & S_IFREG)) return jet_util_FILESIZE_UNKNOWN;
#else
    if (!S_ISREG(statbuf->st_mode)) return jet_util_FILESIZE_UNKNOWN;
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
        size_t linelen = strlen(buf);
        if (strlen(buf) == 0) return 0;
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
const char* jet_util_getFileExtension(const char* infilename) {
    const char* extension = strrchr(infilename, '.');
    if (!extension || extension == infilename) return "";
    return extension;
}

#define DIR_DEFAULT_MODE 0755
static mode_t getDirMode(const char* dirName) {
    stat_t st;
    if (!jet_util_stat(dirName, &st)) {
        jet_util_DISPLAY(
            "zstd: failed to get DIR stats %s: %s\n", dirName, strerror(errno));
        return DIR_DEFAULT_MODE;
    }
    if (!jet_util_isDirectoryStat(&st)) {
        jet_util_DISPLAY("zstd: expected directory: %s\n", dirName);
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
        jet_util_DISPLAY(
            "zstd: failed to create DIR %s: %s\n", dir, strerror(errno));
    }
    return ret;
}

/*-****************************************
 *  count the number of physical cores
 ******************************************/

#if defined(_WIN32) || defined(WIN32)

#include <windows.h>

typedef BOOL(WINAPI* LPFN_GLPI)(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION, PDWORD);

int jet_util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;
    if (numPhysicalCores != 0) return numPhysicalCores;

    {
        LPFN_GLPI glpi;
        BOOL done = FALSE;
        PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
        PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
        DWORD returnLength = 0;
        size_t byteOffset = 0;

#if defined(_MSC_VER)
/* Visual Studio does not like the following cast */
#pragma warning(disable : 4054) /* conversion from function ptr to data ptr */
#pragma warning(disable : 4055) /* conversion from data ptr to function ptr */
#endif
        glpi = (LPFN_GLPI)(void*)GetProcAddress(
            GetModuleHandle(TEXT("kernel32")),
            "GetLogicalProcessorInformation");

        if (glpi == NULL) { goto failed; }

        while (!done) {
            DWORD rc = glpi(buffer, &returnLength);
            if (FALSE == rc) {
                if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                    if (buffer) free(buffer);
                    buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION)malloc(
                        returnLength);

                    if (buffer == NULL) {
                        perror("zstd");
                        exit(1);
                    }
                } else {
                    /* some other error */
                    goto failed;
                }
            } else {
                done = TRUE;
            }
        }

        ptr = buffer;

        while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION)
            <= returnLength) {

            if (ptr->Relationship == RelationProcessorCore) {
                numPhysicalCores++;
            }

            ptr++;
            byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
        }

        free(buffer);

        return numPhysicalCores;
    }

failed:
    /* try to fall back on GetSystemInfo */
    {
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);
        numPhysicalCores = sysinfo.dwNumberOfProcessors;
        if (numPhysicalCores == 0) numPhysicalCores = 1; /* just in case */
    }
    return numPhysicalCores;
}

#elif defined(__APPLE__)

#include <sys/sysctl.h>

/* Use apple-provided syscall
 * see: man 3 sysctl */
int jet_util_countPhysicalCores(void) {
    static int32_t numPhysicalCores = 0; /* apple specifies int32_t */
    if (numPhysicalCores != 0) return numPhysicalCores;

    {
        size_t size = sizeof(int32_t);
        int const ret
            = sysctlbyname("hw.physicalcpu", &numPhysicalCores, &size, NULL, 0);
        if (ret != 0) {
            if (errno == ENOENT) {
                /* entry not present, fall back on 1 */
                numPhysicalCores = 1;
            } else {
                perror("zstd: can't get number of physical cpus");
                exit(1);
            }
        }

        return numPhysicalCores;
    }
}

#elif defined(__linux__)

/* parse /proc/cpuinfo
 * siblings / cpu cores should give hyperthreading ratio
 * otherwise fall back on sysconf */
int jet_util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;

    if (numPhysicalCores != 0) return numPhysicalCores;

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) {
        /* value not queryable, fall back on 1 */
        return numPhysicalCores = 1;
    }

    /* try to determine if there's hyperthreading */
    {
        FILE* const cpuinfo = fopen("/proc/cpuinfo", "r");
#define BUF_SIZE 80
        char buff[BUF_SIZE];

        int siblings = 0;
        int cpu_cores = 0;
        int ratio = 1;

        if (cpuinfo == NULL) {
            /* fall back on the sysconf value */
            return numPhysicalCores;
        }

        /* assume the cpu cores/siblings values will be constant across all
         * present processors */
        while (!feof(cpuinfo)) {
            if (fgets(buff, BUF_SIZE, cpuinfo) != NULL) {
                if (strncmp(buff, "siblings", 8) == 0) {
                    const char* const sep = strchr(buff, ':');
                    if (sep == NULL || *sep == '\0') {
                        /* formatting was broken? */
                        goto failed;
                    }

                    siblings = atoi(sep + 1);
                }
                if (strncmp(buff, "cpu cores", 9) == 0) {
                    const char* const sep = strchr(buff, ':');
                    if (sep == NULL || *sep == '\0') {
                        /* formatting was broken? */
                        goto failed;
                    }

                    cpu_cores = atoi(sep + 1);
                }
            } else if (ferror(cpuinfo)) {
                /* fall back on the sysconf value */
                goto failed;
            }
        }
        if (siblings && cpu_cores) { ratio = siblings / cpu_cores; }
    failed:
        fclose(cpuinfo);
        return numPhysicalCores = numPhysicalCores / ratio;
    }
}

#elif defined(__FreeBSD__)

#include <sys/param.h>
#include <sys/sysctl.h>

/* Use physical core sysctl when available
 * see: man 4 smp, man 3 sysctl */
int jet_util_countPhysicalCores(void) {
    static int numPhysicalCores = 0; /* freebsd sysctl is native int sized */
    if (numPhysicalCores != 0) return numPhysicalCores;

#if __FreeBSD_version >= 1300008
    {
        size_t size = sizeof(numPhysicalCores);
        int ret
            = sysctlbyname("kern.smp.cores", &numPhysicalCores, &size, NULL, 0);
        if (ret == 0) return numPhysicalCores;
        if (errno != ENOENT) {
            perror("zstd: can't get number of physical cpus");
            exit(1);
        }
        /* sysctl not present, fall through to older sysconf method */
    }
#endif

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) {
        /* value not queryable, fall back on 1 */
        numPhysicalCores = 1;
    }
    return numPhysicalCores;
}

#elif defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)    \
    || defined(__CYGWIN__)

/* Use POSIX sysconf
 * see: man 3 sysconf */
int jet_util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;

    if (numPhysicalCores != 0) return numPhysicalCores;

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) {
        /* value not queryable, fall back on 1 */
        return numPhysicalCores = 1;
    }
    return numPhysicalCores;
}

#else

int jet_util_countPhysicalCores(void) {
    /* assume 1 */
    return 1;
}

#endif
