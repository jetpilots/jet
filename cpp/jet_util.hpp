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

#include "platform.h"
#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../lib/common/mem.h"

/*-************************************************************
 * Avoid fseek()'s 2GiB barrier with MSVC, macOS, *BSD, MinGW
 ***************************************************************/
#if defined(_MSC_VER) && (_MSC_VER >= 1400)
#define util_fseek _fseeki64
#elif !defined(__64BIT__) && (PLATFORM_POSIX_VERSION >= 200112L)
#define util_fseek fseeko
#elif defined(__MINGW32__) && defined(__MSVCRT__) && !defined(__STRICT_ANSI__) \
    && !defined(__NO_MINGW_LFS)
#define util_fseek fseeko64
#else
#define util_fseek fseek
#endif

/*-*************************************************
 *  Sleep & priority functions: Windows - Posix - others
 ***************************************************/
#if defined(_WIN32)
#include <windows.h>
#define SET_REALTIME_PRIORITY                                                  \
    SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS)
#define util_sleep(s) Sleep(1000 * s)
#define util_sleepMilli(milli) Sleep(milli)

#elif PLATFORM_POSIX_VERSION > 0
#include <unistd.h>
#define util_sleep(s) sleep(s)
#if ZSTD_NANOSLEEP_SUPPORT
#define util_sleepMilli(milli)                                                 \
    {                                                                          \
        struct timespec t;                                                     \
        t.tv_sec = 0;                                                          \
        t.tv_nsec = milli * 1000000ULL;                                        \
        nanosleep(&t, NULL);                                                   \
    }
#else
#define util_sleepMilli(milli)
#endif
#if ZSTD_SETPRIORITY_SUPPORT
#include <sys/resource.h>
#define SET_REALTIME_PRIORITY setpriority(PRIO_PROCESS, 0, -20)
#else
#define SET_REALTIME_PRIORITY
#endif

#else
#define util_sleep(s)
#define util_sleepMilli(milli)
#define SET_REALTIME_PRIORITY
#endif

/*-****************************************
 *  Compiler specifics
 ******************************************/
#if defined(__INTEL_COMPILER)
#pragma warning(                                                               \
    disable : 177) /* disable: message #177: function was declared but never   \
                      referenced, useful with util_STATIC */
#endif
#if defined(__GNUC__)
#define util_STATIC static __attribute__((unused))
#elif defined(__cplusplus)                                                     \
    || (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L))
#define util_STATIC static inline
#elif defined(_MSC_VER)
#define util_STATIC static __inline
#else
#define util_STATIC                                                            \
    static /* this version may generate warnings for unused static functions;  \
              disable the relevant warning */
#endif

/*-****************************************
 *  count the number of physical cores
 ******************************************/

#if defined(_WIN32) || defined(WIN32)

#include <windows.h>

typedef BOOL(WINAPI* LPFN_GLPI)(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION, PDWORD);

int util_countPhysicalCores(void) {
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

#pragma warning(disable : 4054)
#pragma warning(disable : 4055)
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

failed :

{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    numPhysicalCores = sysinfo.dwNumberOfProcessors;
    if (numPhysicalCores == 0) numPhysicalCores = 1;
}
    return numPhysicalCores;
}

#elif defined(__APPLE__)

#include <sys/sysctl.h>

/* Use apple-provided syscall
 * see: man 3 sysctl */
int util_countPhysicalCores(void) {
    static int32_t numPhysicalCores = 0;
    if (numPhysicalCores != 0) return numPhysicalCores;

    {
        size_t size = sizeof(int32_t);
        int const ret
            = sysctlbyname("hw.physicalcpu", &numPhysicalCores, &size, NULL, 0);
        if (ret != 0) {
            if (errno == ENOENT) {

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
int util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;

    if (numPhysicalCores != 0) return numPhysicalCores;

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) { return numPhysicalCores = 1; }

    {
        FILE* const cpuinfo = fopen("/proc/cpuinfo", "r");
#define BUF_SIZE 80
        char buff[BUF_SIZE];

        int siblings = 0;
        int cpu_cores = 0;
        int ratio = 1;

        if (cpuinfo == NULL) { return numPhysicalCores; }

        /* assume the cpu cores/siblings values will be constant across all
         * present processors */
        while (!feof(cpuinfo)) {
            if (fgets(buff, BUF_SIZE, cpuinfo) != NULL) {
                if (strncmp(buff, "siblings", 8) == 0) {
                    const char* const sep = strchr(buff, ':');
                    if (sep == NULL || *sep == '\0') { goto failed; }

                    siblings = atoi(sep + 1);
                }
                if (strncmp(buff, "cpu cores", 9) == 0) {
                    const char* const sep = strchr(buff, ':');
                    if (sep == NULL || *sep == '\0') { goto failed; }

                    cpu_cores = atoi(sep + 1);
                }
            } else if (ferror(cpuinfo)) {

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
int util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;
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
    }
#endif

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) { numPhysicalCores = 1; }
    return numPhysicalCores;
}

#elif defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)    \
    || defined(__CYGWIN__)

/* Use POSIX sysconf
 * see: man 3 sysconf */
int util_countPhysicalCores(void) {
    static int numPhysicalCores = 0;

    if (numPhysicalCores != 0) return numPhysicalCores;

    numPhysicalCores = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (numPhysicalCores == -1) { return numPhysicalCores = 1; }
    return numPhysicalCores;
}

#else

int util_countPhysicalCores(void) { return 1; }

#endif
