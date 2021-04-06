// benchmarking either clock cycles or CPU time

/*
 * Copyright (c) 2016-2020, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license (see COPYING.md in
 * the root folder).
 */

/*-****************************************
 *  Dependencies
 ******************************************/
#include <time.h> /* clock_t, clock, CLOCKS_PER_SEC */

/*-****************************************
 *  Local Types
 ******************************************/

#if !defined(__VMS)                                                            \
    && (defined(__cplusplus)                                                   \
        || (defined(__STDC_VERSION__)                                          \
            && (__STDC_VERSION__ >= 199901L) /* C99 */))
#include <stdint.h>
typedef uint64_t PreciseTime; /* Precise Time */
#else
typedef unsigned long long
    PreciseTime; /* does not support compilers without long long support */
#endif

/*-****************************************
 *  Time functions
 ******************************************/
#if defined(_WIN32) /* Windows */

#include <windows.h> /* LARGE_INTEGER */
typedef LARGE_INTEGER clock_Time;
#define JET_CLOCK_TIME_INITIALIZER                                             \
    {                                                                          \
        { 0, 0 }                                                               \
    }

#elif defined(__APPLE__) && defined(__MACH__)

#include <mach/mach_time.h>
typedef PreciseTime clock_Time;
#define JET_CLOCK_TIME_INITIALIZER 0

/* C11 requires timespec_get, but FreeBSD 11 lacks it, while still claiming C11
   compliance. Android also lacks it but does define TIME_UTC. */
#elif (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L) /* C11 */)   \
    && defined(TIME_UTC) && !defined(__ANDROID__)

typedef struct timespec clock_Time;
#define JET_CLOCK_TIME_INITIALIZER                                             \
    { 0, 0 }

#else /* relies on standard C90 (note : clock_t measurements can be wrong when \
         using multi-threading) */

typedef clock_t clock_Time;
#define JET_CLOCK_TIME_INITIALIZER 0

#endif

clock_Time clock_getTime(void);
PreciseTime clock_getSpanTimeMicro(clock_Time clockStart, clock_Time clockEnd);
PreciseTime clock_getSpanTimeNano(clock_Time clockStart, clock_Time clockEnd);

#define SEC_TO_MICRO ((PreciseTime)1000000)
PreciseTime clock_clockSpanMicro(clock_Time clockStart);
PreciseTime clock_clockSpanNano(clock_Time clockStart);

void clock_waitForNextTick(void);

/*-****************************************
 *  Time functions
 ******************************************/

#if defined(_WIN32) /* Windows */

#include <stdlib.h> /* abort */
#include <stdio.h> /* perror */

clock_Time clock_getTime(void) {
    clock_Time x;
    QueryPerformanceCounter(&x);
    return x;
}

PreciseTime clock_getSpanTimeMicro(clock_Time clockStart, clock_Time clockEnd) {
    static LARGE_INTEGER ticksPerSecond;
    static int init = 0;
    if (!init) {
        if (!QueryPerformanceFrequency(&ticksPerSecond)) {
            perror("timefn::QueryPerformanceFrequency");
            abort();
        }
        init = 1;
    }
    return 1000000ULL * (clockEnd.QuadPart - clockStart.QuadPart)
        / ticksPerSecond.QuadPart;
}

PreciseTime clock_getSpanTimeNano(clock_Time clockStart, clock_Time clockEnd) {
    static LARGE_INTEGER ticksPerSecond;
    static int init = 0;
    if (!init) {
        if (!QueryPerformanceFrequency(&ticksPerSecond)) {
            perror("timefn::QueryPerformanceFrequency");
            abort();
        }
        init = 1;
    }
    return 1000000000ULL * (clockEnd.QuadPart - clockStart.QuadPart)
        / ticksPerSecond.QuadPart;
}

#elif defined(__APPLE__) && defined(__MACH__)

clock_Time clock_getTime(void) { return mach_absolute_time(); }

PreciseTime clock_getSpanTimeMicro(clock_Time clockStart, clock_Time clockEnd) {
    static mach_timebase_info_data_t rate;
    static int init = 0;
    if (!init) {
        mach_timebase_info(&rate);
        init = 1;
    }
    return (((clockEnd - clockStart) * (PreciseTime)rate.numer)
               / ((PreciseTime)rate.denom))
        / 1000ULL;
}

PreciseTime clock_getSpanTimeNano(clock_Time clockStart, clock_Time clockEnd) {
    static mach_timebase_info_data_t rate;
    static int init = 0;
    if (!init) {
        mach_timebase_info(&rate);
        init = 1;
    }
    return ((clockEnd - clockStart) * (PreciseTime)rate.numer)
        / ((PreciseTime)rate.denom);
}

/* C11 requires timespec_get, but FreeBSD 11 lacks it, while still claiming C11
   compliance. Android also lacks it but does define TIME_UTC. */
#elif (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L) /* C11 */)   \
    && defined(TIME_UTC) && !defined(__ANDROID__)

#include <stdlib.h> /* abort */
#include <stdio.h> /* perror */

clock_Time clock_getTime(void) {
    /* time must be initialized, othersize it may fail msan test.
     * No good reason, likely a limitation of timespec_get() for some target */
    clock_Time time = JET_CLOCK_TIME_INITIALIZER;
    if (timespec_get(&time, TIME_UTC) != TIME_UTC) {
        perror("timefn::timespec_get");
        abort();
    }
    return time;
}

static clock_Time clock_getSpanTime(clock_Time begin, clock_Time end) {
    clock_Time diff;
    if (end.tv_nsec < begin.tv_nsec) {
        diff.tv_sec = (end.tv_sec - 1) - begin.tv_sec;
        diff.tv_nsec = (end.tv_nsec + 1000000000ULL) - begin.tv_nsec;
    } else {
        diff.tv_sec = end.tv_sec - begin.tv_sec;
        diff.tv_nsec = end.tv_nsec - begin.tv_nsec;
    }
    return diff;
}

PreciseTime clock_getSpanTimeMicro(clock_Time begin, clock_Time end) {
    clock_Time const diff = clock_getSpanTime(begin, end);
    PreciseTime micro = 0;
    micro += 1000000ULL * diff.tv_sec;
    micro += diff.tv_nsec / 1000ULL;
    return micro;
}

PreciseTime clock_getSpanTimeNano(clock_Time begin, clock_Time end) {
    clock_Time const diff = clock_getSpanTime(begin, end);
    PreciseTime nano = 0;
    nano += 1000000000ULL * diff.tv_sec;
    nano += diff.tv_nsec;
    return nano;
}

#else /* relies on standard C90 (note : clock_t measurements can be wrong when \
         using multi-threading) */

clock_Time clock_getTime(void) { return clock(); }
PreciseTime clock_getSpanTimeMicro(clock_Time clockStart, clock_Time clockEnd) {
    return 1000000ULL * (clockEnd - clockStart) / CLOCKS_PER_SEC;
}
PreciseTime clock_getSpanTimeNano(clock_Time clockStart, clock_Time clockEnd) {
    return 1000000000ULL * (clockEnd - clockStart) / CLOCKS_PER_SEC;
}

#endif

/* returns time span in microseconds */
PreciseTime clock_clockSpanMicro(clock_Time clockStart) {
    clock_Time const clockEnd = clock_getTime();
    return clock_getSpanTimeMicro(clockStart, clockEnd);
}

/* returns time span in microseconds */
PreciseTime clock_clockSpanNano(clock_Time clockStart) {
    clock_Time const clockEnd = clock_getTime();
    return clock_getSpanTimeNano(clockStart, clockEnd);
}

void clock_waitForNextTick(void) {
    clock_Time const clockStart = clock_getTime();
    clock_Time clockEnd;
    do {
        clockEnd = clock_getTime();
    } while (clock_getSpanTimeNano(clockStart, clockEnd) == 0);
}

thread_local clock_Time __last_tic;

// tic() sets the saved timepoint to the current time. Use with a matching
// toc().
void tic() { __last_tic = clock_getTime(); }

// toc() prints the elapsed time from the previous tic().
void toc() {
    clock_Time now = clock_getTime();
    PreciseTime diffms = clock_getSpanTimeMicro(__last_tic, now);
    fprintf(stderr, "Elapsed time: %f ms\n", diffms / 1.0e3);
    // __last_tic = now;
}