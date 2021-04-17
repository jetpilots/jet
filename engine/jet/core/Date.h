//#include <stdio.h>
#include <time.h>
#include <assert.h>
#include "base.h"

typedef struct {
    int y;
    unsigned char tz, M, d, h, m, s, wd, dst;
} date_t;

typedef struct {
    int y, M, d, h, m, s;
} duration_t;

#include <langinfo.h>
int names() {
    const nl_item nl_abmons[12] = { ABMON_1, ABMON_2, ABMON_3, ABMON_4, ABMON_5,
        ABMON_6, ABMON_7, ABMON_8, ABMON_9, ABMON_10, ABMON_11, ABMON_12 };
    const nl_item nl_months[12] = { MON_1, MON_2, MON_3, MON_4, MON_5, MON_6,
        MON_7, MON_8, MON_9, MON_10, MON_11, MON_12 };
    int i;
    // setlocale(LC_ALL, "");// TODO: does this need to be enabled?
    for (i = 0; i < 12; i++) {
        printf("%d\t%s\t%s\n", i + 1, nl_langinfo(nl_abmons[i]),
            nl_langinfo(nl_months[i]));
    }
    return 0;
}
static const int saz = sizeof(duration_t);
static const int sz = sizeof(date_t);

static const char* const weekDayName[]
    = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

int monthdays(int M, int y) {
    switch (M % 12) {
    case 1: return 31;
    case 2: return 28 + !(y % 4);
    case 3: return 31;
    case 4: return 30;
    case 5: return 31;
    case 6: return 30;
    case 7: return 31;
    case 8: return 31;
    case 9: return 30;
    case 10: return 31;
    case 11: return 30;
    case 12:
    case 0: return 31;
    }
    return 1000000;
}

// duration_t mkDuration(int y, int M, int d, int h, int m,
// int s)

static const char* const tzLabels_[] = { "-1200", "-1130", "-1100", "-1030",
    "-1000", "-0930", "-0900", "-0830", "-0800", "-0730", "-0700", "-0630",
    "-0600", "-0530", "-0500", "-0430", "-0400", "-0330", "-0300", "-0230",
    "-0200", "-0130", "-0100", "-0030", "", "+0030", "+0100", "+0130", "+0200",
    "+0230", "+0300", "+0330", "+0400", "+0430", "+0500", "+0530", "+0600",
    "+0630", "+0700", "+0730", "+0800", "+0830", "+0900", "+0930", "+1000",
    "+1030", "+1100", "+1130", "+1200" };
static const char* const* const tzLabels = tzLabels_ + 24;
// you can index this with [-24 .. +24]

struct tm date_toTM(date_t base) {
    struct tm tmret = {
        .tm_year = base.y - 1900,
        .tm_mon = base.M - 1,
        .tm_mday = base.d,
        .tm_hour = base.h,
        .tm_min = base.m,
        .tm_sec = base.s,
        .tm_isdst = base.dst,
        .tm_wday = base.wd,
        .tm_gmtoff = base.tz * 1800 // 30-min intervals
    };
    return tmret; // mktime(&tmret);
}

date_t date_fromTM(struct tm* tmret) {
    // tmret->tm_isdst = -1;
    // mktime(tmret);
    return (date_t) {
        .y = tmret->tm_year + 1900,
        .M = tmret->tm_mon + 1,
        .d = tmret->tm_mday,
        .h = tmret->tm_hour,
        .m = tmret->tm_min,
        .s = tmret->tm_sec,
        .dst = tmret->tm_isdst ? (tmret->tm_isdst > 0 ? 1 : -1) : 0,
        .wd = tmret->tm_wday,
        .tz = tmret->tm_gmtoff / 1800 // 30-min intervals
    };
}

time_t date_toUnix(date_t base) {
    struct tm t = date_toTM(base);
    return mktime(&t);
}

date_t date_fromUnix(time_t t) {
    struct tm* tmret = gmtime(&t);
    return date_fromTM(tmret);
}

int date_equal(date_t a, date_t b) {
    time_t ta = date_toUnix(a);
    time_t tb = date_toUnix(b);
    return ta == tb;
}

void date_print(date_t base) {
    printf("date_t %s %04d-%02d-%02d %02d:%02d:%02d %s\n", weekDayName[base.wd],
        base.y, base.M, base.d, base.h, base.m, base.s, tzLabels[base.tz]);
}

void duration_print(duration_t dur) {
    printf("duration_t %dy %dM %dd %dh %dm %ds\n", dur.y, dur.M, dur.d, dur.h,
        dur.m, dur.s);
}

duration_t duration_new(
    int years, int months, int days, int hours, int minutes, int seconds) {
    duration_t diff = { .y = years,
        .M = months,
        .d = days,
        .h = hours,
        .m = minutes,
        .s = seconds };

    diff.m += diff.s / 60;
    diff.s %= 60;

    diff.h += diff.m / 60;
    diff.m %= 60;

    diff.d += diff.h / 24;
    diff.h %= 24;

    return diff;
}

date_t date_new(int year, int month, int day) {
    time_t tt = 0;
    struct tm tmt[1] = { {} };
    gmtime_r(&tt, tmt); // use gmtime to not care about timezones

    char buf[64];
    buf[63] = 0;

    tmt->tm_year = year - 1900;
    tmt->tm_mon = month - 1;
    tmt->tm_mday = day;
    tmt->tm_hour = tmt->tm_min = tmt->tm_sec = 0;
    tmt->tm_isdst = 0;

    tt = mktime(tmt);

    return date_fromTM(tmt);
}

date_t date_newWithTime(
    int year, int month, int day, int hour, int minute, int second) {
    date_t t = date_new(year, month, day);
    t.h = hour;
    t.m = minute;
    t.s = second;
    return t;
}

date_t date_add(date_t base, duration_t diff) {

    // save orig base here
    date_t orig = base;

    diff.y += diff.M / 12;
    diff.M %= 12;

    base.y += diff.y;
    base.M += diff.M;
    if (base.M > 12) {
        base.y += 1;
        base.M -= 12;
    }

    int mdays = monthdays(base.M, base.y);
    if (base.d > mdays) base.d = mdays;

    time_t dsec = diff.s + 60 * (diff.m + 60 * (diff.h + 24 * (diff.d)));

    time_t ttorig = date_toUnix(orig) + orig.tz * 1800;
    time_t ttbase = date_toUnix(base) + base.tz * 1800;
    time_t ttret = ttbase + dsec;

    date_t ret = date_fromUnix(ttret);

    ret.tz = base.tz;
    ret.dst = base.dst;

    // TODO: correction for leap days: go over all leap days starting from 29/2
    // in orig.year and check if it falls within the interval [ orig ... new ].
    // if so add a day (check overflow, also for months if > 30 days added...)

    // snap
    // int mdays = monthdays(ret.M, ret.y);
    // if (ret.d > mdays) ret.d = mdays;

    return ret;
}

date_t date_now() { return date_fromUnix(time(NULL)); }

#define TEST_ADD(by, bm, bd, dy, dM, dd, ey, em, ed)                           \
    TEST_ADDT(by, bm, bd, dy, dM, dd, 0, 0, 0, ey, em, ed)

#define TEST_ADDT(by, bm, bd, dy, dM, dd, dh, dm, ds, ey, em, ed)              \
    lineno = __LINE__;                                                         \
    ntot++;                                                                    \
    dur = duration_new(dy, dM, dd, dh, dm, ds);                                \
    base = date_new(by, bm, bd);                                               \
    result = date_add(base, dur);                                              \
    expected = date_new(ey, em, ed);                                           \
    if (!date_equal(result, expected)) {                                       \
        printf("\n--- test #%d failed at ./%s:%d:\n", ntot, __FILE__, lineno); \
        printf("dur  ");                                                       \
        duration_print(dur);                                                   \
        printf("base ");                                                       \
        date_print(base);                                                      \
        printf("need ");                                                       \
        date_print(expected);                                                  \
        printf("got  ");                                                       \
        date_print(result);                                                    \
        nfail++;                                                               \
    }
int maian() {
    date_t base;
    int lineno, ntot = 0, nfail = 0;
    duration_t dur;

    printf("now:  ");
    date_print(date_now());
    date_t result, expected;

    // base = (date_t) { .y = 2020, .M = 1, .d = 1 };
    TEST_ADD(2020, 1, 1, 1, 0, 0, 2021, 1, 1)
    TEST_ADD(2021, 1, 1, 1, 0, 0, 2022, 1, 1)
    TEST_ADD(2020, 1, 1, 0, 1, 0, 2020, 2, 1)
    TEST_ADD(2020, 1, 1, 0, 0, 1, 2020, 1, 2)

    TEST_ADD(2020, 2, 28, 0, 0, 1, 2020, 2, 29)
    TEST_ADD(2021, 2, 28, 0, 0, 1, 2021, 3, 1)

    TEST_ADD(2020, 2, 28, 0, 1, 0, 2020, 3, 28)
    TEST_ADD(2021, 2, 28, 0, 1, 0, 2021, 3, 28)

    TEST_ADD(2020, 1, 30, 0, 1, 0, 2020, 2, 29)
    TEST_ADD(2020, 1, 30, 0, 1, 5, 2020, 3, 5)
    TEST_ADD(2020, 3, 30, 0, 1, 5, 2020, 5, 5)
    TEST_ADD(2020, 4, 30, 0, 1, 5, 2020, 6, 4)

    TEST_ADD(2020, 1, 30, 0, 2, 0, 2020, 3, 30)

    TEST_ADD(2020, 5, 31, 0, 1, 0, 2020, 6, 30)

    TEST_ADD(2020, 1, 1, 0, 0, 390, 2021, 1, 25)
    TEST_ADD(2021, 1, 1, 0, 0, 390, 2022, 1, 26)

    TEST_ADD(2020, 1, 1, 0, 32, 390, 2023, 9, 26)
    TEST_ADD(2021, 1, 1, 0, 32, 390, 2024, 9, 25)

    TEST_ADD(2020, 2, 29, 1, 0, 0, 2021, 2, 28)
    TEST_ADDT(2020, 2, 29, 0, 0, 0, 0, 0, 365 * 24 * 60 * 60, 2021, 2, 28)

    if (nfail) printf("\n--- %d of %d tests failed.\n", nfail, ntot);
    return 0;
}
