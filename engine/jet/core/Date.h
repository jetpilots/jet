//#include <stdio.h>
#include <time.h>
#include <assert.h>
#include "base.h"

typedef struct {
    int y;
    unsigned char tz, M, d, h, m, s, wd, dst;
} Date;

typedef struct {
    int y, M, d, h, m, s;
} Duration;

#include <langinfo.h>
// #include <locale.h>
// #include <stdio.h>
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
// typedef struct {
//    int y : 26, tz : 6;
//    unsigned int M : 4, d : 5, h : 5, m : 6, s : 6, wd : 3, dst : 2,
//        _reserved_ : 1;
//} Date;

// This works only for the 8B Date structure

// typedef struct {
//     int y;
//     int M : 5, d : 6, h : 6, m : 7, s : 7,
//         _reserved_ : 1;
// } Duration;

static const int saz = sizeof(Duration);
static const int sz = sizeof(Date);
// static const char* const weekDayName(int wd)
// {
//     switch (wd) {
//     case 0:
//         return "Sun";
//     case 1:
//         return "Mon";
//     case 2:
//         return "Tue";
//     case 3:
//         return "Wed";
//     case 4:
//         return "Thu";
//     case 5:
//         return "Fri";
//     case 6:
//         return "Sat";
//     }
//     return "";
// }
static const char* const weekDayName[]
    = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

int monthdays(int M, int y) {
    switch (M % 12) {
    case 1:
        return 31;
    case 2:
        return 28 + !(y % 4);
    case 3:
        return 31;
    case 4:
        return 30;
    case 5:
        return 31;
    case 6:
        return 30;
    case 7:
        return 31;
    case 8:
        return 31;
    case 9:
        return 30;
    case 10:
        return 31;
    case 11:
        return 30;
    case 12:
    case 0:
        return 31;
    }
    return 1000000;
}

// Duration mkDuration(int y, int M, int d, int h, int m,
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

struct tm Date_toTM(Date base) {
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

Date Date_fromTM(struct tm* tmret) {
    // tmret->tm_isdst = -1;
    // mktime(tmret);
    return (Date) {
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

time_t Date_toUnix(Date base) {
    struct tm t = Date_toTM(base);
    return mktime(&t);
}

Date Date_fromUnix(time_t t) {
    struct tm* tmret = gmtime(&t);
    return Date_fromTM(tmret);
}

int Date_equal(Date a, Date b) {
    // int64_t *aa = &a, *bb = &b;
    // return *aa == *bb;

    time_t ta = Date_toUnix(a); //+a.tz*1800;
    time_t tb = Date_toUnix(b); //+b.tz*1800;
    return ta == tb;
    //    return a.y == b.y && a.M == b.M && a.d == b.d && a.h == b.h && a.m ==
    //    b.m
    //    && a.s == b.s; //&& a.tz == b.tz;
}

void Date_print(Date base) {
    printf("Date %s %04d-%02d-%02d %02d:%02d:%02d %s\n",
        weekDayName[base.wd], base.y, base.M, base.d, base.h, base.m, base.s,
        tzLabels[base.tz]);

    // struct tm t = Date_toTM(base);
    // // {
    // //     .tm_year = base.y - 1900,
    // //     .tm_mon = base.M - 1,
    // //     .tm_mday = base.d,
    // //     .tm_hour = base.h,
    // //     .tm_min = base.m,
    // //     .tm_sec = base.s,
    // //     .tm_isdst = base.dst,
    // //     .tm_wday = base.wd,
    // //     .tm_gmtoff = base.tz * 1800 // 30-min intervals
    // // };
    // char buf[64];
    // buf[63] = 0;
    // strftime(buf, 63, "%c (%Z)\n", &t);
    // printf("%s", buf);
}

void Duration_print(Duration dur) {
    printf("Duration %dy %dM %dd %dh %dm %ds\n", dur.y, dur.M, dur.d, dur.h,
        dur.m, dur.s);
}

Duration Duration_new(
    int years, int months, int days, int hours, int minutes, int seconds) {
    Duration diff = { .y = years,
        .M = months,
        .d = days,
        .h = hours,
        .m = minutes,
        .s = seconds };
    // if (diff.s >= 60) {
    diff.m += diff.s / 60;
    diff.s %= 60;
    // }
    // if (diff.m >= 60) {
    diff.h += diff.m / 60;
    diff.m %= 60;
    // }
    // if (diff.h >= 24) {
    diff.d += diff.h / 24;
    diff.h %= 24;
    // }

    return diff;
}

/*
Date dateAdd(Date base, Duration diff)
{
    if (diff.s >= 60) {
        diff.m += diff.s / 60;
        diff.s %= 60;
    }
    if (diff.m >= 60) {
        diff.h += diff.m / 60;
        diff.m %= 60;
    }
    if (diff.h >= 24) {
        diff.d += diff.h / 24;
        diff.h %= 24;
    }

    const int daysIn4Years = 365 * 4 + 1;
    if (diff.d >= daysIn4Years) {
        diff.y += 4 * (diff.d / daysIn4Years);
        diff.d -= daysIn4Years;
    }

    int ytmp = base.y;
    int daysIn1Year = 365 + !(ytmp % 4);
    while (diff.d >= daysIn1Year) {
        diff.y += 1;
        diff.d -= daysIn1Year;
        daysIn1Year = 365 + !(ytmp % 4);
    }

    int mdays = monthdays(base.M + diff.M, base.y + diff.y);
    while (diff.d >= mdays) {
        diff.M += 1;
        mdays = monthdays(base.M + diff.M, base.y + diff.y);
        diff.d -= mdays;
    }

    if (diff.M >= 12) {
        diff.y += diff.M / 12;
        diff.M %= 12;
    }
    // Duration_print(diff);

    int s = base.s + diff.s;

    int min = base.m + diff.m + (s >= 60);
    if (s >= 60) s -= 60;

    int h = base.h + diff.h + (min >= 60);
    if (min >= 60) min -= 60;

    int d = base.d + diff.d + (h >= 24);
    if (h >= 24) h -= 24;

    // mdays = monthdays(base.M + diff.M, base.y + diff.y);
    int M = base.M + diff.M + (d > mdays);
    if (d > mdays) d -= mdays;

    // int M = base.M + diff.M; //+ (d > mdays);
    // if (d > mdays) d = mdays;

    int y = base.y + diff.y + (M >= 12);
    int extradays = (diff.y + (M >= 12)) / 4;
    if (M >= 12) M -= 12;

    // if (extradays && diff.d) {
    //     // because you should move the d ahead only if
    //     // d-delta was specified. Perhaps also h-delta?
    //     d += extradays;
    //     // mdays = ...;
    //     M += (d >= mdays);
    //     if (d >= mdays) d -= mdays;
    //     y += (M >= 12);
    //     if (M >= 12) M -= 12;
    // }

    Date ret = {
        //
        .y = y,
        .M = M,
        .d = d,
        .h = h,
        .m = min,
        .s = s,
    };

    return ret;
}
*/

Date Date_new(int year, int month, int day) {
    time_t tt = 0;
    struct tm tmt[1] = { {} };
    gmtime_r(&tt, tmt); // use gmtime to not care about timezones

    char buf[64];
    buf[63] = 0;
    // strftime(buf, 63, "%c (%Z)\n", tmt);
    // printf(".  %s", buf);

    tmt->tm_year = year - 1900;
    tmt->tm_mon = month - 1;
    tmt->tm_mday = day;
    tmt->tm_hour = tmt->tm_min = tmt->tm_sec = 0;
    tmt->tm_isdst = 0;

    tt = mktime(tmt);
    // strftime(buf, 63, "%c (%Z)\n", tmt);
    // printf(".. %s", buf);

    return Date_fromTM(tmt);
}

Date Date_newWithTime(
    int year, int month, int day, int hour, int minute, int second) {
    Date t = Date_new(year, month, day);
    t.h = hour;
    t.m = minute;
    t.s = second;
    return t;
}

Date Date_add(Date base, Duration diff) {

    // save orig base here
    Date orig = base;

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

    time_t dsec = //
        diff.s
        + 60
            * (diff.m
                + 60
                    * (diff.h
                        + 24 * (diff.d /*+ 365 * (diff.y) + (diff.y / 4)*/)));

    // int yy = base.y + diff.y;
    // int mm = base.M + diff.M;
    // if (mm > 12) {
    //     diff.y++;
    //     mm -= 12;
    // }
    // if (diff.y)
    //     dsec += 60 * 60 * 24
    //         * (base.y % 4 == 0 || (base.y % 4 + diff.y % 4 >= 4));
    // int month = base.M, year = base.y;
    // for (int i = 0; i < diff.M; i++) {
    //     int mdays = monthdays(month, year);
    //     dsec += mdays * 24 * 60 * 60;
    //     // year += !(++month % 12);
    //     month += 1;
    //     if (month > 12) {
    //         month -= 12;
    //         year += 1;
    //     }
    // }

    // struct tm t = Date_toTM(base);

    // time_t baset = mktime(&t);
    // // printf("  %15ld\n+ %15ld\n= %15ld\n", baset, dsec, baset + dsec);
    // time_t tmtret = baset + dsec;
    // struct tm* tmret = gmtime(&tmtret);

    time_t ttorig = Date_toUnix(orig) + orig.tz * 1800;
    time_t ttbase = Date_toUnix(base) + base.tz * 1800;
    time_t ttret = ttbase + dsec;

    Date ret = Date_fromUnix(ttret);
    //    ret.tz = base.tz;
    //    ret.dst = base.dst;

    //    ret.h += ret.tz / 2;
    //    ret.m += (ret.tz % 2) * 30;
    //    ret.d += ret.h / 24;
    //    ret.h %= 24;
    // ret.M += diff.M; // this is a special treatment
    // ret.M = mm;
    // ret.y = yy;
    // if (0&&diff.d>1){
    //    time_t ttleap=0;
    //   for (int yy = orig.y - (orig.y%4);yy<=ret.y;yy+=4) {
    //        time_t feb29 = Date_toUnix(Date_new(yy,2,29));
    //        if (feb29>=ttorig && feb29<=ttret) ttleap+=24*60*60;
    //    }
    //
    //    ttret +=ttleap;
    //     ret = Date_fromUnix(ttret);
    //}
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

Date Date_now() {
    // time_t tmt[] = { time(NULL) };
    // struct tm* tmnow = gmtime(tmt);
    // int s = sizeof(struct tm);
    // printf("%s %ld\n", tmnow->tm_zone, tmnow->tm_gmtoff);
    // long tz = tmnow->tm_gmtoff / 1800;

    // struct tm is anyway static inernal storage in gmtime() which will be
    // overwritten, so it is not a bad idea to have our own (much smaller)
    // struct instead of just wrapping a struct tm
    return Date_fromUnix(time(NULL));
    // {
    //     .y = tmnow->tm_year + 1900,
    //     .M = tmnow->tm_mon + 1,
    //     .d = tmnow->tm_mday,
    //     .h = tmnow->tm_hour,
    //     .m = tmnow->tm_min,
    //     .s = tmnow->tm_sec,
    //     .dst = tmnow->tm_isdst ? (tmnow->tm_isdst > 0 ? 1 : -1) : 0,
    //     .wd = tmnow->tm_wday,
    //     .tz = tmnow->tm_gmtoff / 1800 // 30-min intervals
    // };

    // return strftime
}

// #define TEST_DATEADD(durn, exp)                                                \
//     dur = durn;                                                                \
//     result = dateAdd(base, dur);                                               \
//     expected = exp;                                                            \
//     if (!Date_equal(result, expected)) {                                   \
//         printf("\n--- assertion failed:\n");                                   \
//         printf("base ");                                                       \
//         Date_print(base);                                                       \
//         printf("dur  ");                                                       \
//         Duration_print(dur);                                                    \
//         printf("need ");                                                       \
//         Date_print(expected);                                                   \
//         printf("got  ");                                                       \
//         Date_print(result);                                                     \
//     }

#define TEST_ADD(by, bm, bd, dy, dM, dd, ey, em, ed)                           \
    TEST_ADDT(by, bm, bd, dy, dM, dd, 0, 0, 0, ey, em, ed)

#define TEST_ADDT(by, bm, bd, dy, dM, dd, dh, dm, ds, ey, em, ed)              \
    lineno = __LINE__;                                                         \
    ntot++;                                                                    \
    dur = Duration_new(dy, dM, dd, dh, dm, ds);                                \
    base = Date_new(by, bm, bd);                                           \
    result = Date_add(base, dur);                                          \
    expected = Date_new(ey, em, ed);                                       \
    if (!Date_equal(result, expected)) {                                   \
        printf("\n--- test #%d failed at ./%s:%d:\n", ntot, __FILE__, lineno); \
        printf("dur  ");                                                       \
        Duration_print(dur);                                                   \
        printf("base ");                                                       \
        Date_print(base);                                                  \
        printf("need ");                                                       \
        Date_print(expected);                                              \
        printf("got  ");                                                       \
        Date_print(result);                                                \
        nfail++;                                                               \
    }
int maian() {
    Date base; // = { //
    //     .y = 2016,
    //     .M = 5,
    //     .d = 27,
    //     .h = 12,
    //     .m = 15,
    //     .s = 40
    // };
    int lineno, ntot = 0, nfail = 0;
    Duration dur; //= { .y = 4, .h = 48, .d = 363 };
    // Duration_print(dur);
    // Date next = Date_add(base, dur);

    // Date_print(base);
    // Date_print(next);
    printf("now:  ");
    Date_print(Date_now());
    Date result, expected;

    // base = (Date) { .y = 2020, .M = 1, .d = 1 };
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

    // base = (Date) { .y = 2020, .M = 2, .d = 28 };
    // TEST_DATEADD(
    //     ((Duration) { .d = 1 }), ((Date) { .y = 2020, .M = 2, .d = 29 }))
    // TEST_DATEADD(
    //     ((Duration) { .M = 1 }), ((Date) { .y = 2020, .M = 3, .d = 28 }))

    // base = (Date) { .y = 2020, .M = 1, .d = 30 };
    // TEST_DATEADD(
    //     ((Duration) { .M = 1 }), ((Date) { .y = 2020, .M = 2, .d = 29 }))

    // base = (Date) { .y = 2020, .M = 1, .d = 30 };
    // TEST_DATEADD(((Duration) { .M = 1, .d = 5 }),
    //     ((Date) { .y = 2020, .M = 3, .d = 1 }))

    // base = (Date) { .y = 2020, .M = 5, .d = 31 };
    // TEST_DATEADD(
    //     ((Duration) { .M = 1 }), ((Date) { .y = 2020, .M = 6, .d = 30 }))
    if (nfail) printf("\n--- %d of %d tests failed.\n", nfail, ntot);
    return 0;
}
