#include <stdio.h>
#include <math.h>
#define MAKE_IPOW_FUNCS(T, TT)                                                 \
    static T ipow2##TT(T x) { return x * x; };                                 \
    static T ipow3##TT(T x) { return x * x * x; }                              \
    static T ipow4##TT(T x) {                                                  \
        x = ipow2##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow5##TT(T x) { return ipow4##TT(x) * x; }                       \
    static T ipow6##TT(T x) {                                                  \
        x = ipow3##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow7##TT(T x) { return ipow6##TT(x) * x; }                       \
    static T ipow8##TT(T x) {                                                  \
        x = ipow4##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow9##TT(T x) { return ipow8##TT(x) * x; }                       \
    static T ipow10##TT(T x) {                                                 \
        x = ipow5##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow11##TT(T x) { return ipow10##TT(x) * x; }                     \
    static T ipow12##TT(T x) {                                                 \
        x = ipow6##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow13##TT(T x) { return ipow12##TT(x) * x; }                     \
    static T ipow14##TT(T x) {                                                 \
        x = ipow7##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
    static T ipow15##TT(T x) {                                                 \
        x = ipow5##TT(x);                                                      \
        return x * x * x;                                                      \
    }                                                                          \
    static T ipow16##TT(T x) {                                                 \
        x = ipow8##TT(x);                                                      \
        return x * x;                                                          \
    }                                                                          \
                                                                               \
    T ipow##TT(T x, int p) {                                                   \
        switch (p) {                                                           \
        case 0:                                                                \
            return 1;                                                          \
        case 1:                                                                \
            return x;                                                          \
        case 2:                                                                \
            return ipow2##TT(x);                                               \
        case 3:                                                                \
            return ipow3##TT(x);                                               \
        case 4:                                                                \
            return ipow4##TT(x);                                               \
        case 5:                                                                \
            return ipow5##TT(x);                                               \
        case 6:                                                                \
            return ipow6##TT(x);                                               \
        case 7:                                                                \
            return ipow7##TT(x);                                               \
        case 8:                                                                \
            return ipow8##TT(x);                                               \
        case 9:                                                                \
            return ipow9##TT(x);                                               \
        case 10:                                                               \
            return ipow10##TT(x);                                              \
        case 11:                                                               \
            return ipow11##TT(x);                                              \
        case 12:                                                               \
            return ipow12##TT(x);                                              \
        case 13:                                                               \
            return ipow13##TT(x);                                              \
        case 14:                                                               \
            return ipow14##TT(x);                                              \
        case 15:                                                               \
            return ipow15##TT(x);                                              \
        case 16:                                                               \
            return ipow16##TT(x);                                              \
        default:                                                               \
            return pow(x, p);                                                  \
        }                                                                      \
    }
//     T ans = x;
//     case 16:
//         x *= x;
//     case 8:
//         x *= x;
//     case 4:
//         x *= x;
//     case 2:
//         x *= x;
//         return x;
//         // ans = x;
//     case 0:
//         return 1;
//         // break;

//     case 9:
//         x *= x;
//     case 7:
//         ans *= x;
//     case 6:
//         ans *= x;
//     case 5:
//         x *= x;
//     case 3:
//         x *= x;
//         ans *= x;
//     case 1:
//         return ans;

//     default:
//         return 9999; // pow(x, p);
//     }
// }
MAKE_IPOW_FUNCS(double, f)
MAKE_IPOW_FUNCS(int, i)

static double fpowf(double x, double p) {
    double rp = round(p);
    return fabs(p - rp) < 1e-14 ? ipowf(x, rp) : pow(x, p);
}

int main() {
    int c = getc(stdin);
    for (int i = 0; i <= 18000000; i++)
        printf("%d^%d = %f\n", 3, i, fpowf(c, 6.5));
}
