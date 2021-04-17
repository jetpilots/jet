#include <complex.h>
typedef double complex complex_t;
complex_t complex_new(double re, double im) { return CMPLX(re, im); }
complex_t complex_acos(complex_t c) { return cacos(c); };
complex_t complex_asin(complex_t c) { return casin(c); };
complex_t complex_atan(complex_t c) { return catan(c); };
complex_t complex_cos(complex_t c) { return ccos(c); };
complex_t complex_sin(complex_t c) { return csin(c); };
complex_t complex_tan(complex_t c) { return ctan(c); };
complex_t complex_acosh(complex_t c) { return cacosh(c); };
complex_t complex_asinh(complex_t c) { return casinh(c); };
complex_t complex_atanh(complex_t c) { return catanh(c); };
complex_t complex_cosh(complex_t c) { return ccosh(c); };
complex_t complex_sinh(complex_t c) { return csinh(c); };
complex_t complex_tanh(complex_t c) { return ctanh(c); };
complex_t complex_exp(complex_t c) { return cexp(c); };
complex_t complex_log(complex_t c) { return clog(c); };
complex_t complex_pow(complex_t c, complex_t p) { return cpow(c, p); };
complex_t complex_sqrt(complex_t c) { return csqrt(c); };
complex_t complex_conj(complex_t c) { return conj(c); };
complex_t complex_proj(complex_t c) { return cproj(c); };
double complex_abs(complex_t c) { return cabs(c); };
double complex_arg(complex_t c) { return carg(c); };
double complex_imag(complex_t c) { return cimag(c); };
double complex_real(complex_t c) { return creal(c); };

typedef float complex complex4_t;
static inline complex4_t complex4_new(double re, double im) {
    return CMPLX(re, im);
}
static inline complex4_t complex4_acos(complex4_t c) { return cacosf(c); };
static inline complex4_t complex4_asin(complex4_t c) { return casinf(c); };
static inline complex4_t complex4_atan(complex4_t c) { return catanf(c); };
static inline complex4_t complex4_cos(complex4_t c) { return ccosf(c); };
static inline complex4_t complex4_sin(complex4_t c) { return csinf(c); };
static inline complex4_t complex4_tan(complex4_t c) { return ctanf(c); };
static inline complex4_t complex4_acosh(complex4_t c) { return cacoshf(c); };
static inline complex4_t complex4_asinh(complex4_t c) { return casinhf(c); };
static inline complex4_t complex4_atanh(complex4_t c) { return catanhf(c); };
static inline complex4_t complex4_cosh(complex4_t c) { return ccoshf(c); };
static inline complex4_t complex4_sinh(complex4_t c) { return csinhf(c); };
static inline complex4_t complex4_tanh(complex4_t c) { return ctanhf(c); };
static inline complex4_t complex4_exp(complex4_t c) { return cexpf(c); };
static inline complex4_t complex4_log(complex4_t c) { return clogf(c); };
static inline complex4_t complex4_pow(complex4_t c, complex4_t p) {
    return cpowf(c, p);
};
static inline complex4_t complex4_sqrt(complex4_t c) { return csqrtf(c); };
static inline complex4_t complex4_conj(complex4_t c) { return conjf(c); };
static inline complex4_t complex4_proj(complex4_t c) { return cprojf(c); };
static inline float complex4_abs(complex4_t c) { return cabsf(c); };
static inline float complex4_arg(complex4_t c) { return cargf(c); };
static inline float complex4_imag(complex4_t c) { return cimagf(c); };
static inline float complex4_real(complex4_t c) { return crealf(c); };

int psz() {
    printf("int: %zu\n", sizeof(int));
    printf("char: %zu\n", sizeof(char));
    printf("long: %zu\n", sizeof(long));
    printf("double: %zu\n", sizeof(double));
    printf("float: %zu\n", sizeof(float));
    printf("unsigned int: %zu\n", sizeof(unsigned int));
    printf("unsigned: %zu\n", sizeof(unsigned));
    printf("unsigned long: %zu\n", sizeof(unsigned long));
    printf("long long: %zu\n", sizeof(long long));
    printf("long long int: %zu\n", sizeof(long long int));
    printf("short: %zu\n", sizeof(short));
    return 0;
}