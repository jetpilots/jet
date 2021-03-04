#include <complex.h>
typedef double complex Complex;
Complex Complex_new(double re, double im) { return CMPLX(re, im); }
Complex Complex_acos(Complex c) { return cacos(c); };
Complex Complex_asin(Complex c) { return casin(c); };
Complex Complex_atan(Complex c) { return catan(c); };
Complex Complex_cos(Complex c) { return ccos(c); };
Complex Complex_sin(Complex c) { return csin(c); };
Complex Complex_tan(Complex c) { return ctan(c); };
Complex Complex_acosh(Complex c) { return cacosh(c); };
Complex Complex_asinh(Complex c) { return casinh(c); };
Complex Complex_atanh(Complex c) { return catanh(c); };
Complex Complex_cosh(Complex c) { return ccosh(c); };
Complex Complex_sinh(Complex c) { return csinh(c); };
Complex Complex_tanh(Complex c) { return ctanh(c); };
Complex Complex_exp(Complex c) { return cexp(c); };
Complex Complex_log(Complex c) { return clog(c); };
Complex Complex_pow(Complex c, Complex p) { return cpow(c, p); };
Complex Complex_sqrt(Complex c) { return csqrt(c); };
Complex Complex_conj(Complex c) { return conj(c); };
Complex Complex_proj(Complex c) { return cproj(c); };
Real Complex_abs(Complex c) { return cabs(c); };
Real Complex_arg(Complex c) { return carg(c); };
Real Complex_imag(Complex c) { return cimag(c); };
Real Complex_real(Complex c) { return creal(c); };

typedef float complex Complex4;
static inline Complex4 Complex4_new(double re, double im) {
    return CMPLX(re, im);
}
static inline Complex4 Complex4_acos(Complex4 c) { return cacosf(c); };
static inline Complex4 Complex4_asin(Complex4 c) { return casinf(c); };
static inline Complex4 Complex4_atan(Complex4 c) { return catanf(c); };
static inline Complex4 Complex4_cos(Complex4 c) { return ccosf(c); };
static inline Complex4 Complex4_sin(Complex4 c) { return csinf(c); };
static inline Complex4 Complex4_tan(Complex4 c) { return ctanf(c); };
static inline Complex4 Complex4_acosh(Complex4 c) { return cacoshf(c); };
static inline Complex4 Complex4_asinh(Complex4 c) { return casinhf(c); };
static inline Complex4 Complex4_atanh(Complex4 c) { return catanhf(c); };
static inline Complex4 Complex4_cosh(Complex4 c) { return ccoshf(c); };
static inline Complex4 Complex4_sinh(Complex4 c) { return csinhf(c); };
static inline Complex4 Complex4_tanh(Complex4 c) { return ctanhf(c); };
static inline Complex4 Complex4_exp(Complex4 c) { return cexpf(c); };
static inline Complex4 Complex4_log(Complex4 c) { return clogf(c); };
static inline Complex4 Complex4_pow(Complex4 c, Complex4 p) {
    return cpowf(c, p);
};
static inline Complex4 Complex4_sqrt(Complex4 c) { return csqrtf(c); };
static inline Complex4 Complex4_conj(Complex4 c) { return conjf(c); };
static inline Complex4 Complex4_proj(Complex4 c) { return cprojf(c); };
static inline Real32 Complex4_abs(Complex4 c) { return cabsf(c); };
static inline Real32 Complex4_arg(Complex4 c) { return cargf(c); };
static inline Real32 Complex4_imag(Complex4 c) { return cimagf(c); };
static inline Real32 Complex4_real(Complex4 c) { return crealf(c); };

int psz() {
    printf("int: %lu\n", sizeof(int));
    printf("char: %lu\n", sizeof(char));
    printf("long: %lu\n", sizeof(long));
    printf("double: %lu\n", sizeof(double));
    printf("float: %lu\n", sizeof(float));
    printf("unsigned int: %lu\n", sizeof(unsigned int));
    printf("unsigned: %lu\n", sizeof(unsigned));
    printf("unsigned long: %lu\n", sizeof(unsigned long));
    printf("long long: %lu\n", sizeof(long long));
    printf("long long int: %lu\n", sizeof(long long int));
    printf("short: %lu\n", sizeof(short));
    return 0;
}