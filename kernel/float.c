/* float.c — floating-point numbers (M12).
 *
 * There is one kind of float, the IEEE double: a raw heap object of 8 bytes
 * (HT_FLOAT), copied by the collector without being looked at. Any
 * arithmetic with a float argument comes here from bignum.c, so a float
 * makes the whole operation a float one (contagion). Results that are not
 * finite are errors: there are no infinities or NaNs at the Lisp level.
 *
 * What the kernel cannot do without a libm comes from the host: in the
 * browser, JavaScript's Math functions, and the conversions between
 * doubles and decimal text (which are correctly rounded there, and give the
 * shortest digits that read back the same). Natively, the C library does
 * the same jobs.
 */
#include "lisp.h"

#ifdef __wasm__
#define KIMPORT(name) __attribute__((import_module("env"), import_name(name)))
KIMPORT("host_math") double host_math(int op, double x, double y);
KIMPORT("host_float_digits") int host_float_digits(double x, char *buf, int cap);
KIMPORT("host_float_parse") double host_float_parse(const char *s, int len);
KIMPORT("host_wall_clock") double host_wall_clock(void);            /* ms since 1970 (M17) */
KIMPORT("host_utc_offset") double host_utc_offset(double ms);       /* minutes east of UTC then */
#else
#include <time.h>
static double host_wall_clock(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (double)ts.tv_sec * 1000.0 + (double)(ts.tv_nsec / 1000000);
}
static double host_utc_offset(double ms)
{
    time_t t = (time_t)(ms / 1000.0);
    struct tm tm;
    localtime_r(&t, &tm);
    return (double)(tm.tm_gmtoff / 60);
}
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* op numbers shared with host.js */
enum { M_EXP, M_LOG, M_SIN, M_COS, M_TAN, M_ASIN, M_ACOS, M_ATAN2, M_POW, M_SINH, M_COSH, M_TANH };

static double host_math(int op, double x, double y)
{
    switch (op) {
    case M_EXP: return exp(x);
    case M_LOG: return log(x);
    case M_SIN: return sin(x);
    case M_COS: return cos(x);
    case M_TAN: return tan(x);
    case M_ASIN: return asin(x);
    case M_ACOS: return acos(x);
    case M_ATAN2: return atan2(x, y);
    case M_POW: return pow(x, y);
    case M_SINH: return sinh(x);
    case M_COSH: return cosh(x);
    case M_TANH: return tanh(x);
    }
    return 0;
}

/* the shortest "D.DDDDe±X" that reads back as X (X > 0) */
static int host_float_digits(double x, char *buf, int cap)
{
    for (int p = 0; p < 17; p++) {
        int n = snprintf(buf, (size_t)cap, "%.*e", p, x);
        if (strtod(buf, 0) == x) return n;
    }
    return snprintf(buf, (size_t)cap, "%.16e", x);
}

static double host_float_parse(const char *s, int len)
{
    char tmp[600];
    if (len > (int)sizeof tmp - 1) len = (int)sizeof tmp - 1;
    memcpy(tmp, s, (size_t)len);
    tmp[len] = 0;
    return strtod(tmp, 0);
}
#endif

#define FINITE(d) ((d) - (d) == 0)

typedef union { double d; uint64_t u; } bits_t;

obj make_float(double d)
{
    obj x = alloc_object(HT_FLOAT, 8, TAG_OBJECT);
    memcpy(STR_DATA(x), &d, 8);
    return x;
}

double float_value(obj x)
{
    double d;
    memcpy(&d, STR_DATA(x), 8);
    return d;
}

/* a float result, or an error if it is not finite */
static obj result(double d)
{
    if (!FINITE(d)) return lisp_error("Floating-point overflow (the result is not a finite number)", UNBOUND);
    return make_float(d);
}

double num_to_double(obj x)
{
    if (FIXNUMP(x)) return (double)FIX(x);
    if (FLOATP(x)) return float_value(x);
    if (BIGNUMP(x)) {
        int overflow;
        double d = big_to_double(x, &overflow);
        if (overflow) lisp_error("Integer too large to be a float:", x);
        return d;
    }
    if (RATIOP(x)) return num_to_double(FIELD(x, RAT_NUM)) / num_to_double(FIELD(x, RAT_DEN));   /* M18 */
    lisp_error("Not a number:", x);
    return 0;
}

obj flo_arith(int op, obj x, obj y)
{
    if (!NUMBERP(x)) return lisp_error("Not a number:", x);
    if (!NUMBERP(y)) return lisp_error("Not a number:", y);
    double a = num_to_double(x), b = num_to_double(y);
    if (lisp_err) return NIL;
    switch (op) {
    case FOP_ADD: return result(a + b);
    case FOP_SUB: return result(a - b);
    case FOP_MUL: return result(a * b);
    default:
        if (b == 0) return lisp_error("Division by zero", UNBOUND);
        return result(a / b);
    }
}

/* exact comparison, also between a float and an integer of any size */
int flo_cmp(obj x, obj y)
{
    if (!NUMBERP(x)) { lisp_error("Not a number:", x); return 0; }
    if (!NUMBERP(y)) { lisp_error("Not a number:", y); return 0; }
    if (FLOATP(x) && FLOATP(y)) {
        double a = float_value(x), b = float_value(y);
        return a < b ? -1 : a > b;
    }
    int flip = !FLOATP(x);          /* make X the float */
    if (flip) { obj t = x; x = y; y = t; }
    double a = float_value(x);
    int c;
    if (RATIOP(y)) {                    /* exactly: the float as the rational it is (M18) */
        double b = num_to_double(y);
        if (a != b) c = a < b ? -1 : 1;
        else {
            double m = a;
            int k = 0;
            while (m != __builtin_floor(m) && k < 1100) { m *= 2; k++; }
            obj num = NIL, r = NIL;
            PROTECT3(y, num, r);
            num = (m > -1e18 && m < 1e18) ? MAKE_FIX((fixnum_t)m) : big_from_double(m);
            if (!lisp_err) r = num_ash(MAKE_FIX(1), k);
            if (!lisp_err) r = make_ratio(num, r);
            c = lisp_err ? 0 : num_cmp(r, y);
            UNPROTECT(3);
        }
        return flip ? -c : c;
    }
    if (BIGNUMP(y)) {
        int overflow;
        double b = big_to_double(y, &overflow);
        if (overflow) c = num_minusp(y) ? 1 : -1;
        else if (a != b) c = a < b ? -1 : 1;   /* rounding keeps order, so this is exact */
        else c = num_cmp(big_from_double(a), y);   /* both integral and huge */
    } else {
        fixnum_t i = FIX(y);
        double b = (double)i;
        if (a != b) c = a < b ? -1 : 1;
        else if (i > (INT64_C(1) << 53) || i < -(INT64_C(1) << 53)) {
            fixnum_t ai = (fixnum_t)a;   /* integral and within fixnum range */
            c = ai < i ? -1 : ai > i;
        } else c = 0;
    }
    return flip ? -c : c;
}

int flo_eql(obj x, obj y)
{
    if (!FLOATP(x) || !FLOATP(y)) return 0;
    bits_t a = { float_value(x) }, b = { float_value(y) };
    return a.u == b.u;
}

uint64_t flo_hash(obj x)
{
    bits_t b = { float_value(x) };
    return (b.u ^ (b.u >> 29)) * 0x9E3779B97F4A7C15u;
}

/* the integer toward zero from X */
obj flo_truncate(obj x)
{
    if (INTEGERP(x)) return x;
    if (RATIOP(x)) return num_rational_truncate(x);
    if (!FLOATP(x)) return lisp_error("Not a number:", x);
    double d = float_value(x);
    double t = d < 0 ? -__builtin_floor(-d) : __builtin_floor(d);
    if (t > -1e18 && t < 1e18) return MAKE_FIX((fixnum_t)t);
    return big_from_double(t);
}

/* ---- printing -------------------------------------------------------------
 * Shortest digits that read back the same, laid out as Lisp prints them:
 * 1.5, 100.0, 0.001, 1.0e7, 1.2345e-5. */

int flo_to_string(double d, char *buf, int cap)
{
    int n = 0;
#define OUT(c) do { if (n < cap) buf[n++] = (c); } while (0)
    bits_t b = { d };
    if (b.u >> 63) { OUT('-'); d = -d; }
    if (d == 0) { OUT('0'); OUT('.'); OUT('0'); return n; }
    char raw[64];
    int len = host_float_digits(d, raw, (int)sizeof raw - 1);
    /* raw is D[.DDD]e±X */
    char digits[32];
    int nd = 0, i = 0, exp = 0, eneg = 0;
    for (; i < len && raw[i] != 'e'; i++)
        if (raw[i] >= '0' && raw[i] <= '9' && nd < 30) digits[nd++] = raw[i];
    if (i < len) i++;
    if (i < len && (raw[i] == '+' || raw[i] == '-')) eneg = raw[i++] == '-';
    for (; i < len; i++) exp = exp * 10 + (raw[i] - '0');
    if (eneg) exp = -exp;
    while (nd > 1 && digits[nd - 1] == '0') nd--;
    if (exp >= -3 && exp < 7) {
        if (exp < 0) {
            OUT('0'); OUT('.');
            for (int k = 0; k < -exp - 1; k++) OUT('0');
            for (int k = 0; k < nd; k++) OUT(digits[k]);
        } else {
            for (int k = 0; k <= exp; k++) OUT(k < nd ? digits[k] : '0');
            OUT('.');
            if (nd > exp + 1) for (int k = exp + 1; k < nd; k++) OUT(digits[k]);
            else OUT('0');
        }
    } else {
        OUT(digits[0]); OUT('.');
        if (nd > 1) for (int k = 1; k < nd; k++) OUT(digits[k]);
        else OUT('0');
        OUT('e');
        if (exp < 0) { OUT('-'); exp = -exp; }
        char e[8];
        int ne = 0;
        do { e[ne++] = (char)('0' + exp % 10); exp /= 10; } while (exp);
        while (ne) OUT(e[--ne]);
    }
    return n;
#undef OUT
}

/* ---- reading --------------------------------------------------------------
 * [sign] digits [. digits] [marker [sign] digits], with a digit somewhere in
 * the mantissa, and either a fraction or an exponent. The exponent markers
 * are E S F D L, as in Common Lisp; all of them read a double.
 * => 1 and the value in *OUT, 0 if TOK is not a float, -1 if out of range. */

static int is_marker(char c)
{
    return c == 'e' || c == 'E' || c == 's' || c == 'S' || c == 'f' || c == 'F'
        || c == 'd' || c == 'D' || c == 'l' || c == 'L';
}

int flo_parse(const char *tok, int n, double *out)
{
    char t[600];
    int i = 0, m = 0, mant = 0, frac = 0, expdigits = 0, dot = 0, marker = 0;
    if (n > (int)sizeof t - 1) return 0;
    if (i < n && (tok[i] == '+' || tok[i] == '-')) t[m++] = tok[i++];
    for (; i < n && tok[i] >= '0' && tok[i] <= '9'; i++) { t[m++] = tok[i]; mant++; }
    if (i < n && tok[i] == '.') {
        dot = 1;
        t[m++] = tok[i++];
        for (; i < n && tok[i] >= '0' && tok[i] <= '9'; i++) { t[m++] = tok[i]; mant++; frac++; }
    }
    if (mant == 0) return 0;
    if (i < n && is_marker(tok[i])) {
        marker = 1;
        t[m++] = 'e';
        i++;
        if (i < n && (tok[i] == '+' || tok[i] == '-')) t[m++] = tok[i++];
        for (; i < n && tok[i] >= '0' && tok[i] <= '9'; i++) { t[m++] = tok[i]; expdigits++; }
        if (expdigits == 0) return 0;
    }
    if (i != n) return 0;
    if (!marker && !(dot && frac > 0)) return 0;   /* 12 and 12. are integers */
    double d = host_float_parse(t, m);
    if (!FINITE(d)) return -1;
    *out = d;
    return 1;
}

/* ---- primitives ------------------------------------------------------------ */

#define NEED_NUM(x) do { if (!NUMBERP(x)) return lisp_error("Not a number:", x); } while (0)

obj fp_floatp(obj *a, int n)  { (void)n; return FLOATP(a[0]) ? S_T : NIL; }
obj fp_numberp(obj *a, int n) { (void)n; return NUMBERP(a[0]) ? S_T : NIL; }

obj fp_float(obj *a, int n)
{
    (void)n;
    NEED_NUM(a[0]);
    if (FLOATP(a[0])) return a[0];
    double d = num_to_double(a[0]);
    return lisp_err ? NIL : make_float(d);
}

obj fp_truncate(obj *a, int n) { (void)n; return flo_truncate(a[0]); }

/* (%float-function op x [y]): op is a fixnum, see host_math; and -1 is sqrt */
obj fp_float_function(obj *a, int n)
{
    if (!FIXNUMP(a[0])) return lisp_error("Bad float function:", a[0]);
    NEED_NUM(a[1]);
    double x = num_to_double(a[1]), y = 0;
    if (n > 2) { NEED_NUM(a[2]); y = num_to_double(a[2]); }
    if (lisp_err) return NIL;
    int op = (int)FIX(a[0]);
    if (op == -1) {
        if (x < 0) return lisp_error("SQRT of a negative number (there are no complex numbers):", a[1]);
        return make_float(__builtin_sqrt(x));
    }
    double r = host_math(op, x, y);
    if (r != r) return lisp_error("The result is not a real number for", a[1]);
    return result(r);
}

/* (%decode-float x) => (mantissa-integer exponent sign), x = sign * m * 2^e */
obj fp_integer_decode(obj *a, int n)
{
    (void)n;
    if (!FLOATP(a[0])) return lisp_error("Not a float:", a[0]);
    bits_t b = { float_value(a[0]) };
    int e = (int)((b.u >> 52) & 0x7ff);
    uint64_t m = b.u & ((UINT64_C(1) << 52) - 1);
    if (e) m |= UINT64_C(1) << 52, e -= 1075;
    else e = m ? -1074 : 0;
    obj r = cons(MAKE_FIX((b.u >> 63) ? -1 : 1), NIL);
    r = cons(MAKE_FIX(e), r);
    return cons(MAKE_FIX((fixnum_t)m), r);
}

/* ---- the wall clock (M17) -------------------------------------------------------- */

/* (%wall-clock) => seconds since 1970-01-01 UTC, an integer */
obj fp_wall_clock(obj *a, int n) { (void)a; (void)n; return MAKE_FIX((fixnum_t)(host_wall_clock() / 1000.0)); }

/* (%utc-offset unix-seconds) => the local time zone then, in minutes east of UTC */
obj fp_utc_offset(obj *a, int n)
{
    (void)n;
    if (!FIXNUMP(a[0])) return lisp_error("Not an integer:", a[0]);
    return MAKE_FIX((fixnum_t)host_utc_offset((double)FIX(a[0]) * 1000.0));
}
