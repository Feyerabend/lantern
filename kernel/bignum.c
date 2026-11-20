/* bignum.c — integers of any size (M11).
 *
 * An integer is a fixnum when it fits in 61 bits, else a bignum: a raw heap
 * object (HT_BIGNUM, sized in bytes like a string, never scanned by the
 * collector) holding 32-bit cells: cell 0 is the sign (0 or 1), then the
 * magnitude's limbs, least significant first, with no leading zero limb.
 * Results are always normalized: anything that fits is a fixnum again.
 *
 * Every operation copies its operands into static scratch buffers first and
 * allocates only the result, so a collection in between moves nothing it
 * still needs. Division is Knuth's algorithm D (TAOCP 4.3.1); wasm has native
 * 64-bit integers, so 64-by-32 steps need no runtime library.
 */
#include "lisp.h"

#define MAX_LIMBS 8192                      /* 262,144 bits: about 79,000 digits */

#define CELLS(x)  ((uint32_t *)STR_DATA(x))
#define NLIMBS(x) (STR_LEN(x) / 4 - 1)

static uint32_t A[MAX_LIMBS + 2], B[MAX_LIMBS + 2], R[2 * MAX_LIMBS + 4], Q[MAX_LIMBS + 2];
static uint32_t UN[MAX_LIMBS + 3], VN[MAX_LIMBS + 2];

static int too_big(void) { lisp_error("Integer too large (over 262,144 bits)", UNBOUND); return -1; }

/* the magnitude of integer X into D; => number of limbs (0 for zero), sign in *NEG */
static int load(obj x, uint32_t *d, int *neg)
{
    if (FIXNUMP(x)) {
        fixnum_t v = FIX(x);
        *neg = v < 0;
        uint64_t m = v < 0 ? -(uint64_t)v : (uint64_t)v;
        d[0] = (uint32_t)m;
        d[1] = (uint32_t)(m >> 32);
        return d[1] ? 2 : d[0] ? 1 : 0;
    }
    int n = NLIMBS(x);
    *neg = CELLS(x)[0] != 0;
    memcpy(d, CELLS(x) + 1, (__SIZE_TYPE__)n * 4);
    return n;
}

static int trim(const uint32_t *d, int n)
{
    while (n > 0 && d[n - 1] == 0) n--;
    return n;
}

/* the integer NEG * D[0..N) */
static obj make(int neg, const uint32_t *d, int n)
{
    n = trim(d, n);
    if (n == 0) return MAKE_FIX(0);
    if (n <= 2) {
        uint64_t m = d[0] | (n == 2 ? (uint64_t)d[1] << 32 : 0);
        if (!neg && m <= (uint64_t)FIXNUM_MAX) return MAKE_FIX((fixnum_t)m);
        if (neg && m <= (uint64_t)FIXNUM_MAX + 1) return MAKE_FIX(-(fixnum_t)(m - 1) - 1);
    }
    if (n > MAX_LIMBS) { too_big(); return NIL; }
    obj x = alloc_object(HT_BIGNUM, (uint64_t)(n + 1) * 4, TAG_OBJECT);
    CELLS(x)[0] = neg ? 1 : 0;
    memcpy(CELLS(x) + 1, d, (__SIZE_TYPE__)n * 4);
    return x;
}

static int cmp_mag(const uint32_t *a, int na, const uint32_t *b, int nb)
{
    if (na != nb) return na < nb ? -1 : 1;
    for (int i = na - 1; i >= 0; i--)
        if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
    return 0;
}

static int add_mag(const uint32_t *a, int na, const uint32_t *b, int nb, uint32_t *r)
{
    if (na < nb) { const uint32_t *t = a; a = b; b = t; int tn = na; na = nb; nb = tn; }
    uint64_t carry = 0;
    for (int i = 0; i < na; i++) {
        uint64_t s = (uint64_t)a[i] + (i < nb ? b[i] : 0) + carry;
        r[i] = (uint32_t)s;
        carry = s >> 32;
    }
    r[na] = (uint32_t)carry;
    return na + 1;
}

/* a - b, with a >= b */
static int sub_mag(const uint32_t *a, int na, const uint32_t *b, int nb, uint32_t *r)
{
    int64_t borrow = 0;
    for (int i = 0; i < na; i++) {
        int64_t s = (int64_t)a[i] - (i < nb ? b[i] : 0) - borrow;
        borrow = s < 0;
        r[i] = (uint32_t)(s + (borrow ? ((int64_t)1 << 32) : 0));
    }
    return na;
}

static int mul_mag(const uint32_t *a, int na, const uint32_t *b, int nb, uint32_t *r)
{
    for (int i = 0; i < na + nb; i++) r[i] = 0;
    for (int i = 0; i < na; i++) {
        uint64_t carry = 0;
        for (int j = 0; j < nb; j++) {
            uint64_t t = (uint64_t)a[i] * b[j] + r[i + j] + carry;
            r[i + j] = (uint32_t)t;
            carry = t >> 32;
        }
        r[i + nb] = (uint32_t)carry;
    }
    return na + nb;
}

/* u / v: quotient in q (nu - nv + 1 limbs), remainder in r (nv limbs); nv >= 1, nu >= nv */
static void divmod_mag(const uint32_t *u, int nu, const uint32_t *v, int nv, uint32_t *q, uint32_t *r)
{
    if (nv == 1) {
        uint64_t rem = 0;
        for (int i = nu - 1; i >= 0; i--) {
            uint64_t cur = (rem << 32) | u[i];
            q[i] = (uint32_t)(cur / v[0]);
            rem = cur % v[0];
        }
        r[0] = (uint32_t)rem;
        return;
    }
    int s = __builtin_clz(v[nv - 1]);                 /* normalize: top bit of v set */
    for (int i = nv - 1; i > 0; i--) VN[i] = (v[i] << s) | (s ? v[i - 1] >> (32 - s) : 0);
    VN[0] = v[0] << s;
    UN[nu] = s ? u[nu - 1] >> (32 - s) : 0;
    for (int i = nu - 1; i > 0; i--) UN[i] = (u[i] << s) | (s ? u[i - 1] >> (32 - s) : 0);
    UN[0] = u[0] << s;

    const uint64_t base = (uint64_t)1 << 32;
    for (int j = nu - nv; j >= 0; j--) {
        uint64_t num = ((uint64_t)UN[j + nv] << 32) | UN[j + nv - 1];
        uint64_t qhat = num / VN[nv - 1], rhat = num % VN[nv - 1];
        while (qhat >= base || qhat * VN[nv - 2] > ((rhat << 32) | UN[j + nv - 2])) {
            qhat--;
            rhat += VN[nv - 1];
            if (rhat >= base) break;
        }
        int64_t k = 0, t;
        for (int i = 0; i < nv; i++) {                 /* multiply and subtract */
            uint64_t p = qhat * VN[i];
            t = (int64_t)UN[i + j] - k - (int64_t)(p & 0xffffffffu);
            UN[i + j] = (uint32_t)t;
            k = (int64_t)(p >> 32) - (t >> 32);
        }
        t = (int64_t)UN[j + nv] - k;
        UN[j + nv] = (uint32_t)t;
        q[j] = (uint32_t)qhat;
        if (t < 0) {                                   /* subtracted too much: add back */
            q[j]--;
            uint64_t c = 0;
            for (int i = 0; i < nv; i++) {
                uint64_t sum = (uint64_t)UN[i + j] + VN[i] + c;
                UN[i + j] = (uint32_t)sum;
                c = sum >> 32;
            }
            UN[j + nv] += (uint32_t)c;
        }
    }
    for (int i = 0; i < nv; i++)                       /* unnormalize the remainder */
        r[i] = (UN[i] >> s) | (s ? UN[i + 1] << (32 - s) : 0);
}

/* ---- the operations, on fixnums and bignums alike ----------------------------- */

static int need(obj x)
{
    if (FIXNUMP(x) || BIGNUMP(x)) return 1;
    lisp_error(FLOATP(x) ? "Not an integer (a float):" : "Not a number:", x);
    return 0;
}

static obj add_signed(obj x, obj y, int negate_y)
{
    if (!need(x) || !need(y)) return NIL;
    int sa, sb;
    int na = load(x, A, &sa), nb = load(y, B, &sb);
    if (na > MAX_LIMBS || nb > MAX_LIMBS) { too_big(); return NIL; }
    if (negate_y) sb = !sb;
    if (sa == sb) return make(sa, R, add_mag(A, na, B, nb, R));
    int c = cmp_mag(A, na, B, nb);
    if (c == 0) return MAKE_FIX(0);
    if (c > 0) return make(sa, R, sub_mag(A, na, B, nb, R));
    return make(sb, R, sub_mag(B, nb, A, na, R));
}

#define FLOAT_CASE(op) if (FLOATP(x) || FLOATP(y)) return flo_arith(op, x, y)

/* ---- ratios (M18) ------------------------------------------------------------
 * A ratio is a heap object holding two integers, the numerator and a
 * denominator above 1, with no common factor. The functions below work on
 * integers and ratios alike; a float anywhere makes the result a float. */

static obj rat_add(obj x, obj y, int negate_y);
static obj rat_mul(obj x, obj y, int divide);
static int rat_cmp(obj x, obj y);
#define RATIO_CASE(expr) if (RATIOP(x) || RATIOP(y)) return expr

obj num_add(obj x, obj y) { FLOAT_CASE(FOP_ADD); RATIO_CASE(rat_add(x, y, 0)); return add_signed(x, y, 0); }
obj num_sub(obj x, obj y) { FLOAT_CASE(FOP_SUB); RATIO_CASE(rat_add(x, y, 1)); return add_signed(x, y, 1); }

obj num_mul(obj x, obj y)
{
    FLOAT_CASE(FOP_MUL);
    RATIO_CASE(rat_mul(x, y, 0));
    if (!need(x) || !need(y)) return NIL;
    int sa, sb;
    int na = load(x, A, &sa), nb = load(y, B, &sb);
    if (na + nb > MAX_LIMBS + 1) { too_big(); return NIL; }
    if (na == 0 || nb == 0) return MAKE_FIX(0);
    return make(sa != sb, R, mul_mag(A, na, B, nb, R));
}

/* truncating division: WANT_REM 0 gives the quotient, 1 the remainder */
static obj divide(obj x, obj y, int want_rem)
{
    if (!need(x) || !need(y)) return NIL;
    int sa, sb;
    int na = load(x, A, &sa), nb = load(y, B, &sb);
    if (nb == 0) { lisp_error("Division by zero", UNBOUND); return NIL; }
    if (cmp_mag(A, na, B, nb) < 0) return want_rem ? x : MAKE_FIX(0);
    divmod_mag(A, na, B, nb, Q, R);
    return want_rem ? make(sa, R, nb) : make(sa != sb, Q, na - nb + 1);
}

/* with a float, / is true division, and the remainder is x - trunc(x/y)*y */
obj num_quotient(obj x, obj y) { FLOAT_CASE(FOP_DIV); RATIO_CASE(num_rational_truncate(rat_mul(x, y, 1))); return divide(x, y, 0); }

obj num_rem(obj x, obj y)
{
    if (RATIOP(x) || RATIOP(y)) {                  /* x - y * truncate(x/y) */
        obj q = NIL, xx = x, yy = y;
        PROTECT3(q, xx, yy);
        q = num_rational_truncate(rat_mul(xx, yy, 1));
        if (!lisp_err) q = num_mul(q, yy);
        obj r = lisp_err ? NIL : num_sub(xx, q);
        UNPROTECT(3);
        return r;
    }
    if (FLOATP(x) || FLOATP(y)) {
        obj q = flo_arith(FOP_DIV, x, y);
        if (lisp_err) return NIL;
        double dq = float_value(q);
        double t = dq < 0 ? -__builtin_floor(-dq) : __builtin_floor(dq);
        double r = num_to_double(x) - t * num_to_double(y);
        return lisp_err ? NIL : make_float(r);
    }
    return divide(x, y, 1);
}

/* -1, 0 or 1; NEED both integers */
int num_cmp(obj x, obj y)
{
    if (FIXNUMP(x) && FIXNUMP(y)) return FIX(x) < FIX(y) ? -1 : FIX(x) > FIX(y);
    if (FLOATP(x) || FLOATP(y)) return flo_cmp(x, y);
    if (RATIOP(x) || RATIOP(y)) return rat_cmp(x, y);
    if (!need(x) || !need(y)) return 0;
    int sa, sb;
    int na = load(x, A, &sa), nb = load(y, B, &sb);
    if (na == 0) sa = 0;
    if (nb == 0) sb = 0;
    if (sa != sb) return sa ? -1 : 1;
    int c = cmp_mag(A, na, B, nb);
    return sa ? -c : c;
}

int num_minusp(obj x)
{
    if (FLOATP(x)) return float_value(x) < 0;
    if (RATIOP(x)) return num_minusp(FIELD(x, RAT_NUM));
    return FIXNUMP(x) ? FIX(x) < 0 : CELLS(x)[0] != 0;
}

/* EQL for bignums: the same value */
int big_eql(obj x, obj y)
{
    if (FLOATP(x)) return flo_eql(x, y);
    if (RATIOP(x)) return RATIOP(y) && num_cmp(FIELD(x, RAT_NUM), FIELD(y, RAT_NUM)) == 0
                          && num_cmp(FIELD(x, RAT_DEN), FIELD(y, RAT_DEN)) == 0;
    if (!BIGNUMP(x) || !BIGNUMP(y) || STR_LEN(x) != STR_LEN(y)) return 0;
    for (int i = 0; i <= NLIMBS(x); i++) if (CELLS(x)[i] != CELLS(y)[i]) return 0;
    return 1;
}

uint64_t big_hash(obj x)
{
    uint64_t h = 1469598103934665603u;
    for (int i = 0; i <= NLIMBS(x); i++) { h ^= CELLS(x)[i]; h *= 1099511628211u; }
    return h;
}

/* ---- decimal ----------------------------------------------------------------------- */

/* the digits of bignum X into BUF (at most CAP); => their number, sign first */
int big_to_decimal(obj x, char *buf, int cap)
{
    int neg;
    int n = load(x, A, &neg);
    int len = 0;
    /* collect 9-digit chunks from the end */
    static char rev[MAX_LIMBS * 10 + 16];
    int nrev = 0;
    while (n > 0) {
        uint64_t rem = 0;
        for (int i = n - 1; i >= 0; i--) {
            uint64_t cur = (rem << 32) | A[i];
            A[i] = (uint32_t)(cur / 1000000000u);
            rem = cur % 1000000000u;
        }
        n = trim(A, n);
        for (int k = 0; k < 9 && (n > 0 || rem > 0 || k == 0); k++) {
            rev[nrev++] = (char)('0' + rem % 10);
            rem /= 10;
            if (n == 0 && rem == 0) break;
        }
    }
    if (neg && len < cap) buf[len++] = '-';
    while (nrev > 0 && len < cap) buf[len++] = rev[--nrev];
    return len;
}

/* the integer written as N decimal digits, negated if NEG (the reader) */
obj big_from_decimal(const char *digits, int n, int neg)
{
    int na = 0;
    for (int k = 0; k < n; k++) {
        uint64_t carry = (uint64_t)(digits[k] - '0');
        for (int i = 0; i < na; i++) {
            uint64_t t = (uint64_t)A[i] * 10 + carry;
            A[i] = (uint32_t)t;
            carry = t >> 32;
        }
        if (carry) {
            if (na >= MAX_LIMBS) { too_big(); return NIL; }
            A[na++] = (uint32_t)carry;
        }
    }
    return make(neg, A, na);
}

/* ---- to and from doubles ---------------------------------------------------------- */

static double pow2(int e)            /* 2^E for -1022 <= E <= 1023 */
{
    union { uint64_t u; double d; } v;
    v.u = (uint64_t)(e + 1023) << 52;
    return v.d;
}

static double scale2(double m, int e)   /* M * 2^E, exactly while no overflow */
{
    while (e > 1000) { m *= pow2(1000); e -= 1000; }
    while (e < -1000) { m *= pow2(-1000); e += 1000; }
    return m * pow2(e);
}

/* bignum X as the nearest double (ties to even); *OVERFLOW if out of range */
double big_to_double(obj x, int *overflow)
{
    int neg;
    int n = load(x, A, &neg);
    *overflow = 0;
    if (n == 0) return 0.0;
    int top = 31;
    while (!(A[n - 1] >> top)) top--;
    int bits = (n - 1) * 32 + top + 1;          /* the magnitude's length in bits */
    /* the 64 bits below the top one, and whether anything under them is set */
    uint64_t t = 0;
    int sticky = 0;
    for (int b = bits - 1; b >= 0; b--) {
        int bit = (A[b / 32] >> (b % 32)) & 1;
        if (b >= bits - 64) t = (t << 1) | (uint64_t)bit;
        else if (bit) { sticky = 1; break; }
    }
    int shift = bits < 64 ? 0 : bits - 64;      /* value = t * 2^shift (+ sticky) */
    int lead = 63;
    while (!(t >> lead)) lead--;
    int drop = lead - 52;                        /* keep 53 bits */
    uint64_t keep = t, rest = 0, half = 0;
    if (drop > 0) {
        keep = t >> drop;
        rest = t & ((UINT64_C(1) << drop) - 1);
        half = UINT64_C(1) << (drop - 1);
        if (rest > half || (rest == half && (sticky || (keep & 1)))) keep++;
        if (keep >> 53) { keep >>= 1; drop++; }
    } else drop = 0;
    int e = shift + drop;
    if (e + 52 > 1023) { *overflow = 1; return 0.0; }
    double d = scale2((double)keep, e);
    return neg ? -d : d;
}

/* the integer value of integral, finite D */
obj big_from_double(double d)
{
    int neg = d < 0;
    if (neg) d = -d;
    union { double d; uint64_t u; } v = { d };
    int e = (int)((v.u >> 52) & 0x7ff) - 1075;   /* d = m * 2^e */
    uint64_t m = (v.u & ((UINT64_C(1) << 52) - 1)) | (UINT64_C(1) << 52);
    if (e < 0) {
        if (e <= -64) return MAKE_FIX(0);
        m >>= -e;
        A[0] = (uint32_t)m;
        A[1] = (uint32_t)(m >> 32);
        return make(neg, A, 2);
    }
    int w = e / 32, s = e % 32;
    for (int i = 0; i < w; i++) A[i] = 0;
    uint64_t lo = m << s, hi = s ? m >> (64 - s) : 0;
    A[w] = (uint32_t)lo;
    A[w + 1] = (uint32_t)(lo >> 32);
    A[w + 2] = (uint32_t)hi;
    return make(neg, A, w + 3);
}

/* ---- bit operations (M14) -------------------------------------------------------------
 * On integers as infinite two's complement, like Common Lisp's LOGAND and
 * friends: both operands are written out in two's complement one limb wider
 * than the longer magnitude, combined limb by limb, and read back. */

static void to_twos(uint32_t *d, int n, int len, int neg)   /* magnitude D[0..n) to LEN limbs, in place */
{
    for (int i = n; i < len; i++) d[i] = 0;
    if (!neg) return;
    uint64_t carry = 1;
    for (int i = 0; i < len; i++) {
        uint64_t t = (uint64_t)(uint32_t)~d[i] + carry;
        d[i] = (uint32_t)t;
        carry = t >> 32;
    }
}

static obj from_twos(uint32_t *d, int len)
{
    int neg = (d[len - 1] >> 31) & 1;
    if (neg) to_twos(d, len, len, 1);                        /* negating again gives the magnitude */
    return make(neg, d, len);
}

/* OP: 0 and, 1 or, 2 xor */
obj num_logop(int op, obj x, obj y)
{
    if (!need(x) || !need(y)) return NIL;
    int sa, sb;
    int na = load(x, A, &sa), nb = load(y, B, &sb);
    int len = (na > nb ? na : nb) + 1;
    if (len > MAX_LIMBS) { too_big(); return NIL; }
    to_twos(A, na, len, sa);
    to_twos(B, nb, len, sb);
    for (int i = 0; i < len; i++)
        R[i] = op == 0 ? (A[i] & B[i]) : op == 1 ? (A[i] | B[i]) : (A[i] ^ B[i]);
    return from_twos(R, len);
}

/* X * 2^COUNT, or for negative COUNT the floor of X / 2^-COUNT */
obj num_ash(obj x, fixnum_t count)
{
    if (!need(x)) return NIL;
    int neg;
    int n = load(x, A, &neg);
    if (n == 0) return MAKE_FIX(0);
    if (count >= 0) {
        fixnum_t words = count / 32;
        int bits = (int)(count % 32);
        if (n + words + 1 > MAX_LIMBS) { too_big(); return NIL; }
        for (int i = 0; i < n + words + 1; i++) R[i] = 0;
        for (int i = 0; i < n; i++) {
            uint64_t v = (uint64_t)A[i] << bits;
            R[i + words] |= (uint32_t)v;
            R[i + words + 1] |= (uint32_t)(v >> 32);
        }
        return make(neg, R, (int)(n + words + 1));
    }
    /* right: floor. For a negative X that is -((|x| - 1) >> s) - 1 */
    fixnum_t s = -count;
    if (neg) {                                           /* |x| - 1 */
        for (int i = 0; i < n; i++) { if (A[i]--) break; }
        n = trim(A, n);
    }
    fixnum_t words = s / 32;
    int bits = (int)(s % 32);
    int m = words >= n ? 0 : (int)(n - words);
    for (int i = 0; i < m; i++) {
        uint64_t lo = A[i + words], hi = i + words + 1 < n ? A[i + words + 1] : 0;
        R[i] = (uint32_t)(((hi << 32) | lo) >> bits);
    }
    m = trim(R, m);
    if (!neg) return make(0, R, m);
    /* -(r + 1) */
    R[m] = 0;
    uint64_t carry = 1;
    for (int i = 0; i <= m && carry; i++) { uint64_t t = (uint64_t)R[i] + carry; R[i] = (uint32_t)t; carry = t >> 32; }
    return make(1, R, m + 1);
}

/* the number of bits in X's two's complement, sign bit left out; and the
 * number of 1 bits of a non-negative X (of LOGNOT X for a negative one) */
static int bitcount(uint32_t v) { int c = 0; while (v) { v &= v - 1; c++; } return c; }

obj num_integer_length(obj x, int count_ones)
{
    if (!need(x)) return NIL;
    int neg;
    int n = load(x, A, &neg);
    if (neg) {                                           /* lognot x = |x| - 1 */
        for (int i = 0; i < n; i++) { if (A[i]--) break; }
        n = trim(A, n);
    }
    if (count_ones) {
        fixnum_t c = 0;
        for (int i = 0; i < n; i++) c += bitcount(A[i]);
        return MAKE_FIX(c);
    }
    if (n == 0) return MAKE_FIX(0);
    int top = 31;
    while (!(A[n - 1] >> top)) top--;
    return MAKE_FIX((fixnum_t)(n - 1) * 32 + top + 1);
}

/* ---- ratios (M18) ---------------------------------------------------------------- */

static int int_zerop(obj x) { return FIXNUMP(x) && FIX(x) == 0; }

static obj int_abs(obj x) { return num_minusp(x) ? num_sub(MAKE_FIX(0), x) : x; }

/* the greatest common divisor of two integers, >= 0 */
static obj int_gcd(obj a, obj b)
{
    PROTECT2(a, b);
    a = int_abs(a);
    b = int_abs(b);
    while (!lisp_err && !int_zerop(b)) {
        obj r = divide(a, b, 1);
        a = b;
        b = r;
    }
    UNPROTECT(2);
    return a;
}

obj make_ratio(obj n, obj d)
{
    if (!INTEGERP(n) || !INTEGERP(d)) return lisp_error("Not an integer:", INTEGERP(n) ? d : n);
    if (int_zerop(d)) return lisp_error("Division by zero", UNBOUND);
    obj g = NIL;
    PROTECT3(n, d, g);
    if (num_minusp(d)) { n = num_sub(MAKE_FIX(0), n); d = num_sub(MAKE_FIX(0), d); }
    g = int_gcd(n, d);
    if (!lisp_err && !(FIXNUMP(g) && FIX(g) == 1)) { n = divide(n, g, 0); d = divide(d, g, 0); }
    obj r = n;
    if (!lisp_err && !(FIXNUMP(d) && FIX(d) == 1)) {
        r = alloc_object(HT_RATIO, RAT_NFIELDS, TAG_OBJECT);
        FIELD(r, RAT_NUM) = n;
        FIELD(r, RAT_DEN) = d;
    }
    UNPROTECT(3);
    return lisp_err ? NIL : r;
}

obj num_numerator(obj x) { return RATIOP(x) ? FIELD(x, RAT_NUM) : x; }
obj num_denominator(obj x) { return RATIOP(x) ? FIELD(x, RAT_DEN) : MAKE_FIX(1); }

obj num_rational_truncate(obj x)
{
    if (lisp_err) return NIL;
    return RATIOP(x) ? divide(FIELD(x, RAT_NUM), FIELD(x, RAT_DEN), 0) : x;
}

/* a/b + c/d = (ad + cb) / bd, and minus */
static obj rat_add(obj x, obj y, int negate_y)
{
    obj t1 = NIL, t2 = NIL;
    PROTECT4(x, y, t1, t2);
    t1 = num_mul(num_numerator(x), num_denominator(y));
    if (!lisp_err) t2 = num_mul(num_numerator(y), num_denominator(x));
    if (!lisp_err) t1 = negate_y ? num_sub(t1, t2) : num_add(t1, t2);
    if (!lisp_err) t2 = num_mul(num_denominator(x), num_denominator(y));
    obj r = lisp_err ? NIL : make_ratio(t1, t2);
    UNPROTECT(4);
    return r;
}

/* a/b * c/d = ac / bd; divided: ad / bc */
static obj rat_mul(obj x, obj y, int divide_p)
{
    obj t1 = NIL, t2 = NIL;
    PROTECT4(x, y, t1, t2);
    if (divide_p && int_zerop(num_numerator(y))) { UNPROTECT(4); return lisp_error("Division by zero", UNBOUND); }
    t1 = num_mul(num_numerator(x), divide_p ? num_denominator(y) : num_numerator(y));
    if (!lisp_err) t2 = num_mul(num_denominator(x), divide_p ? num_numerator(y) : num_denominator(y));
    obj r = lisp_err ? NIL : make_ratio(t1, t2);
    UNPROTECT(4);
    return r;
}

/* a/b against c/d: ad against cb, the denominators being positive */
static int rat_cmp(obj x, obj y)
{
    obj t1 = NIL, t2 = NIL;
    PROTECT4(x, y, t1, t2);
    t1 = num_mul(num_numerator(x), num_denominator(y));
    if (!lisp_err) t2 = num_mul(num_numerator(y), num_denominator(x));
    int c = lisp_err ? 0 : num_cmp(t1, t2);
    UNPROTECT(4);
    return c;
}

/* true division (/): a ratio when integers do not divide */
obj num_div(obj x, obj y)
{
    FLOAT_CASE(FOP_DIV);
    if (int_zerop(y)) return lisp_error("Division by zero", UNBOUND);
    if (FIXNUMP(x) && FIXNUMP(y) && FIX(y) != -1 && FIX(x) % FIX(y) == 0) return MAKE_FIX(FIX(x) / FIX(y));
    return rat_mul(x, y, 1);
}
