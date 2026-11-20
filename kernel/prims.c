/* prims.c — primitive functions written in C.
 *
 * A primitive receives its evaluated arguments as an array that lives on the
 * evaluator stack, so a[i] stays valid (and is updated) across allocation.
 * Any other obj held in a C local across an allocation must be protected.
 */
#include "lisp.h"

#define NO_IRRITANT UNBOUND
#define P(name) static obj name(obj *a, int n)

double lisp_now_ms;
uint64_t gensym_counter;   /* saved in world images */

static obj boolean(int b) { return b ? S_T : NIL; }

#define NEED(cond, msg, x) do { if (!(cond)) return lisp_error(msg, x); } while (0)
#define NEED_FIX(x)    NEED(FIXNUMP(x), "Not an integer:", x)
#define NEED_LIST(x)   NEED(LISTP(x), "Not a list:", x)
#define NEED_CONS(x)   NEED(CONSP(x), "Not a cons:", x)
#define NEED_SYM(x)    NEED(TAG(x) == TAG_SYMBOL, "Not a symbol:", x)
#define NEED_STRING(x) NEED(STRINGP(x), "Not a string:", x)


/* ---- capture sink for prin1-to-string and error messages ----------------- */

#define CAPTURE_MAX 65536
static char capture_buf[CAPTURE_MAX];
static int capture_len;

static void capture_write(sink_t *s, const char *p, int n)
{
    (void)s;
    for (int i = 0; i < n && capture_len < CAPTURE_MAX; i++) capture_buf[capture_len++] = p[i];
}
static sink_t capture_sink = { capture_write };

/* ---- lists --------------------------------------------------------------- */

P(p_car) { (void)n; if (a[0] == NIL) return NIL; NEED_CONS(a[0]); return CAR(a[0]); }
P(p_cdr) { (void)n; if (a[0] == NIL) return NIL; NEED_CONS(a[0]); return CDR(a[0]); }
P(p_cons) { (void)n; return cons(a[0], a[1]); }
P(p_rplaca) { (void)n; NEED_CONS(a[0]); CAR(a[0]) = a[1]; return a[0]; }
P(p_rplacd) { (void)n; NEED_CONS(a[0]); CDR(a[0]) = a[1]; return a[0]; }

P(p_list)
{
    obj r = NIL;
    for (int i = n - 1; i >= 0; i--) r = cons(a[i], r);
    return r;
}

/* copy LIST with TAIL as the final cdr */
static obj copy_onto(obj list, obj tail)
{
    obj head = NIL, last = NIL;
    PROTECT4(list, tail, head, last);
    for (; CONSP(list); list = CDR(list)) {
        obj cell = cons(CAR(list), NIL);
        if (last == NIL) head = cell;
        else CDR(last) = cell;
        last = cell;
    }
    if (list != NIL) lisp_error("Not a proper list:", list);
    if (last == NIL) head = tail;
    else CDR(last) = tail;
    UNPROTECT(4);
    return head;
}

P(p_append)
{
    if (n == 0) return NIL;
    obj r = a[n - 1];
    PROTECT1(r);
    for (int i = n - 2; i >= 0 && !lisp_err; i--) r = copy_onto(a[i], r);
    UNPROTECT(1);
    return r;
}

P(p_length)
{
    (void)n;
    obj x = a[0];
    if (STRINGP(x)) return MAKE_FIX(STR_LEN(x));
    if (VECTORP(x)) return MAKE_FIX(VEC_LEN(x));
    if (ARRAYP(x)) {
        NEED(array_rank(x) == 1, "Not a sequence (an array of rank other than 1):", x);
        return MAKE_FIX(array_length(x));
    }
    fixnum_t len = list_length(x);
    NEED(len >= 0, "Not a proper list:", x);
    return MAKE_FIX(len);
}

P(p_reverse)
{
    (void)n;
    obj list = a[0], r = NIL;
    PROTECT2(list, r);
    for (; CONSP(list); list = CDR(list)) r = cons(CAR(list), r);
    UNPROTECT(2);
    if (list != NIL) return lisp_error("Not a proper list:", a[0]);
    return r;
}

P(p_nreverse) { (void)n; NEED_LIST(a[0]); return nreverse(a[0]); }

/* ---- predicates and equality --------------------------------------------- */

static int equal(obj x, obj y, int depth)
{
    for (;;) {
        if (x == y) return 1;
        if (depth > 10000) return 0;
        if (CONSP(x) && CONSP(y)) {
            if (!equal(CAR(x), CAR(y), depth + 1)) return 0;
            x = CDR(x);
            y = CDR(y);
            continue;
        }
        if (STRINGP(x) && STRINGP(y)) return str_equal(x, y);
        if (BIGNUMP(x) || FLOATP(x) || RATIOP(x)) return big_eql(x, y);
        if (bit_array_p(x) && bit_array_p(y)) {          /* bit vectors compare by their bits (M18) */
            if (array_rank(x) != 1 || array_rank(y) != 1 || array_length(x) != array_length(y)) return 0;
            int ox = (int)FIX(FIELD(x, AR_OFFSET)), oy = (int)FIX(FIELD(y, AR_OFFSET));
            for (int i = 0; i < array_length(x); i++)
                if (STR_DATA(FIELD(x, AR_DATA))[ox + i] != STR_DATA(FIELD(y, AR_DATA))[oy + i]) return 0;
            return 1;
        }
        if (VECTORP(x) && VECTORP(y)) {
            if (VEC_LEN(x) != VEC_LEN(y)) return 0;
            for (int i = 0; i < VEC_LEN(x); i++)
                if (!equal(VEC_REF(x, i), VEC_REF(y, i), depth + 1)) return 0;
            return 1;
        }
        return 0;
    }
}

P(p_eq)         { (void)n; return boolean(a[0] == a[1]); }
P(p_eql)        { (void)n; return boolean(a[0] == a[1] || big_eql(a[0], a[1])); }
P(p_equal)      { (void)n; return boolean(equal(a[0], a[1], 0)); }
P(p_null)       { (void)n; return boolean(a[0] == NIL); }
P(p_atom)       { (void)n; return boolean(!CONSP(a[0])); }
P(p_consp)      { (void)n; return boolean(CONSP(a[0])); }
P(p_listp)      { (void)n; return boolean(LISTP(a[0])); }
P(p_symbolp)    { (void)n; return boolean(SYMBOLP(a[0])); }
P(p_integerp)   { (void)n; return boolean(INTEGERP(a[0])); }
P(p_stringp)    { (void)n; return boolean(STRINGP(a[0])); }
P(p_vectorp)    { (void)n; return boolean(VECTORP(a[0]) || (ARRAYP(a[0]) && array_rank(a[0]) == 1)); }
P(p_characterp) { (void)n; return boolean(CHARP(a[0])); }
P(p_functionp)  { (void)n; return boolean(CLOSUREP(a[0]) || PRIMITIVEP(a[0]) || (CFUNCP(a[0]) && !MACROP(a[0]))); }

/* ---- arithmetic ------------------------------------------------------------
 * Integers of any size: fixnums take the fast path here, anything else
 * (a bignum, or a result that does not fit) goes to bignum.c. The running
 * total of a fold may be a fresh bignum, so it is protected while the next
 * step allocates. */

#define NEED_INT(x) NEED(INTEGERP(x), "Not an integer:", x)
#define NEED_NUM(x) NEED(NUMBERP(x), "Not a number:", x)

static obj add2(obj x, obj y)
{
    if (FIXNUMP(x) && FIXNUMP(y)) {
        fixnum_t s = FIX(x) + FIX(y);           /* 61-bit values: no int64 overflow */
        if (s >= FIXNUM_MIN && s <= FIXNUM_MAX) return MAKE_FIX(s);
    }
    return num_add(x, y);
}

static obj sub2(obj x, obj y)
{
    if (FIXNUMP(x) && FIXNUMP(y)) {
        fixnum_t s = FIX(x) - FIX(y);
        if (s >= FIXNUM_MIN && s <= FIXNUM_MAX) return MAKE_FIX(s);
    }
    return num_sub(x, y);
}

static obj mul2(obj x, obj y)
{
    if (FIXNUMP(x) && FIXNUMP(y)) {
        /* |x * y| <= FIXNUM_MAX fits; tested by division, which wasm has natively
         * (__builtin_mul_overflow would need a 128-bit runtime helper there) */
        fixnum_t a = FIX(x), b = FIX(y);
        uint64_t ua = a < 0 ? -(uint64_t)a : (uint64_t)a, ub = b < 0 ? -(uint64_t)b : (uint64_t)b;
        if (ua == 0 || ub == 0) return MAKE_FIX(0);
        if (ua <= (uint64_t)FIXNUM_MAX / ub) return MAKE_FIX(a * b);
    }
    return num_mul(x, y);
}

typedef obj (*op2_t)(obj, obj);

static obj fold(obj *a, int n, obj start, op2_t f)
{
    obj s = start;
    PROTECT1(s);
    for (int i = 0; i < n && !lisp_err; i++) s = f(s, a[i]);
    UNPROTECT(1);
    return lisp_err ? NIL : s;
}

P(p_add) { for (int i = 0; i < n; i++) NEED_NUM(a[i]); return fold(a, n, MAKE_FIX(0), add2); }
P(p_mul) { for (int i = 0; i < n; i++) NEED_NUM(a[i]); return fold(a, n, MAKE_FIX(1), mul2); }

P(p_sub)
{
    for (int i = 0; i < n; i++) NEED_NUM(a[i]);
    if (n == 1) return sub2(MAKE_FIX(0), a[0]);
    return fold(a + 1, n - 1, a[0], sub2);
}

/* integer division, truncating toward zero */
static obj quo2(obj x, obj y)
{
    if (FIXNUMP(x) && FIXNUMP(y)) {
        if (FIX(y) == 0) return lisp_error("Division by zero", NO_IRRITANT);
        fixnum_t q = FIX(x) / FIX(y);
        if (q <= FIXNUM_MAX) return MAKE_FIX(q);          /* FIXNUM_MIN / -1 does not fit */
    }
    return num_quotient(x, y);
}

/* %QUOTIENT: what / did before ratios (M18), truncating on integers */
P(p_quotient) { for (int i = 0; i < n; i++) NEED_NUM(a[i]); return fold(a + 1, n - 1, a[0], quo2); }

/* / is true division: (/ 7 2) is 7/2; (/ x) is 1/x */
static obj div2(obj x, obj y) { return num_div(x, y); }
P(p_div)
{
    for (int i = 0; i < n; i++) NEED_NUM(a[i]);
    if (n == 1) return num_div(MAKE_FIX(1), a[0]);
    return fold(a + 1, n - 1, a[0], div2);
}

P(p_numerator)   { (void)n; NEED(RATIONALP(a[0]), "Not a rational number:", a[0]); return num_numerator(a[0]); }
P(p_denominator) { (void)n; NEED(RATIONALP(a[0]), "Not a rational number:", a[0]); return num_denominator(a[0]); }
P(p_rationalp)   { (void)n; return boolean(RATIONALP(a[0])); }

P(p_rem)
{
    (void)n;
    NEED_NUM(a[0]);
    NEED_NUM(a[1]);
    if (FIXNUMP(a[0]) && FIXNUMP(a[1])) {
        if (FIX(a[1]) == 0) return lisp_error("Division by zero", NO_IRRITANT);
        return MAKE_FIX(FIX(a[0]) % FIX(a[1]));
    }
    return num_rem(a[0], a[1]);
}

/* the remainder with the divisor's sign (floor division) */
P(p_mod)
{
    (void)n;
    NEED_NUM(a[0]);
    NEED_NUM(a[1]);
    if (FIXNUMP(a[0]) && FIXNUMP(a[1])) {
        fixnum_t d = FIX(a[1]);
        if (d == 0) return lisp_error("Division by zero", NO_IRRITANT);
        fixnum_t r = FIX(a[0]) % d;
        if (r != 0 && (r < 0) != (d < 0)) r += d;
        return MAKE_FIX(r);
    }
    obj r = num_rem(a[0], a[1]);
    if (lisp_err) return NIL;
    if (num_cmp(r, MAKE_FIX(0)) != 0 && num_minusp(r) != num_minusp(a[1])) r = num_add(r, a[1]);
    return r;
}

P(p_1plus)  { (void)n; NEED_NUM(a[0]); return add2(a[0], MAKE_FIX(1)); }
P(p_1minus) { (void)n; NEED_NUM(a[0]); return sub2(a[0], MAKE_FIX(1)); }

enum { CMP_EQ, CMP_LT, CMP_GT, CMP_LE, CMP_GE };

static obj compare(obj *a, int n, int op)
{
    for (int i = 0; i < n; i++) NEED_NUM(a[i]);
    for (int i = 0; i + 1 < n; i++) {
        int c = num_cmp(a[i], a[i + 1]);
        int ok = op == CMP_EQ ? c == 0 : op == CMP_LT ? c < 0 : op == CMP_GT ? c > 0
               : op == CMP_LE ? c <= 0 : c >= 0;
        if (!ok) return NIL;
    }
    return S_T;
}

P(p_num_eq) { return compare(a, n, CMP_EQ); }
P(p_lt)     { return compare(a, n, CMP_LT); }
P(p_gt)     { return compare(a, n, CMP_GT); }
P(p_le)     { return compare(a, n, CMP_LE); }
P(p_ge)     { return compare(a, n, CMP_GE); }

/* ---- symbols ------------------------------------------------------------ */

/* A keyword keeps its colon in its name inside the kernel (:FOO is named
 * ":FOO"); SYMBOL-NAME and STRING give Common Lisp's "FOO" (M17), and
 * %SYMBOL-NAME the name as it is kept. */
static obj lisp_symbol_name(obj sym)
{
    obj name = SYM(sym, SYM_NAME);
    if (SYM(sym, SYM_PACKAGE) == keyword_package && STR_LEN(name) > 0 && str_ref(name, 0) == ':') {
        int len = STR_LEN(name) - 1;
        PROTECT1(name);                                /* the collector may move it */
        obj s = make_string_of(len, WSTRINGP(name));
        UNPROTECT(1);
        for (int i = 0; i < len; i++) str_set(s, i, str_ref(name, i + 1));
        return s;
    }
    return name;
}

P(p_symbol_name)
{
    (void)n;
    if (a[0] == NIL) return make_string("NIL", 3);
    NEED_SYM(a[0]);
    return lisp_symbol_name(a[0]);
}

P(p_raw_symbol_name)
{
    (void)n;
    if (a[0] == NIL) return make_string("NIL", 3);
    NEED_SYM(a[0]);
    return SYM(a[0], SYM_NAME);
}

P(p_make_symbol) { (void)n; NEED_STRING(a[0]); return make_symbol(a[0]); }

P(p_gensym)
{
    (void)a; (void)n;
    char buf[24];
    int len = sizeof buf;
    uint64_t v = ++gensym_counter;
    do { buf[--len] = (char)('0' + v % 10); v /= 10; } while (v);
    buf[--len] = 'G';
    return make_symbol(make_string(buf + len, (int)sizeof buf - len));
}

P(p_symbol_value)
{
    (void)n;
    if (a[0] == NIL) return NIL;
    NEED_SYM(a[0]);
    NEED(SYM(a[0], SYM_VALUE) != UNBOUND, "Unbound variable", a[0]);
    return SYM(a[0], SYM_VALUE);
}

P(p_set)
{
    (void)n;
    NEED_SYM(a[0]);
    NEED(!(SYM_FLAGS_OF(a[0]) & SYMF_CONSTANT), "Cannot set a constant", a[0]);
    SYM(a[0], SYM_VALUE) = a[1];
    return a[1];
}

P(p_boundp)      { (void)n; if (a[0] == NIL) return S_T; NEED_SYM(a[0]); return boolean(SYM(a[0], SYM_VALUE) != UNBOUND); }
P(p_makunbound)  { (void)n; NEED_SYM(a[0]); NEED(!(SYM_FLAGS_OF(a[0]) & SYMF_CONSTANT), "Cannot unbind a constant", a[0]); SYM(a[0], SYM_VALUE) = UNBOUND; return a[0]; }
P(p_fboundp)     { (void)n; if (a[0] == NIL) return NIL; NEED_SYM(a[0]); return boolean(SYM(a[0], SYM_FUNCTION) != UNBOUND); }
P(p_fmakunbound) { (void)n; NEED_SYM(a[0]); SYM(a[0], SYM_FUNCTION) = UNBOUND; return a[0]; }

P(p_symbol_function)
{
    (void)n;
    NEED_SYM(a[0]);
    NEED(SYM(a[0], SYM_FUNCTION) != UNBOUND, "Undefined function", a[0]);
    return SYM(a[0], SYM_FUNCTION);
}

P(p_fset)
{
    (void)n;
    NEED_SYM(a[0]);
    NEED(SYM_FLAGS_OF(a[0]) & SYMF_FORM_MASK ? 0 : 1, "Cannot redefine a special form", a[0]);
    SYM(a[0], SYM_FUNCTION) = a[1];
    return a[1];
}

P(p_symbol_plist) { (void)n; if (a[0] == NIL) return NIL; NEED_SYM(a[0]); return SYM(a[0], SYM_PLIST); }
P(p_setplist)     { (void)n; NEED_SYM(a[0]); NEED_LIST(a[1]); SYM(a[0], SYM_PLIST) = a[1]; return a[1]; }

P(p_get)
{
    (void)n;
    if (a[0] == NIL) return NIL;
    NEED_SYM(a[0]);
    for (obj p = SYM(a[0], SYM_PLIST); CONSP(p) && CONSP(CDR(p)); p = CDR(CDR(p)))
        if (CAR(p) == a[1]) return CAR(CDR(p));
    return NIL;
}

/* (putprop symbol value indicator) */
P(p_putprop)
{
    (void)n;
    NEED_SYM(a[0]);
    for (obj p = SYM(a[0], SYM_PLIST); CONSP(p) && CONSP(CDR(p)); p = CDR(CDR(p)))
        if (CAR(p) == a[2]) { CAR(CDR(p)) = a[1]; return a[1]; }
    obj t = cons(a[1], SYM(a[0], SYM_PLIST));
    t = cons(a[2], t);
    SYM(a[0], SYM_PLIST) = t;
    return a[1];
}

/* ---- output ----------------------------------------------------------------- */

P(p_prin1) { (void)n; print_obj(a[0], 1, lisp_stdout); return a[0]; }
P(p_princ) { (void)n; print_obj(a[0], 0, lisp_stdout); return a[0]; }
P(p_print)
{
    (void)n;
    lisp_write("\n", 1);
    print_obj(a[0], 1, lisp_stdout);
    lisp_write(" ", 1);
    return a[0];
}
P(p_terpri)     { (void)a; (void)n; lisp_write("\n", 1); return NIL; }
P(p_fresh_line) { (void)a; (void)n; lisp_fresh_line(); return NIL; }
P(p_write_char)
{
    (void)n;
    NEED(CHARP(a[0]), "Not a character:", a[0]);
    char e[4];
    lisp_write(e, utf8_encode(CHAR_CODE(a[0]), e));
    return a[0];
}

static obj to_string(obj x, int escape)
{
    capture_len = 0;
    print_obj(x, escape, &capture_sink);
    return make_string_utf8(capture_buf, capture_len);    /* the printer writes UTF-8 */
}

P(p_prin1_to_string) { (void)n; return to_string(a[0], 1); }

/* Does X hold an instance (#(:instance ...)) anywhere the printer would
 * look: in its conses, plain vectors and array data? (M14: then the Lisp
 * printer, with PRINT-OBJECT, prints it; else the C one.) */
static int holds_instance(obj x, int depth, long *budget)
{
    for (;;) {
        if (--*budget < 0 || depth > 200) return 1;      /* too big to tell: let Lisp print it */
        if (CONSP(x)) {
            if (holds_instance(CAR(x), depth + 1, budget)) return 1;
            x = CDR(x);
            continue;
        }
        if (ARRAYP(x)) { x = FIELD(x, AR_DATA); continue; }
        if (!VECTORP(x)) return 0;
        int n = VEC_LEN(x);
        if (n > 0 && TAG(VEC_REF(x, 0)) == TAG_SYMBOL) {
            return str_eq_utf8(SYM(VEC_REF(x, 0), SYM_NAME), ":INSTANCE", 9);   /* other tagged objects print as #<...> */
        }
        for (int i = 0; i < n; i++)
            if (holds_instance(VEC_REF(x, i), depth + 1, budget)) return 1;
        return 0;
    }
}

P(p_holds_instance) { (void)n; long budget = 100000; return boolean(holds_instance(a[0], 0, &budget)); }
P(p_princ_to_string) { (void)n; return to_string(a[0], 0); }

/* ---- strings and characters ----------------------------------------------- */

P(p_string_eq)
{
    (void)n;
    NEED_STRING(a[0]);
    NEED_STRING(a[1]);
    return boolean(str_equal(a[0], a[1]));
}

P(p_string_append)
{
    long total = 0;
    int wide = 0;
    for (int i = 0; i < n; i++) {
        NEED_STRING(a[i]);
        total += STR_LEN(a[i]);
        if (str_wide_p(a[i], 0, STR_LEN(a[i]))) wide = 1;
    }
    NEED(total < (1L << 24), "String too long", NO_IRRITANT);
    obj s = make_string_of((int)total, wide);     /* a[] is on the stack: kept up to date */
    long pos = 0;
    for (int i = 0; i < n; i++) {
        int len = STR_LEN(a[i]);
        if (!wide && BSTRINGP(a[i])) memcpy(STR_DATA(s) + pos, STR_DATA(a[i]), (__SIZE_TYPE__)len);
        else for (int k = 0; k < len; k++) str_set(s, (int)pos + k, str_ref(a[i], k));
        pos += len;
    }
    return s;
}

P(p_string)
{
    (void)n;
    obj x = a[0];
    if (STRINGP(x)) return x;
    if (x == NIL) return make_string("NIL", 3);
    if (TAG(x) == TAG_SYMBOL) return lisp_symbol_name(x);
    if (CHARP(x)) { int c = CHAR_CODE(x); return make_string_codes(&c, 1); }
    return lisp_error("Cannot make a string from", x);
}

P(p_substring)
{
    NEED_STRING(a[0]);
    NEED_FIX(a[1]);
    fixnum_t len = STR_LEN(a[0]), start = FIX(a[1]), end = len;
    if (n > 2 && a[2] != NIL) { NEED_FIX(a[2]); end = FIX(a[2]); }
    NEED(start >= 0 && start <= end && end <= len, "Bad substring bounds", a[1]);
    int wide = str_wide_p(a[0], (int)start, (int)end);
    obj s = make_string_of((int)(end - start), wide);
    if (BSTRINGP(a[0])) memcpy(STR_DATA(s), STR_DATA(a[0]) + start, (__SIZE_TYPE__)(end - start));
    else for (fixnum_t i = start; i < end; i++) str_set(s, (int)(i - start), str_ref(a[0], (int)i));
    return s;
}

P(p_char_code) { (void)n; NEED(CHARP(a[0]), "Not a character:", a[0]); return MAKE_FIX(CHAR_CODE(a[0])); }
P(p_code_char) { (void)n; NEED_FIX(a[0]); NEED(FIX(a[0]) >= 0 && FIX(a[0]) < 0x110000, "Bad character code", a[0]); return MAKE_CHAR(FIX(a[0])); }

/* ---- vectors (and indexing into strings) ---------------------------------- */

P(p_vector)
{
    obj v = make_vector(n, NIL);
    for (int i = 0; i < n; i++) VEC_REF(v, i) = a[i];
    return v;
}

P(p_make_vector)
{
    NEED_FIX(a[0]);
    fixnum_t len = FIX(a[0]);
    NEED(len >= 0 && (uint64_t)len < heap_limit / 2, "Bad vector length", a[0]);
    return make_vector((int)len, n > 1 ? a[1] : NIL);
}

static int index_ok(obj x, obj i, int *len)
{
    *len = STRINGP(x) ? STR_LEN(x) : VECTORP(x) ? VEC_LEN(x) : -1;
    return *len >= 0 && FIXNUMP(i) && FIX(i) >= 0 && FIX(i) < *len;
}

P(p_aref)
{
    int len;
    if (ARRAYP(a[0])) return array_ref(a[0], a + 1, n - 1);
    NEED(STRINGP(a[0]) || VECTORP(a[0]), "Not an array:", a[0]);
    NEED(n == 2, "Wrong number of subscripts for a vector:", MAKE_FIX(n - 1));
    NEED(index_ok(a[0], a[1], &len), "Index out of range:", a[1]);
    if (STRINGP(a[0])) return MAKE_CHAR(str_ref(a[0], (int)FIX(a[1])));
    return VEC_REF(a[0], FIX(a[1]));
}

/* (aset value array index) — Lisp Machine argument order */
P(p_aset)
{
    int len;
    if (ARRAYP(a[1])) return array_set(a[1], a + 2, n - 2, a[0]);
    NEED(STRINGP(a[1]) || VECTORP(a[1]), "Not an array:", a[1]);
    NEED(n == 3, "Wrong number of subscripts for a vector:", MAKE_FIX(n - 2));
    NEED(index_ok(a[1], a[2], &len), "Index out of range:", a[2]);
    if (STRINGP(a[1])) {
        NEED(CHARP(a[0]), "Not a character:", a[0]);
        NEED(str_set(a[1], (int)FIX(a[2]), CHAR_CODE(a[0])), "Too wide a character for a narrow string:", a[0]);
    } else {
        VEC_REF(a[1], FIX(a[2])) = a[0];
    }
    return a[0];
}

/* ---- system ---------------------------------------------------------------- */

/* ---- conditions -------------------------------------------------------------- */

/* (error "message" irritants...)  or  (error 'type "message" irritants...) */
/* the debugger's frame primitives are in vm.c */
obj p_frames(obj *a, int n);
obj p_values(obj *a, int n);
obj p_frame_function(obj *a, int n);
obj p_frame_locals(obj *a, int n);
obj p_frame_args(obj *a, int n);
obj p_frame_return(obj *a, int n);
obj p_frame_reinvoke(obj *a, int n);

/* the name of a compiled function (a symbol, a list like (METHOD ...), or NIL) */
P(p_function_name)
{
    (void)n;
    if (CFUNCP(a[0])) return FIELD(a[0], CF_NAME);
    if (PRIMITIVEP(a[0])) return intern_c(prims[FIX(FIELD(a[0], 0))].name);
    return NIL;
}

P(p_set_function_name)
{
    (void)n;
    NEED(CFUNCP(a[0]), "Not a compiled function:", a[0]);
    FIELD(a[0], CF_NAME) = a[1];
    return a[1];
}

/* the world's Lisp source as it was built into the kernel: the editor's files */
extern const char *const world_source;
extern const long world_source_len;
P(p_world_source)
{
    (void)a; (void)n;
    return make_string_utf8(world_source, (int)world_source_len);
}

/* (%split-lines string) => the lines of STRING, without their newlines */
P(p_split_lines)
{
    (void)n;
    NEED(STRINGP(a[0]), "Not a string:", a[0]);
    obj s = a[0], list = NIL, line = NIL;
    PROTECT3(s, list, line);
    int end = (int)STR_LEN(s);
    for (int i = (int)STR_LEN(s) - 1; i >= -1; i--) {
        if (i < 0 || str_ref(s, i) == '\n') {
            line = make_string_of(end - (i + 1), str_wide_p(s, i + 1, end));
            for (int k = i + 1; k < end; k++) str_set(line, k - (i + 1), str_ref(s, k));   /* s after any GC */
            list = cons(line, list);
            end = i;
        }
    }
    UNPROTECT(3);
    return list;
}

/* ---- hashing, for hash tables (world/cl.lisp) ----------------------------
 *
 * (%hash x test) with TEST one of EQ, EQL, EQUAL => a fixnum >= 0 that only
 * depends on X's contents (numbers, characters, symbols by name, and for
 * EQUAL strings and list structure), or a negative fixnum -1-h when h comes
 * from X's address: the collector moves objects, so a table holding such
 * keys rehashes after a collection. */

#define HASH_MASK ((UINT64_C(1) << 58) - 1)

static uint64_t mix(uint64_t h, uint64_t v) { h ^= v; h *= 1099511628211u; return h; }

static uint64_t hash_of(obj x, int equal_p, int depth, int *by_address)
{
    if (FIXNUMP(x)) return (uint64_t)FIX(x) * 2654435761u;
    if (TAG(x) == TAG_IMM) return x;                               /* NIL, characters */
    if (TAG(x) == TAG_SYMBOL) {
        return (uint64_t)str_hash(SYM(x, SYM_NAME)) + 7;
    }
    if (BIGNUMP(x)) return big_hash(x);
    if (RATIOP(x)) return mix(hash_of(FIELD(x, RAT_NUM), 0, depth, by_address), hash_of(FIELD(x, RAT_DEN), 0, depth, by_address));
    if (FLOATP(x)) return flo_hash(x);
    if (equal_p && STRINGP(x)) return str_hash(x);      /* by characters: narrow and wide alike */
    if (equal_p && CONSP(x)) {
        uint64_t h = 31;
        for (int i = 0; i < 8 && CONSP(x); i++, x = CDR(x))
            h = mix(h, depth < 4 ? hash_of(CAR(x), 1, depth + 1, by_address) : 17);
        return depth < 4 && !CONSP(x) && x != NIL ? mix(h, hash_of(x, 1, depth + 1, by_address)) : h;
    }
    *by_address = 1;
    return IDX(x) * 2654435761u;
}

P(p_hash)
{
    (void)n;
    int by_address = 0;
    int equal_p = a[1] == intern_c("EQUAL");
    uint64_t h = hash_of(a[0], equal_p, 0, &by_address) & HASH_MASK;
    return MAKE_FIX(by_address ? -1 - (fixnum_t)h : (fixnum_t)h);
}

P(p_gc_count) { (void)a; (void)n; return MAKE_FIX((fixnum_t)gc_count); }

P(p_error)
{
    int i = 0;
    obj type = S_ERROR, msg = NIL, irritants = NIL;
    if (TAG(a[0]) == TAG_SYMBOL) { type = a[0]; i = 1; }
    PROTECT3(type, msg, irritants);
    for (int k = n - 1; k > i; k--) irritants = cons(a[k], irritants);
    if (i < n) msg = STRINGP(a[i]) ? a[i] : to_string(a[i], 0);
    else msg = SYM(type, SYM_NAME);
    obj c = make_condition(type, msg, irritants);
    UNPROTECT(3);
    return lisp_signal(c);
}

#define NEED_COND(x) NEED(CONDITIONP(x), "Not a condition:", x)
P(p_resignal)            { (void)n; NEED_COND(a[0]); return lisp_signal(a[0]); }

/* (%unhandled c): no handler took C; on to the debugger, past the hook */
P(p_unhandled)
{
    (void)n;
    extern int signal_hook_done;
    NEED_COND(a[0]);
    signal_hook_done = 1;
    return lisp_signal(a[0]);
}

/* (%make-condition type message irritants data) */
P(p_make_condition)
{
    (void)n;
    obj c = make_condition(a[0], a[1], a[2]);
    FIELD(c, COND_DATA) = a[3];
    return c;
}

P(p_condition_data)      { (void)n; NEED_COND(a[0]); return FIELD(a[0], COND_DATA); }
P(p_conditionp)          { (void)n; return boolean(CONDITIONP(a[0])); }
P(p_condition_type)      { (void)n; NEED_COND(a[0]); return FIELD(a[0], COND_TYPE); }
P(p_condition_message)   { (void)n; NEED_COND(a[0]); return FIELD(a[0], COND_MESSAGE); }
P(p_condition_irritants) { (void)n; NEED_COND(a[0]); return FIELD(a[0], COND_IRRITANTS); }

/* (%condition-is c type): ERROR and T match every condition */
P(p_condition_is)
{
    (void)n;
    NEED_COND(a[0]);
    return boolean(a[1] == S_T || a[1] == S_ERROR || a[1] == FIELD(a[0], COND_TYPE));
}

/* ---- support for the compiler ------------------------------------------------- */

static int slen(const char *s) { int k = 0; while (s[k]) k++; return k; }

/* (%make-code bytes consts name info) -> compiled function */
P(p_make_code)
{
    (void)n;
    NEED(VECTORP(a[1]), "Not a vector:", a[1]);
    NEED_FIX(a[3]);
    fixnum_t len = list_length(a[0]);
    NEED(len >= 0 && len < 65536, "Bad bytecode list", a[0]);
    obj code = alloc_object(HT_STRING, (uint64_t)len, TAG_OBJECT);
    PROTECT1(code);
    obj f = alloc_object(HT_CFUNC, CF_NFIELDS, TAG_OBJECT);
    UNPROTECT(1);
    int i = 0;
    for (obj b = a[0]; CONSP(b); b = CDR(b), i++) {
        NEED(FIXNUMP(CAR(b)) && FIX(CAR(b)) >= 0 && FIX(CAR(b)) < 256, "Bad byte", CAR(b));
        STR_DATA(code)[i] = (char)FIX(CAR(b));
    }
    FIELD(f, CF_CODE) = code;
    FIELD(f, CF_CONSTS) = a[1];
    FIELD(f, CF_NAME) = a[2];
    FIELD(f, CF_INFO) = a[3];
    FIELD(f, CF_ENV) = NIL;
    return f;
}

/* (%code-parts f) -> (bytes consts name info env) */
P(p_code_parts)
{
    (void)n;
    NEED(CFUNCP(a[0]), "Not a compiled function:", a[0]);
    obj bytes = NIL, r = NIL;
    PROTECT2(bytes, r);
    for (int i = STR_LEN(FIELD(a[0], CF_CODE)) - 1; i >= 0; i--)
        bytes = cons(MAKE_FIX((unsigned char)STR_DATA(FIELD(a[0], CF_CODE))[i]), bytes);
    r = cons(FIELD(a[0], CF_ENV), NIL);
    r = cons(FIELD(a[0], CF_INFO), r);
    r = cons(FIELD(a[0], CF_NAME), r);
    r = cons(FIELD(a[0], CF_CONSTS), r);
    r = cons(bytes, r);
    UNPROTECT(2);
    return r;
}

/* (%opcodes) -> ((name operand-format) ...) in opcode order */
P(p_opcodes)
{
    (void)a; (void)n;
    obj r = NIL, e = NIL, s = NIL;
    PROTECT3(r, e, s);
    for (int i = nopcodes - 1; i >= 0; i--) {
        e = make_string(opcodes[i].operands, slen(opcodes[i].operands));
        s = intern_c(opcodes[i].name);
        e = list2(s, e);
        r = cons(e, r);
    }
    UNPROTECT(3);
    return r;
}

P(p_macro_function)
{
    (void)n;
    if (a[0] == NIL) return NIL;
    NEED_SYM(a[0]);
    obj f = SYM(a[0], SYM_FUNCTION);
    return f != UNBOUND && MACROP(f) ? f : NIL;
}

P(p_special_variable_p)
{
    (void)n;
    if (TAG(a[0]) != TAG_SYMBOL) return NIL;
    return boolean(SYM_FLAGS_OF(a[0]) & SYMF_SPECIAL);
}

P(p_proclaim_special)
{
    (void)n;
    NEED_SYM(a[0]);
    NEED(!(SYM_FLAGS_OF(a[0]) & SYMF_CONSTANT), "Cannot make a constant special", a[0]);
    SYM(a[0], SYM_FLAGS) = MAKE_FIX(SYM_FLAGS_OF(a[0]) | SYMF_SPECIAL);
    return a[0];
}

/* (%closure-parts f) -> (lambda-list body env name macrop) for interpreted functions */
P(p_closure_parts)
{
    (void)n;
    NEED(CLOSUREP(a[0]) || OBJECTP(a[0], HT_MACRO), "Not an interpreted function:", a[0]);
    obj r = NIL;
    PROTECT1(r);
    r = cons(boolean(OBJECTP(a[0], HT_MACRO)), NIL);
    r = cons(FIELD(a[0], FN_NAME), r);
    r = cons(FIELD(a[0], FN_ENV), r);
    r = cons(FIELD(a[0], FN_BODY), r);
    r = cons(FIELD(a[0], FN_LAMBDA_LIST), r);
    UNPROTECT(1);
    return r;
}

/* every symbol in every package (each once: only those at home there) */
P(p_all_symbols)
{
    (void)a; (void)n;
    int count = 0;
#define EACH_SYMBOL(body)                                                           \
    for (obj l = package_list; l != NIL; l = CDR(l)) {                              \
        obj t = VEC_REF(CAR(l), PK_TABLE);                                          \
        if (!VECTORP(t)) continue;                                                  \
        for (int h = 0; h < VEC_LEN(t); h++)                                        \
            for (obj b = VEC_REF(t, h); b != NIL; b = CDR(b))                       \
                if (t == obarray || SYM(CAR(b), SYM_PACKAGE) == CAR(l)) { body; }   \
    }
    EACH_SYMBOL(count++)
    obj v = make_vector(count, NIL), r = NIL;
    PROTECT2(v, r);
    int i = 0;
    EACH_SYMBOL(VEC_REF(v, i++) = CAR(b))
#undef EACH_SYMBOL
    for (i = count - 1; i >= 0; i--) r = cons(VEC_REF(v, i), r);
    UNPROTECT(2);
    return r;
}

P(p_set_toplevel_evaluator) { (void)n; toplevel_evaluator = a[0]; return a[0]; }
P(p_compiled_function_p)    { (void)n; return boolean(CFUNCP(a[0])); }
P(p_interpreted_function_p) { (void)n; return boolean(CLOSUREP(a[0]) || OBJECTP(a[0], HT_MACRO)); }

/* ---- worlds ------------------------------------------------------------------- */

/* (disk-save): the host saves the world as soon as the current top-level
 * form has finished (an image can only be taken while the machine is idle). */
P(p_disk_save)    { (void)a; (void)n; world_save_requested = 1; return S_T; }
/* (forget-world): drop the saved world, so the next start boots from source */
P(p_forget_world) { (void)a; (void)n; world_forget_requested = 1; return S_T; }

P(p_gc) { (void)a; (void)n; gc(); return MAKE_FIX((fixnum_t)heap_used); }

static void put_num(uint64_t v)
{
    char buf[24];
    int len = sizeof buf;
    do { buf[--len] = (char)('0' + v % 10); v /= 10; } while (v);
    lisp_write(buf + len, (int)sizeof buf - len);
}

P(p_room)
{
    (void)a; (void)n;
    lisp_fresh_line();
    lisp_puts("Heap: ");
    put_num(heap_used);
    lisp_puts(" of ");
    put_num(heap_limit);
    lisp_puts(" words in use (");
    put_num(heap_used * 100 / heap_limit);
    lisp_puts("%), ");
    put_num(gc_count);
    lisp_puts(" collections, ");
    put_num(eval_steps);
    lisp_puts(" evaluator steps.\n");
    return NIL;
}

P(p_real_time) { (void)a; (void)n; return MAKE_FIX((fixnum_t)lisp_now_ms); }


/* graphics, events, host (gprims.c) */
#define G(name) obj name(obj *a, int n);
G(gp_bitblt) G(gp_draw_rect) G(gp_draw_char) G(gp_pixel) G(gp_screen_width) G(gp_screen_height)
G(gp_make_bytes) G(gp_next_event) G(gp_wait) G(gp_host_write) G(gp_host_fresh_line)
G(gp_world_changed) G(gp_heap_stats) G(gp_read_from_string) G(gp_logand) G(gp_logior)
G(gp_logxor) G(gp_ash) G(gp_integer_length) G(gp_logcount) G(gp_process_yield) G(gp_process_sleep) G(gp_store_conditional)
G(gp_file_request) G(gp_file_server_p)
G(gp_read_fixups_p) G(p_progv_bind) G(p_progv_unbind) G(fp_wall_clock) G(fp_utc_offset) G(ap_make_array) G(ap_array_header_p) G(ap_array_field) G(ap_set_array_field)
G(tp_char_upcase) G(tp_char_downcase) G(tp_upper_case_p) G(tp_lower_case_p) G(tp_both_case_p)
G(tp_alpha_char_p) G(tp_graphic_char_p) G(tp_make_string) G(tp_wide_string_p) G(tp_widen)
G(tp_utf8_encode) G(tp_utf8_decode)
G(pk_package_list) G(pk_find_package) G(pk_make_package) G(pk_find_symbol) G(pk_intern)
G(pk_symbol_package) G(pk_package_add) G(pk_package_remove) G(pk_set_external) G(pk_externalp) G(pk_delete_package)
G(fp_floatp) G(fp_numberp) G(fp_float) G(fp_truncate) G(fp_float_function) G(fp_integer_decode)
#undef G

/* ---- table ------------------------------------------------------------------ */

const prim_t prims[] = {
    /* handled inside the evaluator; order must match PRIM_* */
    { "FUNCALL", 0, 1, -1 },
    { "APPLY", 0, 1, -1 },
    { "EVAL", 0, 1, 1 },
    { "%INTERPRET", 0, 1, 1 },

    { "CAR", p_car, 1, 1 },
    { "CDR", p_cdr, 1, 1 },
    { "CONS", p_cons, 2, 2 },
    { "RPLACA", p_rplaca, 2, 2 },
    { "RPLACD", p_rplacd, 2, 2 },
    { "LIST", p_list, 0, -1 },
    { "APPEND", p_append, 0, -1 },
    { "LENGTH", p_length, 1, 1 },
    { "REVERSE", p_reverse, 1, 1 },
    { "NREVERSE", p_nreverse, 1, 1 },

    { "EQ", p_eq, 2, 2 },
    { "EQL", p_eql, 2, 2 },
    { "EQUAL", p_equal, 2, 2 },
    { "NULL", p_null, 1, 1 },
    { "NOT", p_null, 1, 1 },
    { "ATOM", p_atom, 1, 1 },
    { "CONSP", p_consp, 1, 1 },
    { "LISTP", p_listp, 1, 1 },
    { "SYMBOLP", p_symbolp, 1, 1 },
    { "INTEGERP", p_integerp, 1, 1 },
    { "NUMBERP", p_integerp, 1, 1 },
    { "STRINGP", p_stringp, 1, 1 },
    { "VECTORP", p_vectorp, 1, 1 },
    { "CHARACTERP", p_characterp, 1, 1 },
    { "FUNCTIONP", p_functionp, 1, 1 },

    { "+", p_add, 0, -1 },
    { "-", p_sub, 1, -1 },
    { "*", p_mul, 0, -1 },
    { "/", p_div, 1, -1 },
    { "%QUOTIENT", p_quotient, 2, -1 },
    { "NUMERATOR", p_numerator, 1, 1 },
    { "DENOMINATOR", p_denominator, 1, 1 },
    { "RATIONALP", p_rationalp, 1, 1 },
    { "REM", p_rem, 2, 2 },
    { "MOD", p_mod, 2, 2 },
    { "1+", p_1plus, 1, 1 },
    { "1-", p_1minus, 1, 1 },
    { "=", p_num_eq, 1, -1 },
    { "<", p_lt, 1, -1 },
    { ">", p_gt, 1, -1 },
    { "<=", p_le, 1, -1 },
    { ">=", p_ge, 1, -1 },

    { "SYMBOL-NAME", p_symbol_name, 1, 1 },
    { "%SYMBOL-NAME", p_raw_symbol_name, 1, 1 },
    { "INTERN", pk_intern, 1, 2 },
    { "MAKE-SYMBOL", p_make_symbol, 1, 1 },
    { "GENSYM", p_gensym, 0, 0 },
    { "SYMBOL-VALUE", p_symbol_value, 1, 1 },
    { "SET", p_set, 2, 2 },
    { "BOUNDP", p_boundp, 1, 1 },
    { "MAKUNBOUND", p_makunbound, 1, 1 },
    { "SYMBOL-FUNCTION", p_symbol_function, 1, 1 },
    { "FSET", p_fset, 2, 2 },
    { "FBOUNDP", p_fboundp, 1, 1 },
    { "FMAKUNBOUND", p_fmakunbound, 1, 1 },
    { "SYMBOL-PLIST", p_symbol_plist, 1, 1 },
    { "SETPLIST", p_setplist, 2, 2 },
    { "GET", p_get, 2, 2 },
    { "PUTPROP", p_putprop, 3, 3 },

    { "PRIN1", p_prin1, 1, 1 },
    { "PRINC", p_princ, 1, 1 },
    { "PRINT", p_print, 1, 1 },
    { "TERPRI", p_terpri, 0, 0 },
    { "FRESH-LINE", p_fresh_line, 0, 0 },
    { "WRITE-CHAR", p_write_char, 1, 1 },
    { "PRIN1-TO-STRING", p_prin1_to_string, 1, 1 },
    { "PRINC-TO-STRING", p_princ_to_string, 1, 1 },
    { "%PRIN1-TO-STRING", p_prin1_to_string, 1, 1 },
    { "%PRINC-TO-STRING", p_princ_to_string, 1, 1 },
    { "%HOLDS-INSTANCE-P", p_holds_instance, 1, 1 },

    { "STRING=", p_string_eq, 2, 2 },
    { "STRING-APPEND", p_string_append, 0, -1 },
    { "STRING", p_string, 1, 1 },
    { "SUBSTRING", p_substring, 2, 3 },
    { "CHAR-CODE", p_char_code, 1, 1 },
    { "CODE-CHAR", p_code_char, 1, 1 },

    { "VECTOR", p_vector, 0, -1 },
    { "MAKE-VECTOR", p_make_vector, 1, 2 },
    { "AREF", p_aref, 1, -1 },
    { "ASET", p_aset, 2, -1 },

    { "ERROR", p_error, 1, -1 },
    { "%ERROR", p_error, 1, -1 },
    { "%RESIGNAL", p_resignal, 1, 1 },
    { "CONDITIONP", p_conditionp, 1, 1 },
    { "CONDITION-TYPE", p_condition_type, 1, 1 },
    { "CONDITION-MESSAGE", p_condition_message, 1, 1 },
    { "CONDITION-IRRITANTS", p_condition_irritants, 1, 1 },
    { "%CONDITION-IS", p_condition_is, 2, 2 },
    { "%UNHANDLED", p_unhandled, 1, 1 },
    { "%MAKE-CONDITION", p_make_condition, 4, 4 },
    { "%CONDITION-DATA", p_condition_data, 1, 1 },

    { "%MAKE-CODE", p_make_code, 4, 4 },
    { "%CODE-PARTS", p_code_parts, 1, 1 },
    { "%OPCODES", p_opcodes, 0, 0 },
    { "MACRO-FUNCTION", p_macro_function, 1, 1 },
    { "SPECIAL-VARIABLE-P", p_special_variable_p, 1, 1 },
    { "%PROCLAIM-SPECIAL", p_proclaim_special, 1, 1 },
    { "%CLOSURE-PARTS", p_closure_parts, 1, 1 },
    { "%ALL-SYMBOLS", p_all_symbols, 0, 0 },
    { "%SET-TOPLEVEL-EVALUATOR", p_set_toplevel_evaluator, 1, 1 },
    { "COMPILED-FUNCTION-P", p_compiled_function_p, 1, 1 },
    { "%INTERPRETED-FUNCTION-P", p_interpreted_function_p, 1, 1 },
    { "DISK-SAVE", p_disk_save, 0, 0 },
    { "FORGET-WORLD", p_forget_world, 0, 0 },

    { "BITBLT", gp_bitblt, 9, 9 },
    { "%DRAW-RECTANGLE", gp_draw_rect, 6, 7 },
    { "%DRAW-CHAR", gp_draw_char, 5, 6 },
    { "%BITMAP-PIXEL", gp_pixel, 3, 3 },
    { "%SCREEN-WIDTH", gp_screen_width, 0, 0 },
    { "%SCREEN-HEIGHT", gp_screen_height, 0, 0 },
    { "%MAKE-BYTES", gp_make_bytes, 1, 1 },
    { "%NEXT-EVENT", gp_next_event, 0, 0 },
    { "%WAIT-FOR-EVENT", gp_wait, 0, 1 },
    { "%HOST-WRITE", gp_host_write, 1, 1 },
    { "%HOST-FRESH-LINE", gp_host_fresh_line, 0, 0 },
    { "%WORLD-CHANGED", gp_world_changed, 0, 0 },
    { "%HEAP-STATS", gp_heap_stats, 0, 0 },
    { "%READ-FROM-STRING", gp_read_from_string, 1, 2 },
    { "%READ-FIXUPS-P", gp_read_fixups_p, 0, 0 },
    { "%PROGV-BIND", p_progv_bind, 2, 2 },
    { "%PROGV-UNBIND", p_progv_unbind, 1, 1 },
    { "%WALL-CLOCK", fp_wall_clock, 0, 0 },
    { "%UTC-OFFSET", fp_utc_offset, 1, 1 },
    { "LOGAND", gp_logand, 0, -1 },
    { "LOGIOR", gp_logior, 0, -1 },
    { "LOGXOR", gp_logxor, 0, -1 },
    { "ASH", gp_ash, 2, 2 },
    { "INTEGER-LENGTH", gp_integer_length, 1, 1 },
    { "LOGCOUNT", gp_logcount, 1, 1 },
    { "%PROCESS-YIELD", gp_process_yield, 0, 0 },
    { "%PROCESS-SLEEP", gp_process_sleep, 1, 1 },
    { "STORE-CONDITIONAL", gp_store_conditional, 4, 4 },

    { "VALUES", p_values, 0, -1 },
    { "%FRAMES", p_frames, 0, 0 },
    { "%FRAME-FUNCTION", p_frame_function, 1, 1 },
    { "%FRAME-LOCALS", p_frame_locals, 1, 1 },
    { "%FRAME-ARGS", p_frame_args, 1, 1 },
    { "%FRAME-RETURN", p_frame_return, 2, 2 },
    { "%FRAME-REINVOKE", p_frame_reinvoke, 3, 3 },
    { "%SET-FUNCTION-NAME", p_set_function_name, 2, 2 },
    { "%FUNCTION-NAME", p_function_name, 1, 1 },
    { "%WORLD-SOURCE", p_world_source, 0, 0 },
    { "%SPLIT-LINES", p_split_lines, 1, 1 },
    { "%HASH", p_hash, 2, 2 },
    { "%GC-COUNT", p_gc_count, 0, 0 },
    { "%FILE-REQUEST", gp_file_request, 3, 3 },
    { "%FILE-SERVER-P", gp_file_server_p, 0, 0 },

    { "GC", p_gc, 0, 0 },
    { "ROOM", p_room, 0, 0 },
    { "GET-INTERNAL-REAL-TIME", p_real_time, 0, 0 },

    /* arrays (array.c) */
    { "CHAR-UPCASE", tp_char_upcase, 1, 1 },
    { "CHAR-DOWNCASE", tp_char_downcase, 1, 1 },
    { "UPPER-CASE-P", tp_upper_case_p, 1, 1 },
    { "LOWER-CASE-P", tp_lower_case_p, 1, 1 },
    { "BOTH-CASE-P", tp_both_case_p, 1, 1 },
    { "ALPHA-CHAR-P", tp_alpha_char_p, 1, 1 },
    { "GRAPHIC-CHAR-P", tp_graphic_char_p, 1, 1 },
    { "%MAKE-STRING", tp_make_string, 2, 2 },
    { "%WIDE-STRING-P", tp_wide_string_p, 1, 1 },
    { "%WIDEN", tp_widen, 1, 1 },
    { "%UTF8-ENCODE", tp_utf8_encode, 1, 1 },
    { "%UTF8-DECODE", tp_utf8_decode, 1, 1 },
    { "%MAKE-ARRAY", ap_make_array, 5, 5 },
    { "%ARRAY-HEADER-P", ap_array_header_p, 1, 1 },
    { "%ARRAY-FIELD", ap_array_field, 2, 2 },
    { "%SET-ARRAY-FIELD", ap_set_array_field, 3, 3 },

    /* packages (package.c) */
    { "%PACKAGE-LIST", pk_package_list, 0, 0 },
    { "%INTERN", pk_intern, 2, 2 },
    { "%DELETE-PACKAGE", pk_delete_package, 1, 1 },
    { "%FIND-PACKAGE", pk_find_package, 1, 1 },
    { "%MAKE-PACKAGE", pk_make_package, 3, 3 },
    { "%FIND-SYMBOL", pk_find_symbol, 2, 2 },
    { "SYMBOL-PACKAGE", pk_symbol_package, 1, 1 },
    { "%PACKAGE-ADD", pk_package_add, 2, 2 },
    { "%PACKAGE-REMOVE", pk_package_remove, 2, 2 },
    { "%SET-EXTERNAL", pk_set_external, 2, 2 },
    { "%EXTERNALP", pk_externalp, 1, 1 },

    /* floats (float.c) */
    { "FLOATP", fp_floatp, 1, 1 },
    { "NUMBERP", fp_numberp, 1, 1 },
    { "FLOAT", fp_float, 1, 2 },
    { "%TRUNCATE", fp_truncate, 1, 1 },
    { "%FLOAT-FUNCTION", fp_float_function, 2, 3 },
    { "%INTEGER-DECODE-FLOAT", fp_integer_decode, 1, 1 },
};

const int nprims = sizeof prims / sizeof prims[0];

void prims_init(void)
{
    for (int i = 0; i < nprims; i++) {
        obj p = make_primitive(i);
        PROTECT1(p);
        obj s = intern_c(prims[i].name);
        SYM(s, SYM_FUNCTION) = p;
        UNPROTECT(1);
    }
}
