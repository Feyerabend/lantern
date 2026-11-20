/* obj.c — constructors, symbols and the obarray. */
#include "lisp.h"

#define OBARRAY_SIZE 1021

obj obarray = NIL;
obj S_T, S_QUOTE, S_FUNCTION, S_LAMBDA, S_OPTIONAL, S_REST, S_BODY,
    S_UNQUOTE, S_UNQUOTE_SPLICING, S_APPEND, S_LIST, S_PROGN, S_LET, S_LETSTAR, S_ERROR;

int k_memeq(const void *a, const void *b, int n)
{
    const unsigned char *p = a, *q = b;
    for (int i = 0; i < n; i++)
        if (p[i] != q[i]) return 0;
    return 1;
}

obj alloc_object(unsigned type, uint64_t len, unsigned tag)
{
    uint64_t words = RAW_TYPE(type) ? (len + 7) / 8 : len;
    if (words == 0) words = 1;
    uint64_t i = alloc_words(1 + words);
    heap[i] = MAKE_HEADER(type, len);
    obj fill = RAW_TYPE(type) ? 0 : NIL;
    for (uint64_t k = 1; k <= words; k++) heap[i + k] = fill;
    return MAKE_PTR(i, tag);
}

obj cons(obj a, obj d)
{
    PROTECT2(a, d);
    uint64_t i = alloc_words(2);
    UNPROTECT(2);
    heap[i] = a;
    heap[i + 1] = d;
    return MAKE_PTR(i, TAG_CONS);
}

obj list2(obj a, obj b)
{
    PROTECT1(a);
    obj t = cons(b, NIL);
    UNPROTECT(1);
    return cons(a, t);
}

obj make_string(const char *s, int len)
{
    obj x = alloc_object(HT_STRING, (uint64_t)len, TAG_OBJECT);
    if (len) memcpy(STR_DATA(x), s, (__SIZE_TYPE__)len);
    return x;
}

obj make_vector(int n, obj init)
{
    PROTECT1(init);
    obj v = alloc_object(HT_VECTOR, (uint64_t)n, TAG_OBJECT);
    UNPROTECT(1);
    for (int i = 0; i < n; i++) VEC_REF(v, i) = init;
    return v;
}

obj make_closure(unsigned type, obj ll, obj body, obj env, obj name)
{
    PROTECT4(ll, body, env, name);
    obj f = alloc_object(type, FN_NFIELDS, TAG_OBJECT);
    UNPROTECT(4);
    FIELD(f, FN_LAMBDA_LIST) = ll;
    FIELD(f, FN_BODY) = body;
    FIELD(f, FN_ENV) = env;
    FIELD(f, FN_NAME) = name;
    return f;
}

obj make_primitive(int index)
{
    obj p = alloc_object(HT_PRIMITIVE, 1, TAG_OBJECT);
    FIELD(p, 0) = MAKE_FIX(index);
    return p;
}

obj make_condition(obj type, obj message, obj irritants)
{
    PROTECT3(type, message, irritants);
    obj c = alloc_object(HT_CONDITION, COND_NFIELDS, TAG_OBJECT);
    UNPROTECT(3);
    FIELD(c, COND_TYPE) = type;
    FIELD(c, COND_MESSAGE) = message;
    FIELD(c, COND_IRRITANTS) = irritants;
    FIELD(c, COND_DATA) = NIL;
    return c;
}

obj make_symbol(obj name)
{
    PROTECT1(name);
    obj s = alloc_object(HT_SYMBOL, SYM_NFIELDS, TAG_SYMBOL);
    UNPROTECT(1);
    SYM(s, SYM_NAME) = name;
    SYM(s, SYM_VALUE) = UNBOUND;
    SYM(s, SYM_FUNCTION) = UNBOUND;
    SYM(s, SYM_PLIST) = NIL;
    SYM(s, SYM_FLAGS) = MAKE_FIX(0);
    SYM(s, SYM_PACKAGE) = NIL;
    return s;
}

obj intern(const char *s, int len)
{
    if (len == 3 && k_memeq(s, "NIL", 3)) return NIL;
    uint32_t h = utf8_hash(s, len) % OBARRAY_SIZE;      /* S is UTF-8 (M15) */
    for (obj b = VEC_REF(obarray, h); b != NIL; b = CDR(b))
        if (str_eq_utf8(SYM(CAR(b), SYM_NAME), s, len)) return CAR(b);
    obj sym = make_symbol(make_string_utf8(s, len));
    PROTECT1(sym);
    obj cell = cons(sym, VEC_REF(obarray, h));
    VEC_REF(obarray, h) = cell;
    UNPROTECT(1);
    SYM(sym, SYM_PACKAGE) = lisp_package;
    if (len > 0 && s[0] == ':') {   /* keywords evaluate to themselves */
        SYM(sym, SYM_VALUE) = sym;
        SYM(sym, SYM_FLAGS) = MAKE_FIX(SYMF_CONSTANT);
        SYM(sym, SYM_PACKAGE) = keyword_package;
    }
    return sym;
}

obj intern_c(const char *s)
{
    int n = 0;
    while (s[n]) n++;
    return intern(s, n);
}

obj nreverse(obj list)
{
    obj prev = NIL;
    while (CONSP(list)) {
        obj next = CDR(list);
        CDR(list) = prev;
        prev = list;
        list = next;
    }
    return prev;
}

fixnum_t list_length(obj list)
{
    fixnum_t n = 0;
    obj slow = list;
    while (CONSP(list)) {
        n++;
        list = CDR(list);
        if (n % 2 == 0) {
            slow = CDR(slow);
            if (slow == list && CONSP(list)) return -1;   /* circular */
        }
    }
    return list == NIL ? n : -1;
}

void obj_init(void)
{
    static obj *const roots[] = {
        &obarray, &S_T, &S_QUOTE, &S_FUNCTION, &S_LAMBDA, &S_OPTIONAL, &S_REST, &S_BODY,
        &S_UNQUOTE, &S_UNQUOTE_SPLICING, &S_APPEND, &S_LIST, &S_PROGN, &S_LET, &S_LETSTAR, &S_ERROR,
    };
    for (unsigned i = 0; i < sizeof roots / sizeof roots[0]; i++) gc_global(roots[i]);

    obarray = make_vector(OBARRAY_SIZE, NIL);
    S_T = intern_c("T");
    SYM(S_T, SYM_VALUE) = S_T;
    SYM(S_T, SYM_FLAGS) = MAKE_FIX(SYMF_CONSTANT);
    S_QUOTE = intern_c("QUOTE");
    S_FUNCTION = intern_c("FUNCTION");
    S_LAMBDA = intern_c("LAMBDA");
    S_OPTIONAL = intern_c("&OPTIONAL");
    S_REST = intern_c("&REST");
    S_BODY = intern_c("&BODY");
    S_UNQUOTE = intern_c("UNQUOTE");
    S_UNQUOTE_SPLICING = intern_c("UNQUOTE-SPLICING");
    S_APPEND = intern_c("APPEND");
    S_LIST = intern_c("LIST");
    S_PROGN = intern_c("PROGN");
    S_LET = intern_c("LET");
    S_LETSTAR = intern_c("LET*");
    S_ERROR = intern_c("ERROR");
    packages_init();
}
