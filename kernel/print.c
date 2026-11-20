/* print.c — the printer and the standard output stream.
 *
 * The printer never allocates, so it is safe to call at any time.
 */
#include "lisp.h"

#define PRINT_DEPTH_MAX 200
#define PRINT_LENGTH_MAX 100000

/* ---- standard output: forwards to the host and tracks the column ------- */

static void (*host_output)(const char *p, int n);
static int at_line_start = 1;

static void stdout_write(sink_t *s, const char *p, int n)
{
    (void)s;
    if (n <= 0) return;
    if (host_output) host_output(p, n);
    at_line_start = p[n - 1] == '\n';
}

static sink_t stdout_sink = { stdout_write };
sink_t *lisp_stdout = &stdout_sink;

void lisp_set_output(void (*fn)(const char *p, int n)) { host_output = fn; }
void lisp_write(const char *p, int n) { stdout_write(&stdout_sink, p, n); }
void lisp_puts(const char *s) { int n = 0; while (s[n]) n++; lisp_write(s, n); }
void lisp_fresh_line(void) { if (!at_line_start) lisp_write("\n", 1); }

/* ---- printer -------------------------------------------------------------- */

/* The printer writes UTF-8 (M15). */
static void put(sink_t *o, const char *s, int n) { o->write(o, s, n); }

static void put_code(sink_t *o, int c)
{
    char e[4];
    put(o, e, utf8_encode(c, e));
}

/* a string's characters from START to END, with \ before " and \ if ESCAPE */
static void put_chars(sink_t *o, obj s, int start, int end, int escape)
{
    if (BSTRINGP(s) && !escape && utf8_ascii_p(STR_DATA(s) + start, end - start)) {
        put(o, STR_DATA(s) + start, end - start);
        return;
    }
    char buf[256];
    int k = 0;
    for (int i = start; i < end; i++) {
        int c = str_ref(s, i);
        if (k > (int)sizeof buf - 8) { put(o, buf, k); k = 0; }
        if (escape && (c == '"' || c == '\\')) buf[k++] = '\\';
        k += utf8_encode(c, buf + k);
    }
    put(o, buf, k);
}

static void put_str(sink_t *o, obj s) { put_chars(o, s, 0, STR_LEN(s), 0); }
static void puts_(sink_t *o, const char *s) { int n = 0; while (s[n]) n++; put(o, s, n); }

static void put_int(sink_t *o, fixnum_t v)
{
    char buf[24];
    int n = sizeof buf;
    uint64_t u = v < 0 ? -(uint64_t)v : (uint64_t)v;
    do { buf[--n] = (char)('0' + u % 10); u /= 10; } while (u);
    if (v < 0) buf[--n] = '-';
    put(o, buf + n, (int)sizeof buf - n);
}

static void put_symbol_name(sink_t *o, obj sym)
{
    if (sym == NIL) { puts_(o, "NIL"); return; }
    put_str(o, SYM(sym, SYM_NAME));
}

/* Would NAME, from character START on, read back as itself? Not if it has
 * a character the reader would upcase or that ends or escapes a token, a
 * colon, or looks like a number. (M16) */
static int name_needs_bars(obj name, int start)
{
    int n = STR_LEN(name);
    if (n == start) return 1;
    int dots = 1;
    for (int i = start; i < n; i++) {
        int c = str_ref(name, i);
        if (c <= ' ' || c == '(' || c == ')' || c == '"' || c == '\'' || c == ';' || c == '`' ||
            c == ',' || c == '|' || c == '\\' || c == ':' || char_upcase(c) != c || c == 127)
            return 1;
        if (c != '.') dots = 0;
    }
    if (dots) return 1;
    char buf[64];
    int len = n - start;
    if (len < (int)sizeof buf && BSTRINGP(name)) {
        memcpy(buf, STR_DATA(name) + start, (__SIZE_TYPE__)len);
        if (token_number_p(buf, len)) return 1;
    }
    return 0;
}

/* NAME from START, as |...| when it must be */
static void put_name_escaped(sink_t *o, obj name, int start)
{
    if (!name_needs_bars(name, start)) { put_chars(o, name, start, STR_LEN(name), 0); return; }
    put(o, "|", 1);
    for (int i = start; i < STR_LEN(name); i++) {
        int c = str_ref(name, i);
        if (c == '|' || c == '\\') put(o, "\\", 1);
        put_code(o, c);
    }
    put(o, "|", 1);
}

/* with a package prefix when *PACKAGE* does not see it (M12) */
static void put_symbol(sink_t *o, obj sym, int esc)
{
    if (esc && sym != NIL) {
        obj home = SYM(sym, SYM_PACKAGE);
        if (home == NIL) puts_(o, "#:");
        else if (home != keyword_package && !symbol_accessible(sym)) {
            put_str(o, VEC_REF(home, PK_NAME));
            puts_(o, symbol_external(sym) ? ":" : "::");
        }
        obj name = SYM(sym, SYM_NAME);
        int kw = home == keyword_package && STR_LEN(name) > 0 && str_ref(name, 0) == ':';
        if (kw) put(o, ":", 1);
        put_name_escaped(o, name, kw);
        return;
    }
    if (sym != NIL && SYM(sym, SYM_PACKAGE) == keyword_package) {   /* PRINC :FOO writes FOO (M17) */
        obj name = SYM(sym, SYM_NAME);
        int kw = STR_LEN(name) > 0 && str_ref(name, 0) == ':';
        put_chars(o, name, kw, STR_LEN(name), 0);
        return;
    }
    put_symbol_name(o, sym);
}

static void print_char(sink_t *o, int c, int escape)
{
    if (!escape) { put_code(o, c); return; }
    puts_(o, "#\\");
    switch (c) {
    case ' ':  puts_(o, "Space"); return;
    case '\n': puts_(o, "Newline"); return;
    case '\t': puts_(o, "Tab"); return;
    case '\r': puts_(o, "Return"); return;
    case 0:    puts_(o, "Null"); return;
    case 127:  puts_(o, "Rubout"); return;
    case 8:    puts_(o, "Backspace"); return;
    case 12:   puts_(o, "Page"); return;
    }
    if (!char_graphic_p(c)) {                 /* other control characters: #\U+0085 */
        static const char hex[] = "0123456789ABCDEF";
        char buf[10];
        int n = 0;
        buf[n++] = 'U'; buf[n++] = '+';
        for (int shift = c > 0xFFFF ? 20 : 12; shift >= 0; shift -= 4) buf[n++] = hex[(c >> shift) & 15];
        put(o, buf, n);
        return;
    }
    put_code(o, c);
}

static void print_string(sink_t *o, obj s, int escape)
{
    if (escape) put(o, "\"", 1);
    put_chars(o, s, 0, STR_LEN(s), escape);
    if (escape) put(o, "\"", 1);
}

static void print_rec(obj x, int esc, sink_t *o, int depth);

static int named(obj sym, const char *name)
{
    if (TAG(sym) != TAG_SYMBOL) return 0;
    int n = 0;
    while (name[n]) n++;
    return str_eq_utf8(SYM(sym, SYM_NAME), name, n);
}

static void put_name_field(sink_t *o, obj x)
{
    if (STRINGP(x)) put_str(o, x);
    else if (TAG(x) == TAG_SYMBOL) put_symbol_name(o, x);
    else puts_(o, "?");
}

/* Lisp-level objects are vectors whose first element says what they are
 * (instances, classes, bitmaps, fonts, streams, processes...). Print them as
 * #<...> rather than dumping their insides. Returns 0 for ordinary vectors. */
static int print_tagged_vector(obj x, sink_t *o)
{
    int n = VEC_LEN(x);
    if (n < 2 || TAG(VEC_REF(x, 0)) != TAG_SYMBOL) return 0;
    obj k = VEC_REF(x, 0);
    if (named(k, ":INSTANCE") && VECTORP(VEC_REF(x, 1)) && VEC_LEN(VEC_REF(x, 1)) > 1) {
        obj name = VEC_REF(VEC_REF(x, 1), 1);
        puts_(o, "#<");
        if (TAG(name) == TAG_SYMBOL) put_symbol(o, name, 1);   /* with its package prefix (M17) */
        else put_name_field(o, name);
        put(o, ">", 1);
        return 1;
    }
    static const struct { const char *tag, *label; } kinds[] = {
        { ":CLASS", "Class " }, { ":FONT", "Font " }, { ":PROCESS", "Process " },
        { ":GENERIC", "Generic-function " }, { ":LOCK", "Lock " }, { ":METHOD", "Method " },
        { ":HASH-TABLE", "Hash-table " }, { ":PACKAGE", "Package " },
    };
    for (unsigned i = 0; i < sizeof kinds / sizeof kinds[0]; i++)
        if (named(k, kinds[i].tag)) {
            puts_(o, "#<");
            puts_(o, kinds[i].label);
            put_name_field(o, VEC_REF(x, 1));
            put(o, ">", 1);
            return 1;
        }
    if (named(k, ":BITMAP") && n >= 3) {
        puts_(o, "#<Bitmap ");
        print_rec(VEC_REF(x, 1), 0, o, 1);
        put(o, "x", 1);
        print_rec(VEC_REF(x, 2), 0, o, 1);
        put(o, ">", 1);
        return 1;
    }
    if (named(k, ":STREAM")) { puts_(o, "#<Stream>"); return 1; }
    return 0;
}

/* arrays (M13): a one-dimensional one as its active elements, #( ) or a
 * string; others as #nA with nested lists, as Common Lisp writes them */
static obj arr_being_printed;        /* the array print_array_level prints */

static void print_array_level(obj data, int *index, obj dims, int esc, sink_t *o, int depth)
{
    if (!CONSP(dims)) {
        obj d = data;
        int i = (*index)++;
        if (bit_array_p(arr_being_printed)) print_rec(MAKE_FIX(STR_DATA(d)[i]), esc, o, depth + 1);
        else if (STRINGP(d)) print_char(o, str_ref(d, i), esc);
        else print_rec(VEC_REF(d, i), esc, o, depth + 1);
        return;
    }
    put(o, "(", 1);
    fixnum_t n = FIX(CAR(dims));
    for (fixnum_t k = 0; k < n; k++) {
        if (k) put(o, " ", 1);
        print_array_level(data, index, CDR(dims), esc, o, depth + 1);
    }
    put(o, ")", 1);
}

static void print_array(obj x, int esc, sink_t *o, int depth)
{
    obj data = FIELD(x, AR_DATA), dims = FIELD(x, AR_DIMS);
    int off = (int)FIX(FIELD(x, AR_OFFSET));
    int rank = array_rank(x);
    if (rank == 1) {
        int n = array_length(x);
        if (bit_array_p(x)) {                     /* #*1011 (M18) */
            puts_(o, "#*");
            for (int i = 0; i < n; i++) put(o, STR_DATA(FIELD(x, AR_DATA))[off + i] ? "1" : "0", 1);
            return;
        }
        if (STRINGP(data)) {
            if (esc) put(o, "\"", 1);
            put_chars(o, data, off, off + n, esc);
            if (esc) put(o, "\"", 1);
            return;
        }
        puts_(o, "#(");
        for (int i = 0; i < n; i++) {
            if (i) put(o, " ", 1);
            print_rec(VEC_REF(data, off + i), esc, o, depth + 1);
        }
        put(o, ")", 1);
        return;
    }
    put(o, "#", 1);
    put_int(o, rank);
    put(o, "A", 1);
    int index = off;
    if (rank == 0) put(o, " ", 1);
    obj outer = arr_being_printed;          /* arrays inside arrays */
    arr_being_printed = x;
    print_array_level(data, &index, dims, esc, o, depth);
    arr_being_printed = outer;
}

static void print_list(obj x, int esc, sink_t *o, int depth)
{
    obj head = CAR(x), rest = CDR(x);
    if (CONSP(rest) && CDR(rest) == NIL) {
        if (head == S_QUOTE)    { put(o, "'", 1);  print_rec(CAR(rest), esc, o, depth + 1); return; }
        if (head == S_FUNCTION) { put(o, "#'", 2); print_rec(CAR(rest), esc, o, depth + 1); return; }
    }
    put(o, "(", 1);
    long n = 0;
    for (;;) {
        print_rec(CAR(x), esc, o, depth + 1);
        x = CDR(x);
        if (x == NIL) break;
        if (!CONSP(x)) { puts_(o, " . "); print_rec(x, esc, o, depth + 1); break; }
        if (++n >= PRINT_LENGTH_MAX) { puts_(o, " ..."); break; }
        put(o, " ", 1);
    }
    put(o, ")", 1);
}

static void print_rec(obj x, int esc, sink_t *o, int depth)
{
    if (depth > PRINT_DEPTH_MAX) { puts_(o, "..."); return; }
    switch (TAG(x)) {
    case TAG_FIXNUM:
        put_int(o, FIX(x));
        return;
    case TAG_IMM:
        if (x == NIL) puts_(o, "NIL");
        else if (CHARP(x)) print_char(o, CHAR_CODE(x), esc);
        else if (x == UNBOUND) puts_(o, "#<unbound>");
        else puts_(o, "#<immediate>");
        return;
    case TAG_SYMBOL:
        put_symbol(o, x, esc);
        return;
    case TAG_CONS:
        print_list(x, esc, o, depth);
        return;
    case TAG_OBJECT:
        switch (OBJ_TYPE(x)) {
        case HT_STRING:
        case HT_WSTRING:
            print_string(o, x, esc);
            return;
        case HT_BIGNUM: {
            static char digits[90000];
            put(o, digits, big_to_decimal(x, digits, (int)sizeof digits));
            return;
        }
        case HT_RATIO:                                    /* 7/2 (M18) */
            print_rec(FIELD(x, RAT_NUM), esc, o, depth + 1);
            put(o, "/", 1);
            print_rec(FIELD(x, RAT_DEN), esc, o, depth + 1);
            return;
        case HT_FLOAT: {
            char buf[64];
            put(o, buf, flo_to_string(float_value(x), buf, (int)sizeof buf));
            return;
        }
        case HT_ARRAY:
            print_array(x, esc, o, depth);
            return;
        case HT_VECTOR:
            if (print_tagged_vector(x, o)) return;
            puts_(o, "#(");
            for (int i = 0; i < VEC_LEN(x); i++) {
                if (i) put(o, " ", 1);
                print_rec(VEC_REF(x, i), esc, o, depth + 1);
            }
            put(o, ")", 1);
            return;
        case HT_CLOSURE:
        case HT_MACRO:
            puts_(o, OBJ_TYPE(x) == HT_MACRO ? "#<Macro " : "#<Function ");
            if (FIELD(x, FN_NAME) != NIL) put_symbol_name(o, FIELD(x, FN_NAME));
            else puts_(o, "(lambda)");
            put(o, ">", 1);
            return;
        case HT_PRIMITIVE:
            puts_(o, "#<Primitive ");
            puts_(o, prims[FIX(FIELD(x, 0))].name);
            put(o, ">", 1);
            return;
        case HT_CFUNC:
            puts_(o, MACROP(x) ? "#<Compiled-macro " : "#<Compiled-function ");
            if (FIELD(x, CF_NAME) == NIL) puts_(o, "(lambda)");
            else if (SYMBOLP(FIELD(x, CF_NAME))) put_symbol_name(o, FIELD(x, CF_NAME));
            else print_rec(FIELD(x, CF_NAME), 1, o, depth + 1);    /* (METHOD name qualifier class) */
            put(o, ">", 1);
            return;
        case HT_CONDITION:
            puts_(o, "#<");
            print_rec(FIELD(x, COND_TYPE), 1, o, depth + 1);
            put(o, " ", 1);
            print_rec(FIELD(x, COND_MESSAGE), 1, o, depth + 1);
            for (obj i = FIELD(x, COND_IRRITANTS); CONSP(i); i = CDR(i)) {
                put(o, " ", 1);
                print_rec(CAR(i), 1, o, depth + 1);
            }
            put(o, ">", 1);
            return;
        }
        break;
    }
    puts_(o, "#<?>");
}

void print_obj(obj x, int escape, sink_t *out)
{
    print_rec(x, escape, out, 0);
}
