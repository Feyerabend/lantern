/* read.c — the reader.
 *
 * Reads from a C buffer (never from the heap), in UTF-8 (M15). Handles lists, dotted pairs,
 * integers, symbols (upcased), strings, characters (#\a #\Space), vectors #(...),
 * quote ', function #', backquote ` , ,@ and comments ; and #| |#.
 *
 * Backquote is expanded at read time into calls to APPEND and LIST. Nested
 * backquotes inside an unquoted expression work; doubly nested ones
 * (`(a `(b ,,c))) are not supported yet.
 *
 * Returns READ_INCOMPLETE when the buffer ends inside a form, so a listener
 * can keep collecting lines.
 */
#include "lisp.h"

#define MAX_DEPTH 1000
#define TOKEN_MAX 512
#define STRING_MAX 65536

typedef struct { const char *s; int len, pos, depth, suppress; } rd_t;

/* read_any may also return READ_SKIP: a #+ or #- form left nothing */
enum { READ_SKIP = 100 };
static int read_any(rd_t *r, obj *out);

/* the next object, past any #+/#- that leave nothing */
static int read_obj(rd_t *r, obj *out)
{
    int st;
    do st = read_any(r, out); while (st == READ_SKIP);
    return st;
}

static int peek(rd_t *r) { return r->pos < r->len ? (unsigned char)r->s[r->pos] : -1; }
static int peek_at(rd_t *r, int k) { return r->pos + k < r->len ? (unsigned char)r->s[r->pos + k] : -1; }

static int is_space(int c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'; }
static int is_delim(int c)
{
    return c < 0 || is_space(c) || c == '(' || c == ')' || c == '"' || c == '\'' ||
           c == ';' || c == '`' || c == ',';
}

static int rd_error(const char *msg)
{
    lisp_error(msg, UNBOUND);
    return READ_ERROR;
}

static int skip_ws(rd_t *r)
{
    for (;;) {
        int c = peek(r);
        if (is_space(c)) { r->pos++; continue; }
        if (c == ';') {
            while (peek(r) >= 0 && peek(r) != '\n') r->pos++;
            continue;
        }
        if (c == '#' && peek_at(r, 1) == '|') {
            int level = 1;
            r->pos += 2;
            while (level > 0) {
                if (peek(r) < 0) return READ_INCOMPLETE;
                if (peek(r) == '|' && peek_at(r, 1) == '#') { level--; r->pos += 2; }
                else if (peek(r) == '#' && peek_at(r, 1) == '|') { level++; r->pos += 2; }
                else r->pos++;
            }
            continue;
        }
        return READ_OK;
    }
}

/* ---- backquote --------------------------------------------------------- */

static obj qq(obj x)
{
    if (!CONSP(x)) {
        if (TAG(x) == TAG_SYMBOL || VECTORP(x)) return list2(S_QUOTE, x);
        return x;   /* NIL, numbers, strings, characters evaluate to themselves */
    }
    if (CAR(x) == S_UNQUOTE) return SCAR(CDR(x));
    if (CAR(x) == S_UNQUOTE_SPLICING) return lisp_error(",@ directly after `", UNBOUND);

    obj segs = NIL, rest = x, tmp = NIL;
    PROTECT3(segs, rest, tmp);
    while (CONSP(rest)) {
        if (CAR(rest) == S_UNQUOTE) {            /* dotted tail: `(a . ,b) */
            tmp = SCAR(CDR(rest));
            segs = cons(tmp, segs);
            rest = NIL;
            break;
        }
        obj item = CAR(rest);
        if (CONSP(item) && CAR(item) == S_UNQUOTE_SPLICING) {
            tmp = SCAR(CDR(item));
        } else {
            tmp = qq(item);
            tmp = list2(S_LIST, tmp);
        }
        segs = cons(tmp, segs);
        rest = CDR(rest);
    }
    if (rest != NIL) {                           /* dotted atom tail */
        tmp = qq(rest);
        segs = cons(tmp, segs);
    }
    segs = nreverse(segs);
    tmp = cons(S_APPEND, segs);
    UNPROTECT(3);
    return tmp;
}

/* ---- pieces -------------------------------------------------------------- */

static int read_list(rd_t *r, obj *out)
{
    obj head = NIL, tail = NIL, item = NIL;
    PROTECT3(head, tail, item);
    int st;
    for (;;) {
        if ((st = skip_ws(r))) break;
        int c = peek(r);
        if (c < 0) { st = READ_INCOMPLETE; break; }
        if (c == ')') { r->pos++; *out = head; st = READ_OK; break; }
        if (c == '.' && is_delim(peek_at(r, 1))) {
            if (tail == NIL) { st = rd_error("Nothing before . in a list"); break; }
            r->pos++;
            st = read_obj(r, &item);
            if (st == READ_EOF) st = READ_INCOMPLETE;
            if (st) break;
            CDR(tail) = item;
            if ((st = skip_ws(r))) break;
            if (peek(r) < 0) { st = READ_INCOMPLETE; break; }
            if (peek(r) != ')') { st = rd_error("More than one object after . in a list"); break; }
            r->pos++;
            *out = head;
            break;
        }
        st = read_any(r, &item);
        if (st == READ_SKIP) continue;
        if (st == READ_EOF) st = READ_INCOMPLETE;
        if (st) break;
        obj cell = cons(item, NIL);
        if (tail == NIL) head = cell;
        else CDR(tail) = cell;
        tail = cell;
    }
    UNPROTECT(3);
    return st;
}

static int read_wrapped(rd_t *r, obj *symp, obj *out)   /* *symp is read after reading: GC may move it */
{
    int st = read_obj(r, out);
    if (st == READ_EOF) return READ_INCOMPLETE;
    if (st) return st;
    *out = list2(*symp, *out);
    return READ_OK;
}

static int read_string(rd_t *r, obj *out)
{
    static char buf[STRING_MAX];
    int n = 0;
    for (;;) {
        int c = peek(r);
        if (c < 0) return READ_INCOMPLETE;
        r->pos++;
        if (c == '"') break;
        if (c == '\\') {
            c = peek(r);
            if (c < 0) return READ_INCOMPLETE;
            r->pos++;
        }
        if (n >= STRING_MAX) return rd_error("String too long");
        buf[n++] = (char)c;              /* the bytes of a character beyond ASCII follow as they are */
    }
    *out = make_string_utf8(buf, n);
    return READ_OK;
}

/* A token: its bytes in BUF and, if ESC is not NULL, which of them were
 * escaped, by \ or inside |...| (M16): escaped characters keep their case
 * and are never a package marker, and a token with escapes is never a
 * number. => its length, -1 if too long, -2 if the text ends inside an
 * escape; *ESCAPED says whether there was one. */
static int read_token_esc(rd_t *r, char *buf, char *esc, int *escaped)
{
    int n = 0, multi = 0;
    *escaped = 0;
    for (;;) {
        int c = peek(r);
        if (c < 0) { if (multi) return -2; break; }
        if (!multi && is_delim(c)) break;
        r->pos++;
        if (c == '|') { multi = !multi; *escaped = 1; continue; }
        int e = multi;
        if (c == '\\') {
            c = peek(r);
            if (c < 0) return -2;
            r->pos++;
            e = 1;
            *escaped = 1;
        }
        if (n >= TOKEN_MAX - 1) return -1;
        if (esc) esc[n] = (char)e;        /* a character beyond ASCII: its first byte says */
        buf[n++] = (char)c;
    }
    buf[n] = 0;
    return n;
}

static int read_token(rd_t *r, char *buf)
{
    int escaped;
    return read_token_esc(r, buf, 0, &escaped);
}

/* an integer from N decimal digits */
static obj decimal(const char *d, int n, int neg)
{
    if (n <= 18) {
        fixnum_t v = 0;
        for (int k = 0; k < n; k++) v = v * 10 + (d[k] - '0');
        return MAKE_FIX(neg ? -v : v);
    }
    return big_from_decimal(d, n, neg);
}

/* Is TOK a ratio, [+-]digits/digits? => 1, with the slash's index in *SLASH */
static int ratio_token(const char *tok, int n, int *slash)
{
    int i = (n > 0 && (tok[0] == '+' || tok[0] == '-')) ? 1 : 0, s = -1;
    for (int k = i; k < n; k++) {
        if (tok[k] == '/') { if (s >= 0) return 0; s = k; }
        else if (tok[k] < '0' || tok[k] > '9') return 0;
    }
    if (s <= i || s >= n - 1) return 0;
    *slash = s;
    return 1;
}

/* Would the reader read TOK as a number? (The printer escapes such symbol names.) */
int token_number_p(const char *tok, int n)
{
    int slash;
    if (ratio_token(tok, n, &slash)) return 1;
    double d;
    if (n == 0) return 0;
    if (flo_parse(tok, n, &d) != 0) return 1;
    int i = (tok[0] == '+' || tok[0] == '-') ? 1 : 0, m = n;
    if (i >= n) return 0;
    if (m > i + 1 && tok[m - 1] == '.') m--;
    for (int k = i; k < m; k++) if (tok[k] < '0' || tok[k] > '9') return 0;
    return 1;
}

/* What the reader leaves for Lisp to finish (M16): #S(...), #n= and #n#,
 * and #. are read as marker vectors #(%READ-... a b), and READ_FIXUPS is set;
 * world/reader.lisp turns them into the objects they stand for. */
int read_fixups;

static obj make_marker(const char *kind, obj a, obj b)
{
    PROTECT2(a, b);
    obj k = intern_c(kind);
    PROTECT1(k);
    obj v = make_vector(3, NIL);
    UNPROTECT(3);
    VEC_REF(v, 0) = k;
    VEC_REF(v, 1) = a;
    VEC_REF(v, 2) = b;
    read_fixups = 1;
    return v;
}

static char upcase(char c) { return c >= 'a' && c <= 'z' ? (char)(c - 32) : c; }

/* a token in upper case, in place: any letter with case, not only ASCII.
 * => its new length (the same for every mapping text.c knows) */
static int upcase_token_esc(char *tok, int n, const char *esc)
{
    char out[TOKEN_MAX + 8];
    int pos = 0, k = 0;
    while (pos < n && k < TOKEN_MAX - 4) {
        int start = pos, c = utf8_decode(tok, n, &pos);
        if (!esc[start]) c = char_upcase(c);
        k += utf8_encode(c, out + k);
    }
    memcpy(tok, out, (__SIZE_TYPE__)k);
    tok[k] = 0;
    return k;
}

static int upcase_token(char *tok, int n)
{
    if (utf8_ascii_p(tok, n)) {
        for (int k = 0; k < n; k++) tok[k] = upcase(tok[k]);
        return n;
    }
    char out[TOKEN_MAX + 8];
    int pos = 0, k = 0;
    while (pos < n && k < TOKEN_MAX - 4) k += utf8_encode(char_upcase(utf8_decode(tok, n, &pos)), out + k);
    memcpy(tok, out, (__SIZE_TYPE__)k);
    tok[k] = 0;
    return k;
}

static int hex_value(int c)
{
    return c >= '0' && c <= '9' ? c - '0' : c >= 'a' && c <= 'f' ? c - 'a' + 10 : c >= 'A' && c <= 'F' ? c - 'A' + 10 : -1;
}

static int token_eq(const char *tok, int n, const char *name)
{
    int i = 0;
    for (; i < n && name[i]; i++)
        if (upcase(tok[i]) != name[i]) return 0;
    return i == n && !name[i];
}

static int read_char(rd_t *r, obj *out)
{
    char tok[TOKEN_MAX];
    int c = peek(r);
    if (c < 0) return READ_INCOMPLETE;
    r->pos++;
    tok[0] = (char)c;
    int n = 1;
    while (!is_delim(peek(r)) && n < TOKEN_MAX - 1) tok[n++] = (char)r->s[r->pos++];
    if (n == 1) { *out = MAKE_CHAR(c); return READ_OK; }
    int pos = 0, code = utf8_decode(tok, n, &pos);
    if (pos == n && c >= 0x80) { *out = MAKE_CHAR(code); return READ_OK; }   /* one character beyond ASCII: #\é */
    static const struct { const char *name; int code; } names[] = {
        { "SPACE", ' ' }, { "NEWLINE", '\n' }, { "TAB", '\t' }, { "RETURN", '\r' },
        { "NULL", 0 }, { "NUL", 0 }, { "RUBOUT", 127 }, { "DELETE", 127 }, { "BACKSPACE", 8 },
        { "PAGE", 12 }, { "LINEFEED", 10 }, { "ESCAPE", 27 }, { "ALTMODE", 27 }, { "BELL", 7 },
    };
    for (unsigned i = 0; i < sizeof names / sizeof names[0]; i++)
        if (token_eq(tok, n, names[i].name)) { *out = MAKE_CHAR(names[i].code); return READ_OK; }
    /* by code: #\U+00E9 or #\U00E9 */
    if (upcase(tok[0]) == 'U') {
        int k = tok[1] == '+' ? 2 : 1, v = 0;
        if (k < n && n - k <= 6) {
            for (; k < n && hex_value(tok[k]) >= 0; k++) v = v * 16 + hex_value(tok[k]);
            if (k == n && v < 0x110000) { *out = MAKE_CHAR(v); return READ_OK; }
        }
    }
    return rd_error("Unknown character name");
}

/* A symbol, in *PACKAGE*: NAME, PKG:NAME (an external symbol of PKG),
 * PKG::NAME (any symbol of PKG, made if need be), :NAME (a keyword). */
static int read_symbol(char *tok, int n, const char *esc, obj *out)
{
#define MARKER(k) (tok[k] == ':' && !(esc && esc[k]))
    int colon = -1;
    for (int k = 1; k < n; k++) if (MARKER(k)) { colon = k; break; }
    if ((n > 0 && MARKER(0)) || colon < 0) {
        *out = n > 0 && MARKER(0) ? intern(tok, n) : pkg_intern(current_package(), tok, n);
        return lisp_err ? READ_ERROR : READ_OK;
    }
    obj pkg = find_package(tok, colon);
    if (pkg == NIL) {
        lisp_error("No package named", make_string_utf8(tok, colon));
        return READ_ERROR;
    }
    int internal = colon + 1 < n && MARKER(colon + 1);
    char *name = tok + colon + 1 + internal;
    int len = n - colon - 1 - internal;
    if (len <= 0) return rd_error("A package prefix with no symbol name");
    for (int k = colon + 1 + internal; k < n; k++) if (MARKER(k)) return rd_error("Too many colons in a symbol");
#undef MARKER
    if (internal || pkg == keyword_package) {
        *out = pkg_intern(pkg, name, len);
        return lisp_err ? READ_ERROR : READ_OK;
    }
    int status;
    obj sym = pkg_find_symbol(pkg, name, len, &status);
    if (sym == UNBOUND || status != PS_EXTERNAL) {
        obj s = make_string_utf8(tok, n);
        lisp_error(sym == UNBOUND ? "No such external symbol (use :: for an internal one):"
                                  : "The symbol is not external in its package (use ::):", s);
        return READ_ERROR;
    }
    *out = sym;
    return READ_OK;
}

static int read_atom(rd_t *r, obj *out)
{
    char tok[TOKEN_MAX], esc[TOKEN_MAX];
    int escaped;
    int n = read_token_esc(r, tok, esc, &escaped);
    if (n == -2) return READ_INCOMPLETE;
    if (n < 0) return rd_error("Token too long");
    if (n == 0 && !escaped) return rd_error("Unexpected character");
    if (r->suppress) { *out = NIL; return READ_OK; }   /* inside a #+ or #- left out */
    if (escaped) {                                     /* |Name|, a\ b: a symbol, as written */
        n = upcase_token_esc(tok, n, esc);
        return read_symbol(tok, n, esc, out);
    }

    int i = 0, neg = 0;
    double d;
    int f = flo_parse(tok, n, &d);
    if (f < 0) return rd_error("Float too large");
    if (f > 0) { *out = make_float(d); return READ_OK; }
    if (tok[0] == '+' || tok[0] == '-') { neg = tok[0] == '-'; i = 1; }
    if (i < n) {
        int digits = 1, m = n;
        if (m > i + 1 && tok[m - 1] == '.') m--;        /* 12. is the integer 12 */
        for (int k = i; k < m; k++) if (tok[k] < '0' || tok[k] > '9') { digits = 0; break; }
        if (digits) {
            n = m;
            if (n - i <= 18) {                     /* fits a fixnum for sure */
                fixnum_t v = 0;
                for (int k = i; k < n; k++) v = v * 10 + (tok[k] - '0');
                *out = MAKE_FIX(neg ? -v : v);
                return READ_OK;
            }
            *out = big_from_decimal(tok + i, n - i, neg);   /* a fixnum again if it fits */
            return lisp_err ? rd_error("Integer too large") : READ_OK;
        }
    }
    if (ratio_token(tok, n, &i)) {                         /* 7/2, -1/3 (M18) */
        obj num = NIL, den = NIL;
        PROTECT2(num, den);
        num = decimal(tok + (tok[0] == '+' || tok[0] == '-'), i - (tok[0] == '+' || tok[0] == '-'), tok[0] == '-');
        if (!lisp_err) den = decimal(tok + i + 1, n - i - 1, 0);
        if (!lisp_err) *out = make_ratio(num, den);
        UNPROTECT(2);
        return lisp_err ? READ_ERROR : READ_OK;
    }
    if (n == 1 && tok[0] == '.') return rd_error("Dot context error");
    n = upcase_token(tok, n);
    return read_symbol(tok, n, 0, out);
}

static obj list_to_vector(obj list)
{
    fixnum_t n = list_length(list);
    PROTECT1(list);
    obj v = make_vector((int)n, NIL);
    UNPROTECT(1);
    for (int i = 0; i < n; i++, list = CDR(list)) VEC_REF(v, i) = CAR(list);
    return v;
}

/* Is feature expression X true? A keyword is true if it is on *FEATURES*;
 * (:and ...), (:or ...) and (:not x) combine them. */
static int feature_true(obj x)
{
    if (TAG(x) == TAG_SYMBOL) {
        for (obj l = SYM(intern_c("*FEATURES*"), SYM_VALUE); CONSP(l); l = CDR(l))
            if (CAR(l) == x) return 1;
        return 0;
    }
    if (!CONSP(x)) return 0;
    obj op = CAR(x);
    if (op == intern_c(":NOT")) return !feature_true(SCAR(CDR(x)));
    int and = op == intern_c(":AND");
    if (!and && op != intern_c(":OR")) return 0;
    for (obj l = CDR(x); CONSP(l); l = CDR(l))
        if (feature_true(CAR(l)) != and) return !and;
    return and;
}

/* #+feature form, #-feature form: the form, or nothing (READ_SKIP). The
 * feature is read in the KEYWORD package; a form left out is read without
 * interning anything, so it may name packages that do not exist here. */
static int read_conditional(rd_t *r, int plus, obj *out)
{
    obj saved = SYM(S_STAR_PACKAGE, SYM_VALUE);
    SYM(S_STAR_PACKAGE, SYM_VALUE) = keyword_package;
    int st = read_obj(r, out);
    SYM(S_STAR_PACKAGE, SYM_VALUE) = saved;
    if (st == READ_EOF) return READ_INCOMPLETE;
    if (st) return st;
    int keep = feature_true(*out) == plus;
    if (keep) {
        st = read_obj(r, out);
        return st == READ_EOF ? READ_INCOMPLETE : st;
    }
    int was = r->suppress;
    r->suppress = 1;
    st = read_obj(r, out);
    r->suppress = was;
    if (st == READ_EOF) return READ_INCOMPLETE;
    if (st) return st;
    *out = NIL;
    return READ_SKIP;
}

static int read_any(rd_t *r, obj *out)
{
    int st = skip_ws(r);
    if (st) return st;
    int c = peek(r);
    if (c < 0) return READ_EOF;
    if (r->depth >= MAX_DEPTH) return rd_error("Expression nested too deeply");
    r->depth++;

    switch (c) {
    case '(':
        r->pos++;
        st = read_list(r, out);
        break;
    case ')':
        r->pos++;
        st = rd_error("Unexpected )");
        break;
    case '\'':
        r->pos++;
        st = read_wrapped(r, &S_QUOTE, out);
        break;
    case '`':
        r->pos++;
        st = read_obj(r, out);
        if (st == READ_EOF) st = READ_INCOMPLETE;
        if (st == READ_OK) {
            *out = qq(*out);
            if (lisp_err) st = READ_ERROR;
        }
        break;
    case ',':
        r->pos++;
        if (peek(r) == '@') {
            r->pos++;
            st = read_wrapped(r, &S_UNQUOTE_SPLICING, out);
        } else {
            st = read_wrapped(r, &S_UNQUOTE, out);
        }
        break;
    case '"':
        r->pos++;
        st = read_string(r, out);
        break;
    case '#':
        r->pos++;
        c = peek(r);
        if (c == '\'') { r->pos++; st = read_wrapped(r, &S_FUNCTION, out); }
        else if (c == '\\') { r->pos++; st = read_char(r, out); }
        else if (c == '(') {
            r->pos++;
            st = read_list(r, out);
            if (st == READ_OK) {
                if (list_length(*out) < 0) st = rd_error("Dotted list in #( )");
                else *out = list_to_vector(*out);
            }
        }
        else if (c == '+' || c == '-') { r->pos++; st = read_conditional(r, c == '+', out); }
        else if (c >= '0' && c <= '9') {              /* #2A((1 2) (3 4)), #1=..., #1# */
            int rank = 0;
            while (peek(r) >= '0' && peek(r) <= '9' && rank < 1000000) rank = rank * 10 + (r->s[r->pos++] - '0');
            if (peek(r) == '=') {
                r->pos++;
                st = read_obj(r, out);
                if (st == READ_EOF) st = READ_INCOMPLETE;
                if (st == READ_OK && !r->suppress) *out = make_marker("%READ-LABEL", MAKE_FIX(rank), *out);
            }
            else if (peek(r) == '#') {
                r->pos++;
                *out = r->suppress ? NIL : make_marker("%READ-REF", MAKE_FIX(rank), NIL);
                st = READ_OK;
            }
            else if (peek(r) < 0) st = READ_INCOMPLETE;
            else if (peek(r) != 'A' && peek(r) != 'a') st = rd_error("Unknown # syntax after a number (#nA, #n= or #n#)");
            else {
                r->pos++;
                st = read_obj(r, out);
                if (st == READ_EOF) st = READ_INCOMPLETE;
                if (st == READ_OK && !r->suppress) {
                    *out = array_from_contents(rank, *out);
                    if (lisp_err) st = READ_ERROR;
                }
            }
        }
        else if (c == '*') {                       /* #*1011, a bit vector (M18) */
            r->pos++;
            char tok[TOKEN_MAX];
            int n = read_token(r, tok);
            if (n < 0) st = rd_error("Bit vector too long");
            else {
                for (int k = 0; k < n; k++) {
                    if (tok[k] != '0' && tok[k] != '1') { st = rd_error("Not a bit in #*"); break; }
                    tok[k] = (char)(tok[k] - '0');
                }
                if (st == READ_OK && !r->suppress) *out = make_bit_vector(tok, n);
            }
        }
        else if (c == 'P' || c == 'p') {           /* #P"dir/name.type", a pathname (M18) */
            r->pos++;
            st = read_obj(r, out);
            if (st == READ_EOF) st = READ_INCOMPLETE;
            if (st == READ_OK && !r->suppress) {
                if (STRINGP(*out)) *out = make_marker("%READ-PATHNAME", *out, NIL);
                else st = rd_error("#P needs a string");
            }
        }
        else if (c == 'S' || c == 's' || c == '.') {   /* #S(name slot value ...), #.form */
            r->pos++;
            st = read_obj(r, out);
            if (st == READ_EOF) st = READ_INCOMPLETE;
            if (st == READ_OK && !r->suppress) {
                if (c == '.') *out = make_marker("%READ-EVAL", *out, NIL);
                else if (CONSP(*out) && TAG(CAR(*out)) == TAG_SYMBOL) *out = make_marker("%READ-STRUCT", *out, NIL);
                else st = rd_error("#S needs a list that starts with the structure's name");
            }
        }
        else if (c == ':') {                       /* #:name, an uninterned symbol */
            r->pos++;
            char tok[TOKEN_MAX], esc[TOKEN_MAX];
            int escaped;
            int n = read_token_esc(r, tok, esc, &escaped);
            if (n == -2) st = READ_INCOMPLETE;
            else if (n < 0 || (n == 0 && !escaped)) st = rd_error("Bad #: syntax");
            else {
                n = upcase_token_esc(tok, n, esc);
                *out = make_symbol(make_string_utf8(tok, n));
                st = READ_OK;
            }
        }
        else if (c == 'x' || c == 'X' || c == 'b' || c == 'B' || c == 'o' || c == 'O') {
            r->pos++;                              /* #xFF #b101 #o17 */
            int radix = (c == 'x' || c == 'X') ? 16 : (c == 'b' || c == 'B') ? 2 : 8;
            char tok[TOKEN_MAX];
            int n = read_token(r, tok), i = 0, neg = 0;
            if (n > 0 && (tok[0] == '-' || tok[0] == '+')) { neg = tok[0] == '-'; i = 1; }
            obj v = MAKE_FIX(0);
            st = n > i ? READ_OK : rd_error("Bad number after #x, #b or #o");
            PROTECT1(v);
            for (int k = i; k < n && st == READ_OK; k++) {
                int ch = upcase(tok[k]);
                int d = ch >= '0' && ch <= '9' ? ch - '0' : ch >= 'A' && ch <= 'Z' ? ch - 'A' + 10 : 99;
                if (d >= radix) { st = rd_error("Bad digit in #x, #b or #o"); break; }
                v = num_add(num_mul(v, MAKE_FIX(radix)), MAKE_FIX(d));
            }
            if (st == READ_OK && neg) v = num_sub(MAKE_FIX(0), v);
            UNPROTECT(1);
            if (st == READ_OK) *out = v;
        }
        else if (c < 0) st = READ_INCOMPLETE;
        else st = rd_error("Unknown # syntax");
        break;
    default:
        st = read_atom(r, out);
    }
    r->depth--;
    return st;
}

int lisp_read(const char *s, int len, int *pos, obj *out)
{
    rd_t r = { s, len, *pos, 0, 0 };
    read_fixups = 0;
    int saved_roots = gc_nroots;
    int st = read_obj(&r, out);
    gc_nroots = saved_roots;
    if (st == READ_OK) *pos = r.pos;
    if (st == READ_EOF) *pos = len;
    return st;
}
