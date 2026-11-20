/* text.c — characters beyond ASCII (M15).
 *
 * A character is a Unicode code point, 0 to #x10FFFF. A string holds any of
 * them, in one of two forms:
 *
 *   HT_STRING   one byte per character: every character is below 256
 *               (Latin-1). The common case, and also what bitmaps and
 *               bytecode are made of.
 *   HT_WSTRING  four bytes per character, for strings with anything else.
 *
 * Functions that make a string from characters choose the narrow form when
 * the characters allow it, so the two forms of the same text compare EQUAL
 * and hash alike. Storing a character of 256 or more into a narrow string
 * is an error: such a string must be made wide to begin with (MAKE-STRING
 * does that by default).
 *
 * Everything outside the heap is UTF-8: C strings in the kernel, the reader's
 * buffers, what the printer writes, and the text that crosses to the host
 * (the terminal, files). Bytes that are not valid UTF-8 read as the Latin-1
 * characters they would be, so nothing is lost.
 *
 * Case and the letter tests know ASCII, Latin-1, Latin Extended-A, Greek
 * and Cyrillic (simple one-to-one mappings); other scripts are letters
 * without case where listed below.
 */
#include "lisp.h"

/* ---- UTF-8 ------------------------------------------------------------------- */

int utf8_decode(const char *s, int len, int *pos)
{
    const unsigned char *p = (const unsigned char *)s;
    int i = *pos, c = p[i];
    if (c < 0x80) { *pos = i + 1; return c; }
    int need = c >= 0xF0 && c < 0xF5 ? 3 : c >= 0xE0 ? 2 : c >= 0xC2 && c < 0xE0 ? 1 : -1;
    if (c >= 0xF5) need = -1;
    if (need > 0 && i + need < len + 0) {
        int v = c & (0x3F >> need), k;
        for (k = 1; k <= need; k++) {
            if ((p[i + k] & 0xC0) != 0x80) break;
            v = (v << 6) | (p[i + k] & 0x3F);
        }
        if (k > need) {
            int min = need == 1 ? 0x80 : need == 2 ? 0x800 : 0x10000;
            if (v >= min && v < 0x110000 && !(v >= 0xD800 && v < 0xE000)) {
                *pos = i + need + 1;
                return v;
            }
        }
    }
    *pos = i + 1;            /* not UTF-8: the byte as a Latin-1 character */
    return c;
}

int utf8_encode(int c, char *out)
{
    if (c < 0x80) { out[0] = (char)c; return 1; }
    if (c < 0x800) { out[0] = (char)(0xC0 | (c >> 6)); out[1] = (char)(0x80 | (c & 0x3F)); return 2; }
    if (c < 0x10000) {
        out[0] = (char)(0xE0 | (c >> 12)); out[1] = (char)(0x80 | ((c >> 6) & 0x3F));
        out[2] = (char)(0x80 | (c & 0x3F)); return 3;
    }
    out[0] = (char)(0xF0 | (c >> 18)); out[1] = (char)(0x80 | ((c >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((c >> 6) & 0x3F)); out[3] = (char)(0x80 | (c & 0x3F));
    return 4;
}

int utf8_ascii_p(const char *s, int len)
{
    for (int i = 0; i < len; i++) if ((unsigned char)s[i] >= 0x80) return 0;
    return 1;
}

/* the number of characters in S[0..LEN) */
int utf8_count(const char *s, int len)
{
    int n = 0, pos = 0;
    while (pos < len) { utf8_decode(s, len, &pos); n++; }
    return n;
}

/* ---- strings ----------------------------------------------------------------- */

obj make_wide_string(int n)
{
    obj x = alloc_object(HT_WSTRING, (uint64_t)n * 4, TAG_OBJECT);
    memset(WSTR_DATA(x), 0, (__SIZE_TYPE__)n * 4);
    return x;
}

obj make_string_of(int n, int wide)
{
    if (wide) return make_wide_string(n);
    obj x = alloc_object(HT_STRING, (uint64_t)n, TAG_OBJECT);
    memset(STR_DATA(x), 0, (__SIZE_TYPE__)n);
    return x;
}

obj make_string_utf8(const char *s, int len)
{
    if (utf8_ascii_p(s, len)) return make_string(s, len);
    int n = 0, wide = 0, pos = 0;
    while (pos < len) { if (utf8_decode(s, len, &pos) >= 256) wide = 1; n++; }
    obj x = make_string_of(n, wide);
    pos = 0;
    for (int i = 0; i < n; i++) str_set(x, i, utf8_decode(s, len, &pos));
    return x;
}

obj make_string_codes(const int *codes, int n)
{
    int wide = 0;
    for (int i = 0; i < n; i++) if (codes[i] >= 256) wide = 1;
    obj x = make_string_of(n, wide);
    for (int i = 0; i < n; i++) str_set(x, i, codes[i]);
    return x;
}

int str_wide_p(obj s, int start, int end)
{
    if (!WSTRINGP(s)) return 0;
    const uint32_t *w = WSTR_DATA(s);
    for (int i = start; i < end; i++) if (w[i] >= 256) return 1;
    return 0;
}

/* S as UTF-8 into BUF (at most CAP bytes). => the length it needs in all */
int str_utf8(obj s, char *buf, int cap)
{
    int n = STR_LEN(s), k = 0;
    if (BSTRINGP(s)) {
        const unsigned char *p = (const unsigned char *)STR_DATA(s);
        if (utf8_ascii_p((const char *)p, n)) {
            memcpy(buf, p, (__SIZE_TYPE__)(n < cap ? n : cap));
            return n;
        }
    }
    for (int i = 0; i < n; i++) {
        char e[4];
        int m = utf8_encode(str_ref(s, i), e);
        for (int j = 0; j < m; j++, k++) if (k < cap) buf[k] = e[j];
    }
    return k;
}

int str_utf8_length(obj s)
{
    int n = STR_LEN(s), k = 0;
    for (int i = 0; i < n; i++) {
        int c = str_ref(s, i);
        k += c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
    }
    return k;
}

/* the same as UTF-8 text? */
int str_eq_utf8(obj s, const char *p, int len)
{
    if (!STRINGP(s)) return 0;
    int n = STR_LEN(s);
    if (BSTRINGP(s) && n == len && k_memeq(STR_DATA(s), p, len) && utf8_ascii_p(p, len)) return 1;
    int pos = 0;
    for (int i = 0; i < n; i++) {
        if (pos >= len) return 0;
        if (utf8_decode(p, len, &pos) != str_ref(s, i)) return 0;
    }
    return pos == len;
}

int str_equal(obj a, obj b)
{
    int n = STR_LEN(a);
    if (n != STR_LEN(b)) return 0;
    if (BSTRINGP(a) && BSTRINGP(b)) return k_memeq(STR_DATA(a), STR_DATA(b), n);
    for (int i = 0; i < n; i++) if (str_ref(a, i) != str_ref(b, i)) return 0;
    return 1;
}

/* Names hash by their characters, whichever form holds them: the same
 * FNV-1a over the codes as the bytes of an ASCII name. */
uint32_t utf8_hash(const char *s, int len)
{
    uint32_t h = 2166136261u;
    int pos = 0;
    while (pos < len) h = (h ^ (uint32_t)utf8_decode(s, len, &pos)) * 16777619u;
    return h;
}

uint32_t str_hash(obj s)
{
    uint32_t h = 2166136261u;
    int n = STR_LEN(s);
    for (int i = 0; i < n; i++) h = (h ^ (uint32_t)str_ref(s, i)) * 16777619u;
    return h;
}

/* ---- case and letters ---------------------------------------------------------- */

static int in(int c, int lo, int hi) { return c >= lo && c <= hi; }

int char_upcase(int c)
{
    if (c < 0x80) return in(c, 'a', 'z') ? c - 32 : c;
    if (c < 0x100) return (in(c, 0xE0, 0xFE) && c != 0xF7) ? c - 32 : c == 0xFF ? 0x178 : c;
    if (c < 0x180) {
        if (in(c, 0x100, 0x12F) || in(c, 0x132, 0x137) || in(c, 0x14A, 0x177))
            return (c & 1) ? c - 1 : c;
        if (in(c, 0x139, 0x148) || in(c, 0x179, 0x17E)) return (c & 1) ? c : c - 1;
        return c;
    }
    if (in(c, 0x3B1, 0x3C1) || in(c, 0x3C3, 0x3CB)) return c - 32;
    if (c == 0x3AC) return 0x386;
    if (in(c, 0x3AD, 0x3AF)) return c - 37;
    if (c == 0x3CC) return 0x38C;
    if (in(c, 0x3CD, 0x3CE)) return c - 63;
    if (in(c, 0x430, 0x44F)) return c - 32;
    if (in(c, 0x450, 0x45F)) return c - 80;
    return c;
}

int char_downcase(int c)
{
    if (c < 0x80) return in(c, 'A', 'Z') ? c + 32 : c;
    if (c < 0x100) return (in(c, 0xC0, 0xDE) && c != 0xD7) ? c + 32 : c;
    if (c < 0x180) {
        if (c == 0x178) return 0xFF;
        if (in(c, 0x100, 0x12F) || in(c, 0x132, 0x137) || in(c, 0x14A, 0x177))
            return (c & 1) ? c : c + 1;
        if (in(c, 0x139, 0x148) || in(c, 0x179, 0x17E)) return (c & 1) ? c + 1 : c;
        return c;
    }
    if (in(c, 0x391, 0x3A1) || in(c, 0x3A3, 0x3AB)) return c + 32;
    if (c == 0x386) return 0x3AC;
    if (in(c, 0x388, 0x38A)) return c + 37;
    if (c == 0x38C) return 0x3CC;
    if (in(c, 0x38E, 0x38F)) return c + 63;
    if (in(c, 0x410, 0x42F)) return c + 32;
    if (in(c, 0x400, 0x40F)) return c + 80;
    return c;
}

int char_upper_p(int c) { return char_downcase(c) != c; }
int char_lower_p(int c) { return char_upcase(c) != c; }

int char_alpha_p(int c)
{
    if (c < 0x80) return in(c, 'a', 'z') || in(c, 'A', 'Z');
    if (char_upper_p(c) || char_lower_p(c)) return 1;
    return c == 0xAA || c == 0xB5 || c == 0xBA || c == 0xDF
        || in(c, 0x100, 0x24F)                        /* the rest of Latin Extended-A and B */
        || in(c, 0x250, 0x2AF)                        /* IPA */
        || c == 0x386 || (in(c, 0x388, 0x3FF) && c != 0x3F6)   /* Greek */
        || in(c, 0x400, 0x481) || in(c, 0x48A, 0x52F) /* Cyrillic */
        || in(c, 0x5D0, 0x5EA)                        /* Hebrew */
        || in(c, 0x620, 0x64A)                        /* Arabic */
        || in(c, 0x1E00, 0x1EFF)                      /* Latin Extended Additional */
        || in(c, 0x3041, 0x3096) || in(c, 0x30A1, 0x30FA)       /* kana */
        || in(c, 0x4E00, 0x9FFF)                      /* CJK ideographs */
        || in(c, 0xAC00, 0xD7A3);                     /* Hangul */
}

/* not a control character (C0, DEL, C1) and not a surrogate */
int char_graphic_p(int c)
{
    return (c >= 32 && c < 127) || (c >= 0xA0 && c < 0x110000 && !in(c, 0xD800, 0xDFFF));
}

/* ---- primitives ------------------------------------------------------------------ */

#define NEED(cond, msg, x) do { if (!(cond)) return lisp_error(msg, x); } while (0)
#define NEED_CHAR(x) NEED(CHARP(x), "Not a character:", x)
#define NEED_STR(x)  NEED(STRINGP(x), "Not a string:", x)

static obj boolean(int b) { return b ? S_T : NIL; }

obj tp_char_upcase(obj *a, int n)   { (void)n; NEED_CHAR(a[0]); return MAKE_CHAR(char_upcase(CHAR_CODE(a[0]))); }
obj tp_char_downcase(obj *a, int n) { (void)n; NEED_CHAR(a[0]); return MAKE_CHAR(char_downcase(CHAR_CODE(a[0]))); }
obj tp_upper_case_p(obj *a, int n)  { (void)n; NEED_CHAR(a[0]); return boolean(char_upper_p(CHAR_CODE(a[0]))); }
obj tp_lower_case_p(obj *a, int n)  { (void)n; NEED_CHAR(a[0]); return boolean(char_lower_p(CHAR_CODE(a[0]))); }
obj tp_both_case_p(obj *a, int n)
{
    (void)n;
    NEED_CHAR(a[0]);
    int c = CHAR_CODE(a[0]);
    return boolean(char_upper_p(c) || char_lower_p(c));
}
obj tp_alpha_char_p(obj *a, int n)   { (void)n; NEED_CHAR(a[0]); return boolean(char_alpha_p(CHAR_CODE(a[0]))); }
obj tp_graphic_char_p(obj *a, int n) { (void)n; NEED_CHAR(a[0]); return boolean(char_graphic_p(CHAR_CODE(a[0]))); }

/* (%make-string n wide) => N NULs */
obj tp_make_string(obj *a, int n)
{
    (void)n;
    NEED(FIXNUMP(a[0]) && FIX(a[0]) >= 0 && FIX(a[0]) < (1 << 24), "Bad string length:", a[0]);
    return make_string_of((int)FIX(a[0]), a[1] != NIL);
}

/* is S a wide string (four bytes per character)? */
obj tp_wide_string_p(obj *a, int n) { (void)n; return boolean(WSTRINGP(a[0])); }

/* (%widen s) => a wide copy of S */
obj tp_widen(obj *a, int n)
{
    (void)n;
    NEED_STR(a[0]);
    int len = STR_LEN(a[0]);
    obj w = make_wide_string(len);
    for (int i = 0; i < len; i++) WSTR_DATA(w)[i] = (uint32_t)str_ref(a[0], i);
    return w;
}

/* (%utf8-encode string) => its UTF-8 bytes, a narrow string */
obj tp_utf8_encode(obj *a, int n)
{
    (void)n;
    NEED_STR(a[0]);
    int len = str_utf8_length(a[0]);
    obj b = make_string_of(len, 0);
    obj s = a[0];
    int k = 0;
    for (int i = 0; i < STR_LEN(s); i++) {
        char e[4];
        int m = utf8_encode(str_ref(s, i), e);
        for (int j = 0; j < m; j++) STR_DATA(b)[k++] = e[j];
    }
    return b;
}

/* (%utf8-decode bytes) => the text in BYTES (a narrow string) read as UTF-8 */
obj tp_utf8_decode(obj *a, int n)
{
    (void)n;
    NEED(BSTRINGP(a[0]), "Not a narrow string:", a[0]);
    obj b = a[0];
    int len = STR_LEN(b);
    if (utf8_ascii_p(STR_DATA(b), len)) return b;
    int count = 0, wide = 0, pos = 0;
    while (pos < len) { if (utf8_decode(STR_DATA(b), len, &pos) >= 256) wide = 1; count++; }
    obj s = make_string_of(count, wide);
    b = a[0];
    pos = 0;
    for (int i = 0; i < count; i++) str_set(s, i, utf8_decode(STR_DATA(b), len, &pos));
    return s;
}
