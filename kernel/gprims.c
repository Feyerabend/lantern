/* gprims.c — the "microcode" for graphics, events and the host.
 *
 * As on the Lisp Machines, only the pixel-pushing inner loops are below
 * Lisp: BITBLT, rectangle fill and drawing one character from a font raster.
 * Bitmaps and fonts are ordinary Lisp data built in world/graphics.lisp:
 *
 *   bitmap  #(:bitmap width height bytes)        bytes: a string, one ink per pixel
 *   font    #(:font name line-height baseline raster x-offsets widths)
 *           raster: a bitmap holding all glyphs side by side;
 *           x-offsets, widths: 128-element vectors indexed by character code
 *
 * The screen is named by the keyword :SCREEN wherever a bitmap is accepted.
 *
 * ALU operations combine a source ink S with the destination ink D:
 *   0 SETA   D = S                 (copy, opaque)
 *   1 IOR    D = max(D, S)         (draw ink over the background)
 *   2 XOR    D = D xor S           (with S = 255: invert; blinkers)
 *   3 ANDCA  D = min(D, 255 - S)   (erase where the source has ink)
 */
#include "kernel.h"
#include "lisp.h"

#define P(name) obj name(obj *a, int n)
#define NEED(cond, msg, x) do { if (!(cond)) return lisp_error(msg, x); } while (0)
#define NEED_FIX(x) NEED(FIXNUMP(x), "Not an integer:", x)

enum { ALU_SETA, ALU_IOR, ALU_XOR, ALU_ANDCA };

static int sym_named(obj x, const char *name)
{
    if (TAG(x) != TAG_SYMBOL) return 0;
    int n = 0;
    while (name[n]) n++;
    return str_eq_utf8(SYM(x, SYM_NAME), name, n);
}

typedef struct { uint8_t *p; int w, h; int screen; } surf_t;

/* Resolve a bitmap designator. Pointers into the heap stay valid only until
 * the next allocation, and nothing below allocates. */
static int surface(obj x, surf_t *s)
{
    if (sym_named(x, ":SCREEN")) {
        s->p = fb; s->w = SCREEN_W; s->h = SCREEN_H; s->screen = 1;
        return 1;
    }
    if (VECTORP(x) && VEC_LEN(x) >= 4 && sym_named(VEC_REF(x, 0), ":BITMAP")
        && FIXNUMP(VEC_REF(x, 1)) && FIXNUMP(VEC_REF(x, 2)) && BSTRINGP(VEC_REF(x, 3))) {
        fixnum_t w = FIX(VEC_REF(x, 1)), h = FIX(VEC_REF(x, 2));
        if (w < 0 || h < 0 || STR_LEN(VEC_REF(x, 3)) != w * h) return 0;
        s->p = (uint8_t *)STR_DATA(VEC_REF(x, 3)); s->w = (int)w; s->h = (int)h; s->screen = 0;
        return 1;
    }
    return 0;
}

static inline uint8_t apply_alu(int alu, uint8_t d, uint8_t s)
{
    switch (alu) {
    case ALU_IOR:   return d > s ? d : s;
    case ALU_XOR:   return d ^ s;
    case ALU_ANDCA: return d < 255 - s ? d : (uint8_t)(255 - s);
    default:        return s;
    }
}

/* Copy a W x H block from SRC (sx,sy) to DST (dx,dy), clipped to both, with
 * the right direction when they overlap (scrolling). */
static void blit(int alu, int w, int h, surf_t *src, int sx, int sy, surf_t *dst, int dx, int dy)
{
    if (sx < 0) { w += sx; dx -= sx; sx = 0; }
    if (sy < 0) { h += sy; dy -= sy; sy = 0; }
    if (dx < 0) { w += dx; sx -= dx; dx = 0; }
    if (dy < 0) { h += dy; sy -= dy; dy = 0; }
    if (sx + w > src->w) w = src->w - sx;
    if (sy + h > src->h) h = src->h - sy;
    if (dx + w > dst->w) w = dst->w - dx;
    if (dy + h > dst->h) h = dst->h - dy;
    if (w <= 0 || h <= 0) return;
    if (dst->screen) fb_dirty = 1;

    int same = src->p == dst->p;
    int down = same && dy > sy;           /* copy bottom row first */
    int right = same && dy == sy && dx > sx;
    for (int k = 0; k < h; k++) {
        int r = down ? h - 1 - k : k;
        uint8_t *sp = src->p + (sy + r) * src->w + sx;
        uint8_t *dp = dst->p + (dy + r) * dst->w + dx;
        if (alu == ALU_SETA) { memmove(dp, sp, (__SIZE_TYPE__)w); continue; }
        if (right) for (int c = w - 1; c >= 0; c--) dp[c] = apply_alu(alu, dp[c], sp[c]);
        else       for (int c = 0; c < w; c++)      dp[c] = apply_alu(alu, dp[c], sp[c]);
    }
}

static int alu_arg(obj x)
{
    return FIXNUMP(x) && FIX(x) >= 0 && FIX(x) <= 3 ? (int)FIX(x) : -1;
}

/* (bitblt alu width height from-bitmap from-x from-y to-bitmap to-x to-y) */
P(gp_bitblt)
{
    (void)n;
    surf_t src, dst;
    int alu = alu_arg(a[0]);
    NEED(alu >= 0, "Bad ALU operation:", a[0]);
    for (int i = 1; i <= 2; i++) NEED_FIX(a[i]);
    NEED(surface(a[3], &src), "Not a bitmap:", a[3]);
    NEED_FIX(a[4]); NEED_FIX(a[5]);
    NEED(surface(a[6], &dst), "Not a bitmap:", a[6]);
    NEED_FIX(a[7]); NEED_FIX(a[8]);
    blit(alu, (int)FIX(a[1]), (int)FIX(a[2]), &src, (int)FIX(a[4]), (int)FIX(a[5]),
         &dst, (int)FIX(a[7]), (int)FIX(a[8]));
    return NIL;
}

/* (%draw-rectangle width height x y alu ink &optional (bitmap :screen)) */
P(gp_draw_rect)
{
    surf_t dst;
    for (int i = 0; i < 4; i++) NEED_FIX(a[i]);
    int alu = alu_arg(a[4]);
    NEED(alu >= 0, "Bad ALU operation:", a[4]);
    NEED(FIXNUMP(a[5]) && FIX(a[5]) >= 0 && FIX(a[5]) <= 255, "Bad ink:", a[5]);
    if (n > 6) NEED(surface(a[6], &dst), "Not a bitmap:", a[6]);
    else { dst.p = fb; dst.w = SCREEN_W; dst.h = SCREEN_H; dst.screen = 1; }
    int w = (int)FIX(a[0]), h = (int)FIX(a[1]), x = (int)FIX(a[2]), y = (int)FIX(a[3]);
    uint8_t ink = (uint8_t)FIX(a[5]);
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > dst.w) w = dst.w - x;
    if (y + h > dst.h) h = dst.h - y;
    if (w <= 0 || h <= 0) return NIL;
    if (dst.screen) fb_dirty = 1;
    for (int r = 0; r < h; r++) {
        uint8_t *p = dst.p + (y + r) * dst.w + x;
        if (alu == ALU_SETA) memset(p, ink, (__SIZE_TYPE__)w);
        else for (int c = 0; c < w; c++) p[c] = apply_alu(alu, p[c], ink);
    }
    return NIL;
}

/* the entry for CODE in a font table of pages (a fixnum), into *OUT */
static int glyph_at(obj table, int code, fixnum_t *out)
{
    int page = code >> 8;
    if (page >= VEC_LEN(table)) return 0;
    obj p = VEC_REF(table, page);
    if (!VECTORP(p) || VEC_LEN(p) != 256 || !FIXNUMP(VEC_REF(p, code & 255))) return 0;
    *out = FIX(VEC_REF(p, code & 255));
    return 1;
}

/* (%draw-char font char-or-code x y alu &optional (bitmap :screen)) => width */
P(gp_draw_char)
{
    fixnum_t w_ = 0, xo_ = 0;
    obj font = a[0];
    NEED(VECTORP(font) && VEC_LEN(font) >= 7 && sym_named(VEC_REF(font, 0), ":FONT"), "Not a font:", font);
    obj xoffs = VEC_REF(font, 5), widths = VEC_REF(font, 6);
    NEED(VECTORP(xoffs) && VECTORP(widths) && VEC_LEN(xoffs) == VEC_LEN(widths), "Bad font:", font);
    surf_t src, dst;
    NEED(surface(VEC_REF(font, 4), &src), "Bad font raster:", font);
    int code = CHARP(a[1]) ? CHAR_CODE(a[1]) : FIXNUMP(a[1]) ? (int)FIX(a[1]) : -1;
    NEED(code >= 0, "Not a character:", a[1]);
    /* the tables are pages of 256 (world/graphics.lisp); a character the
     * font lacks is drawn as U+FFFD, or as ? */
    if (!glyph_at(widths, code, &w_) || !glyph_at(xoffs, code, &xo_)) {
        if (!glyph_at(widths, 0xFFFD, &w_) || !glyph_at(xoffs, 0xFFFD, &xo_)) {
            NEED(glyph_at(widths, '?', &w_) && glyph_at(xoffs, '?', &xo_), "Bad font:", font);
        }
    }
    NEED_FIX(a[2]); NEED_FIX(a[3]);
    int alu = alu_arg(a[4]);
    NEED(alu >= 0, "Bad ALU operation:", a[4]);
    if (n > 5) NEED(surface(a[5], &dst), "Not a bitmap:", a[5]);
    else { dst.p = fb; dst.w = SCREEN_W; dst.h = SCREEN_H; dst.screen = 1; }
    int w = (int)w_, xo = (int)xo_;
    blit(alu, w, src.h, &src, xo, 0, &dst, (int)FIX(a[2]), (int)FIX(a[3]));
    return MAKE_FIX(w);
}

/* (%bitmap-pixel bitmap x y) => ink, 0 outside */
P(gp_pixel)
{
    (void)n;
    surf_t s;
    NEED(surface(a[0], &s), "Not a bitmap:", a[0]);
    NEED_FIX(a[1]); NEED_FIX(a[2]);
    fixnum_t x = FIX(a[1]), y = FIX(a[2]);
    if (x < 0 || y < 0 || x >= s.w || y >= s.h) return MAKE_FIX(0);
    return MAKE_FIX(s.p[y * s.w + x]);
}

P(gp_screen_width)  { (void)a; (void)n; return MAKE_FIX(SCREEN_W); }
P(gp_screen_height) { (void)a; (void)n; return MAKE_FIX(SCREEN_H); }

/* (%make-bytes n) => a string of N zero bytes, the pixel store of a bitmap */
P(gp_make_bytes)
{
    (void)n;
    NEED(FIXNUMP(a[0]) && FIX(a[0]) >= 0 && (uint64_t)FIX(a[0]) < heap_limit * 4, "Bad size:", a[0]);
    return alloc_object(HT_STRING, (uint64_t)FIX(a[0]), TAG_OBJECT);
}

/* ---- events and waiting ----------------------------------------------------- */

/* (%next-event) => #(type a b modifiers), or NIL if the queue is empty */
P(gp_next_event)
{
    (void)a; (void)n;
    event_t ev;
    do {
        if (!event_pop(&ev)) return NIL;
    } while (ev.type == EV_KEY_UP || ev.type == EV_FOCUS);
    obj v = make_vector(4, NIL);
    VEC_REF(v, 0) = MAKE_FIX(ev.type);
    VEC_REF(v, 1) = MAKE_FIX(ev.a);
    VEC_REF(v, 2) = MAKE_FIX(ev.b);
    VEC_REF(v, 3) = MAKE_FIX(ev.mods);
    return v;
}

/* (%wait-for-event &optional timeout-ms): give control back to the host until
 * an event arrives or the timeout passes. Returns T at once if one is queued. */
P(gp_wait)
{
    if (event_pending()) return S_T;
    fixnum_t ms = n > 0 && FIXNUMP(a[0]) ? FIX(a[0]) : 1000;
    lisp_wake_at = lisp_now_ms + (double)ms;
    yield_kind = YIELD_INPUT;
    yield_request = 1;
    return NIL;
}

/* (%process-yield): let the other processes run; continue next round */
P(gp_process_yield)
{
    (void)a; (void)n;
    yield_kind = YIELD_RUN;
    yield_request = 1;
    return NIL;
}

/* (%process-sleep ms) */
P(gp_process_sleep)
{
    (void)n;
    NEED_FIX(a[0]);
    lisp_wake_at = lisp_now_ms + (double)FIX(a[0]);
    yield_kind = YIELD_SLEEP;
    yield_request = 1;
    return NIL;
}

/* (store-conditional vector index old new): if element INDEX is EQ to OLD,
 * replace it with NEW and return T; else NIL. Atomic, because processes are
 * only switched between instructions. */
P(gp_store_conditional)
{
    (void)n;
    NEED(VECTORP(a[0]), "Not a vector:", a[0]);
    NEED(FIXNUMP(a[1]) && FIX(a[1]) >= 0 && FIX(a[1]) < VEC_LEN(a[0]), "Bad index:", a[1]);
    if (VEC_REF(a[0], FIX(a[1])) != a[2]) return NIL;
    VEC_REF(a[0], FIX(a[1])) = a[3];
    return S_T;
}

/* ---- the host ---------------------------------------------------------------------- */

/* (%host-write string): the host's output (terminal, or the browser console) */
P(gp_host_write)
{
    (void)n;
    NEED(STRINGP(a[0]), "Not a string:", a[0]);
    if (BSTRINGP(a[0]) && utf8_ascii_p(STR_DATA(a[0]), STR_LEN(a[0]))) {
        lisp_write(STR_DATA(a[0]), STR_LEN(a[0]));
    } else {
        static char buf[4096];
        obj s = a[0];
        int k = 0;
        for (int i = 0; i < STR_LEN(s); i++) {          /* as UTF-8, a piece at a time */
            if (k > (int)sizeof buf - 4) { lisp_write(buf, k); k = 0; }
            k += utf8_encode(str_ref(s, i), buf + k);
        }
        lisp_write(buf, k);
    }
    return a[0];
}

P(gp_host_fresh_line) { (void)a; (void)n; lisp_fresh_line(); return NIL; }

/* (%world-changed): tell the host the world has changed (it autosaves) */
P(gp_world_changed) { (void)a; (void)n; world_generation++; return NIL; }

/* ---- files, through the host (M9) ----------------------------------------------
 *
 * The machine has no file system: the host has (make serve, tools/serve.py).
 * Lisp puts one request at a time in FILE_REQUEST; the host takes it after a
 * frame (kernel_file_request in kernel.c), does it, and answers with
 * kernel_file_reply, which sets *FILE-REPLY* to (id status . text). */

obj file_request = NIL;     /* #(id op name text), or NIL */
int file_server;            /* the host said it can serve files */
static fixnum_t file_ids;

/* (%file-request op name text) => the request's id, or NIL while another one waits.
 * OP: 1 list the files, 2 read NAME, 3 write TEXT to NAME, 4 download TEXT as
 * NAME, 5 upload files (the host asks the user), 6 where the files are kept,
 * 7 choose a folder for them (the host asks), 8 take the files dropped on the page.
 * Uploads and drops reply with "name\nlength\nbytes" for each file. */
P(gp_file_request)
{
    (void)n;
    /* + 256: the text is bytes (a narrow string), passed as they are, not as UTF-8 (M18) */
    NEED(FIXNUMP(a[0]) && (FIX(a[0]) & 255) >= 1 && (FIX(a[0]) & 255) <= 8 && FIX(a[0]) < 512, "Bad file operation:", a[0]);
    NEED(STRINGP(a[1]), "Not a string:", a[1]);
    NEED(STRINGP(a[2]), "Not a string:", a[2]);
    if (file_request != NIL) return NIL;
    obj v = make_vector(4, NIL);       /* a[] is on the stack: the collector keeps it up to date */
    VEC_REF(v, 0) = MAKE_FIX(++file_ids);
    VEC_REF(v, 1) = a[0];
    VEC_REF(v, 2) = a[1];
    VEC_REF(v, 3) = a[2];
    file_request = v;
    return VEC_REF(v, 0);
}

/* => how the host keeps files: 1 a file server, 2 the browser's storage, 3 a
 * folder the user chose; NIL when there is no host for files (the terminal) */
P(gp_file_server_p) { (void)a; (void)n; return file_server ? MAKE_FIX(file_server) : NIL; }

/* (%heap-stats) => (words-used words-limit collections evaluator-steps) */
P(gp_heap_stats)
{
    (void)a; (void)n;
    obj r = NIL;
    PROTECT1(r);
    r = cons(MAKE_FIX((fixnum_t)eval_steps), r);
    r = cons(MAKE_FIX((fixnum_t)gc_count), r);
    r = cons(MAKE_FIX((fixnum_t)heap_limit), r);
    r = cons(MAKE_FIX((fixnum_t)heap_used), r);
    UNPROTECT(1);
    return r;
}

/* ---- reading from a string ------------------------------------------------- */

/* did the last read leave markers for world/reader.lisp to finish? */
P(gp_read_fixups_p) { (void)a; (void)n; return read_fixups ? S_T : NIL; }

/* (%read-from-string string start) => (form . end), :INCOMPLETE or :EOF;
 * a syntax error is signalled. */
P(gp_read_from_string)
{
    static char buf[65536 * 4];
    NEED(STRINGP(a[0]), "Not a string:", a[0]);
    int len = STR_LEN(a[0]);
    NEED(len < 65536, "String too long to read", a[0]);
    int start = 0;
    if (n > 1) { NEED_FIX(a[1]); start = (int)FIX(a[1]); }
    NEED(start >= 0 && start <= len, "Bad start position:", a[1]);
    /* the reader reads UTF-8; positions go back to characters */
    int ascii = BSTRINGP(a[0]) && utf8_ascii_p(STR_DATA(a[0]), len);
    int bytes = str_utf8(a[0], buf, (int)sizeof buf), pos = start;
    if (!ascii) {
        pos = 0;
        for (int i = 0; i < start; i++) { char e[4]; pos += utf8_encode(str_ref(a[0], i), e); }
    }
    int from = pos;
    obj form = NIL;
    PROTECT1(form);
    int st = lisp_read(buf, bytes, &pos, &form);
    obj r = NIL;
    if (st == READ_OK) r = cons(form, MAKE_FIX(ascii ? pos : start + utf8_count(buf + from, pos - from)));
    else if (st == READ_INCOMPLETE) r = intern_c(":INCOMPLETE");
    else if (st == READ_EOF) r = intern_c(":EOF");
    UNPROTECT(1);
    return r;           /* READ_ERROR: the error is already pending */
}

/* ---- bit operations ---------------------------------------------------------- */

/* LOGAND, LOGIOR, LOGXOR on integers of any size: fixnums here, others in bignum.c */
static obj logfold(obj *a, int n, int op, fixnum_t start)
{
    int fast = 1;
    for (int i = 0; i < n; i++) {
        NEED(INTEGERP(a[i]), "Not an integer:", a[i]);
        if (!FIXNUMP(a[i])) fast = 0;
    }
    if (fast) {
        fixnum_t r = start;
        for (int i = 0; i < n; i++)
            r = op == 0 ? (r & FIX(a[i])) : op == 1 ? (r | FIX(a[i])) : (r ^ FIX(a[i]));
        return MAKE_FIX(r);
    }
    obj r = MAKE_FIX(start);
    PROTECT1(r);
    for (int i = 0; i < n && !lisp_err; i++) r = num_logop(op, r, a[i]);
    UNPROTECT(1);
    return lisp_err ? NIL : r;
}

P(gp_logand) { return logfold(a, n, 0, -1); }
P(gp_logior) { return logfold(a, n, 1, 0); }
P(gp_logxor) { return logfold(a, n, 2, 0); }

/* (ash n count): shift left for positive COUNT, right (rounding down) for negative */
P(gp_ash)
{
    (void)n;
    NEED(INTEGERP(a[0]), "Not an integer:", a[0]);
    NEED_FIX(a[1]);
    fixnum_t c = FIX(a[1]);
    if (FIXNUMP(a[0])) {
        fixnum_t v = FIX(a[0]);
        if (c <= -63) return MAKE_FIX(v < 0 ? -1 : 0);
        if (c <= 0) return MAKE_FIX(v >> -c);
        if (c < 60) {
            fixnum_t r = (fixnum_t)((uint64_t)v << c);
            if ((r >> c) == v && r >= FIXNUM_MIN && r <= FIXNUM_MAX) return MAKE_FIX(r);
        }
    }
    return num_ash(a[0], c);
}

/* (%integer-length n) and (%logcount n) */
P(gp_integer_length) { (void)n; NEED(INTEGERP(a[0]), "Not an integer:", a[0]); return num_integer_length(a[0], 0); }
P(gp_logcount)       { (void)n; NEED(INTEGERP(a[0]), "Not an integer:", a[0]); return num_integer_length(a[0], 1); }
