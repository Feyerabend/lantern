/* mem.c — semispace heap and Cheney copying collector.
 *
 * Two static semispaces. Allocation bumps heap_used; when it would pass
 * heap_limit we collect. The last HEAP_RESERVE words are kept back so that
 * running out of memory becomes a Lisp error ("Heap exhausted") that the
 * evaluator can abort cleanly, instead of a crash.
 *
 * Roots: registered globals, the temporary root stack (PROTECTn), and the
 * evaluator stack.
 */
#include "lisp.h"

#ifndef HEAP_WORDS
#define HEAP_WORDS (UINT64_C(1) << 20)   /* 8 MiB per semispace */
#endif
#define HEAP_RESERVE 16384

static obj space_a[HEAP_WORDS], space_b[HEAP_WORDS];
obj *heap = space_a;
static obj *tospace = space_b;

uint64_t heap_used;
uint64_t heap_max = HEAP_WORDS - HEAP_RESERVE;
uint64_t heap_limit = HEAP_WORDS - HEAP_RESERVE;
uint64_t gc_count;
uint64_t gc_every;          /* stress mode: collect every N allocations (0 = off) */
static uint64_t alloc_count;

obj *gc_roots[MAX_ROOTS];
int gc_nroots;

#define MAX_GLOBALS 256
static obj *globals[MAX_GLOBALS];
static int nglobals;

void mem_set_limit(uint64_t words)
{
    if (words < 4096) words = 4096;
    if (words > heap_max) words = heap_max;
    heap_limit = words;
}

void gc_global(obj *p)
{
    for (int i = 0; i < nglobals; i++)
        if (globals[i] == p) return;          /* registering twice is harmless */
    if (nglobals >= MAX_GLOBALS) lisp_fatal("too many global roots");
    globals[nglobals++] = p;
}

/* ---- support for world images (image.c) ---------------------------------- */

int mem_nglobals(void) { return nglobals; }
obj *mem_global(int i) { return globals[i]; }
uint64_t mem_semispace_words(void) { return HEAP_WORDS; }

/* The idle semispace, free to use as scratch between collections. */
uint64_t *mem_scratch(void) { return tospace; }

/* Forget everything: empty heap, stacks and roots. */
void mem_reset(void)
{
    heap_used = 0;
    lisp_sp = 0;
    bind_sp = 0;
    gc_nroots = 0;
    for (int i = 0; i < nglobals; i++) *globals[i] = NIL;
}

/* Replace the heap with N words from SRC (which may be the scratch space). */
void mem_install(const uint64_t *src, uint64_t n)
{
    for (uint64_t i = 0; i < n; i++) heap[i] = src[i];
    heap_used = n;
}

static uint64_t payload_words(obj header)
{
    uint64_t n = HDR_LEN(header);
    if (RAW_TYPE(HDR_TYPE(header))) n = (n + 7) / 8;
    return n ? n : 1;   /* every object has room for a forwarding pointer */
}

static uint64_t to_free;
static const char *gc_phase = "";
static long gc_index;

static obj copy(obj x)
{
    if (!IS_PTR(x)) return x;
    uint64_t i = IDX(x);

    if (TAG(x) == TAG_CONS) {
        if (heap[i] == FORWARDED) return heap[i + 1];
        uint64_t n = to_free;
        to_free += 2;
        tospace[n] = heap[i];
        tospace[n + 1] = heap[i + 1];
        obj nx = MAKE_PTR(n, TAG_CONS);
        heap[i] = FORWARDED;
        heap[i + 1] = nx;
        return nx;
    }

    obj h = heap[i];
    if (TAG(h) != TAG_HEADER) {
        static char msg[160];
        const char *ph = gc_phase; int k = 0;
        const char *pre = "gc: pointer to a non-object, in ";
        while (*pre) msg[k++] = *pre++;
        while (*ph) msg[k++] = *ph++;
        msg[k++] = ' ';
        long v = gc_index;
        if (gc_phase[0] == 's' && gc_phase[1] == 'c') {   /* scan: report the containing object's type */
            obj w = tospace[gc_index];
            v = TAG(w) == TAG_HEADER ? (long)HDR_TYPE(w) * 1000000 + (long)HDR_LEN(w) : 999;
        }
        char d[24]; int m = 0; if (v < 0) v = -v; do { d[m++] = (char)('0' + v % 10); v /= 10; } while (v);
        while (m) msg[k++] = d[--m];
        msg[k] = 0;
        lisp_fatal(msg);
    }
    if (HDR_TYPE(h) == HT_FORWARD) return heap[i + 1];
    uint64_t size = 1 + payload_words(h);
    uint64_t n = to_free;
    to_free += size;
    for (uint64_t k = 0; k < size; k++) tospace[n + k] = heap[i + k];
    obj nx = MAKE_PTR(n, TAG(x));
    heap[i] = MAKE_HEADER(HT_FORWARD, 0);
    heap[i + 1] = nx;
    return nx;
}

/* ---- debugging: verify the heap before collecting (gc_verify = 1) -------- */

int gc_verify;

static int valid(obj x)
{
    if (!IS_PTR(x)) return 1;
    uint64_t i = IDX(x);
    if (i >= heap_used) return 0;
    obj w = heap[i];
    if (TAG(x) == TAG_CONS) return TAG(w) != TAG_HEADER && w != FORWARDED;
    if (TAG(w) != TAG_HEADER) return 0;
    if (TAG(x) == TAG_SYMBOL) return HDR_TYPE(w) == HT_SYMBOL;
    return HDR_TYPE(w) != HT_SYMBOL && HDR_TYPE(w) != HT_FORWARD;
}

static void verify_fail(const char *where, long a, long b)
{
    static char msg[200];
    int k = 0;
    const char *p = "gc verify: bad pointer in ";
    while (*p) msg[k++] = *p++;
    while (*where) msg[k++] = *where++;
    long vals[2] = { a, b };
    for (int j = 0; j < 2; j++) {
        msg[k++] = ' ';
        long v = vals[j];
        char d[24]; int m = 0; if (v < 0) { msg[k++] = '-'; v = -v; }
        do { d[m++] = (char)('0' + v % 10); v /= 10; } while (v);
        while (m) msg[k++] = d[--m];
    }
    msg[k] = 0;
    lisp_fatal(msg);
}

/* Non-fatal check used when loading an image: 1 if every root and every
 * pointer in the heap points at a valid object start. */
int heap_check(void)
{
    for (int i = 0; i < nglobals; i++) if (!valid(*globals[i])) return 0;
    uint64_t i = 0;
    while (i < heap_used) {
        obj w = heap[i];
        if (TAG(w) == TAG_HEADER) {
            if (HDR_TYPE(w) == HT_FORWARD || HDR_TYPE(w) > HT_LAST) return 0;
            uint64_t n = payload_words(w);
            if (i + 1 + n > heap_used) return 0;
            if (!RAW_TYPE(HDR_TYPE(w)))
                for (uint64_t k = 1; k <= n; k++) if (!valid(heap[i + k])) return 0;
            i += 1 + n;
        } else {
            if (i + 2 > heap_used || !valid(w) || !valid(heap[i + 1])) return 0;
            i += 2;
        }
    }
    return 1;
}

static void verify_heap(void)
{
    for (int i = 0; i < nglobals; i++) if (!valid(*globals[i])) verify_fail("global", i, 0);
    for (int i = 0; i < gc_nroots; i++) if (!valid(*gc_roots[i])) verify_fail("root", i, 0);
    for (int i = 0; i < lisp_sp; i++) if (!valid(lisp_stack[i])) verify_fail("stack (index, sp)", i, lisp_sp);
    for (int i = 0; i < bind_sp; i++) if (!valid(bind_stack[i])) verify_fail("bindings", i, 0);
    uint64_t i = 0;
    while (i < heap_used) {
        obj w = heap[i];
        if (TAG(w) == TAG_HEADER) {
            uint64_t n = payload_words(w);
            if (!RAW_TYPE(HDR_TYPE(w)))
                for (uint64_t k = 1; k <= n; k++)
                    if (!valid(heap[i + k])) verify_fail("object (type, field)", HDR_TYPE(w), (long)k);
            i += 1 + n;
        } else {
            if (!valid(w)) {
                obj d = heap[i + 1];
                lisp_fresh_line();
                lisp_puts("bad car: tag ");
                lisp_write(&"0123456789"[TAG(w)], 1);
                lisp_puts(", cdr is ");
                if (valid(d)) print_obj(d, 1, lisp_stdout); else lisp_puts("also bad");
                lisp_puts("\n");
                verify_fail("car of cons at (index, bad target index)", (long)i, (long)IDX(w));
            }
            if (!valid(heap[i + 1])) verify_fail("cdr of cons at", (long)i, 0);
            i += 2;
        }
    }
}

void gc(void)
{
    if (gc_verify) verify_heap();
    to_free = 0;
    gc_phase = "globals";
    for (int i = 0; i < nglobals; i++) { gc_index = i; *globals[i] = copy(*globals[i]); }
    gc_phase = "roots";
    for (int i = 0; i < gc_nroots; i++) { gc_index = i; *gc_roots[i] = copy(*gc_roots[i]); }
    gc_phase = "stack";
    for (int i = 0; i < lisp_sp; i++) { gc_index = i; lisp_stack[i] = copy(lisp_stack[i]); }
    gc_phase = "bindings";
    for (int i = 0; i < bind_sp; i++) { gc_index = i; bind_stack[i] = copy(bind_stack[i]); }
    gc_phase = "scan";

    uint64_t scan = 0;
    while (scan < to_free) {
        gc_index = (long)scan;
        obj w = tospace[scan];
        if (TAG(w) == TAG_HEADER) {
            uint64_t n = payload_words(w);
            if (!RAW_TYPE(HDR_TYPE(w)))
                for (uint64_t k = 1; k <= n; k++) tospace[scan + k] = copy(tospace[scan + k]);
            scan += 1 + n;
        } else {
            tospace[scan] = copy(w);
            tospace[scan + 1] = copy(tospace[scan + 1]);
            scan += 2;
        }
    }

    obj *t = heap;
    heap = tospace;
    tospace = t;
    heap_used = to_free;
    gc_count++;
}

uint64_t alloc_words(uint64_t n)
{
    if (gc_every && ++alloc_count >= gc_every) {
        alloc_count = 0;
        gc();
    }
    if (heap_used + n > heap_limit) {
        gc();
        if (heap_used + n > heap_limit) {
            if (heap_used + n > HEAP_WORDS) lisp_fatal("heap exhausted, reserve used up");
            lisp_error("Heap exhausted", UNBOUND);   /* the evaluator aborts at its next step */
        }
    }
    uint64_t i = heap_used;
    heap_used += n;
    return i;
}
