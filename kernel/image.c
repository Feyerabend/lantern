/* image.c — saving and loading the world (the "band" of the Lisp Machines).
 *
 * An image is a snapshot of the whole Lisp world, taken when the machine is
 * idle and right after a compacting collection:
 *
 *   header (IMG_HEADER_WORDS words)
 *   values of the registered global roots
 *   heap words [0, heap_used)
 *   "extra" bytes for the host (the listener transcript), padded to a word
 *
 * All words are little-endian 64-bit, the same on wasm32 and arm64. Heap
 * pointers are word indices, so an image needs no relocation and moves
 * freely between the native build and the browser.
 *
 * The header carries a fingerprint of everything an image depends on: the
 * primitive and opcode tables, the number of roots, the heap size and the
 * world source embedded in the kernel. An image from another build, a
 * truncated file or a damaged one is rejected, never half-loaded.
 */
#include "lisp.h"

#define IMG_VERSION 1
enum {
    H_MAGIC, H_VERSION, H_FINGERPRINT, H_HEAP_WORDS, H_NGLOBALS, H_GENSYM,
    H_EXTRA_BYTES, H_CHECKSUM, IMG_HEADER_WORDS
};

int world_save_requested;
int world_forget_requested;
uint64_t world_generation;     /* bumped whenever a top-level form finishes */

extern uint64_t gensym_counter;
extern const char *const world_source;   /* repl.c: the embedded boot + compiler */
extern const long world_source_len;

int mem_nglobals(void);
obj *mem_global(int i);
uint64_t mem_semispace_words(void);
uint64_t *mem_scratch(void);
void mem_reset(void);
void mem_install(const uint64_t *src, uint64_t n);
int heap_check(void);

static uint64_t magic(void)
{
    const char *m = "LISPIMG1";
    uint64_t v = 0;
    for (int i = 7; i >= 0; i--) v = (v << 8) | (unsigned char)m[i];
    return v;
}

static uint64_t fnv(uint64_t h, const void *p, long n)
{
    const unsigned char *b = p;
    for (long i = 0; i < n; i++) h = (h ^ b[i]) * UINT64_C(1099511628211);
    return h;
}

static uint64_t fnv_str(uint64_t h, const char *s)
{
    long n = 0;
    while (s[n]) n++;
    return fnv(h, s, n + 1);
}

static uint64_t fingerprint(void)
{
    uint64_t h = UINT64_C(14695981039346656037);
    for (int i = 0; i < nprims; i++) h = fnv_str(h, prims[i].name);
    for (int i = 0; i < nopcodes; i++) h = fnv_str(h, opcodes[i].name);
    uint64_t n[3] = { (uint64_t)mem_nglobals(), mem_semispace_words(), IMG_VERSION };
    h = fnv(h, n, sizeof n);
    return fnv(h, world_source, world_source_len);
}

/* Write an image of the idle world into the scratch semispace.
 * Returns its size in bytes, or 0 if the machine is busy or it does not fit. */
/* The caller must make sure the machine is idle (at top level, or waiting
 * for input): the evaluator's stack is not part of the image. */
long image_save(const char *extra, long extra_len, const uint64_t **data)
{
    if (lisp_err || gc_nroots) return 0;
    gc();                                   /* compact; the old space is now scratch */
    uint64_t *out = mem_scratch();
    int ng = mem_nglobals();
    uint64_t extra_words = ((uint64_t)extra_len + 7) / 8;
    uint64_t total = IMG_HEADER_WORDS + (uint64_t)ng + heap_used + extra_words;
    if (total > mem_semispace_words()) return 0;

    uint64_t *p = out + IMG_HEADER_WORDS;
    for (int i = 0; i < ng; i++) *p++ = *mem_global(i);
    bindings_out();                         /* the image holds global values */
    for (uint64_t i = 0; i < heap_used; i++) *p++ = heap[i];
    bindings_in();
    p[extra_words ? extra_words - 1 : 0] = 0;
    if (extra_len) memcpy(p, extra, (__SIZE_TYPE__)extra_len);

    out[H_MAGIC] = magic();
    out[H_VERSION] = IMG_VERSION;
    out[H_FINGERPRINT] = fingerprint();
    out[H_HEAP_WORDS] = heap_used;
    out[H_NGLOBALS] = (uint64_t)ng;
    out[H_GENSYM] = gensym_counter;
    out[H_EXTRA_BYTES] = (uint64_t)extra_len;
    out[H_CHECKSUM] = fnv(UINT64_C(14695981039346656037), out + IMG_HEADER_WORDS,
                          (long)(total - IMG_HEADER_WORDS) * 8);
    *data = out;
    return (long)(total * 8);
}

/* Check an image without touching the world. Returns NULL or the reason. */
static const char *image_validate(const uint64_t *in, long nbytes)
{
    if (nbytes < IMG_HEADER_WORDS * 8 || nbytes % 8) return "not a world image (bad size)";
    if (in[H_MAGIC] != magic()) return "not a world image";
    if (in[H_VERSION] != IMG_VERSION) return "image format version differs";
    if (in[H_FINGERPRINT] != fingerprint()) return "image was saved by a different version of the system";
    if (in[H_NGLOBALS] != (uint64_t)mem_nglobals()) return "image does not match this kernel";
    uint64_t words = (uint64_t)nbytes / 8;
    uint64_t need = IMG_HEADER_WORDS + in[H_NGLOBALS] + in[H_HEAP_WORDS] + (in[H_EXTRA_BYTES] + 7) / 8;
    if (in[H_HEAP_WORDS] > heap_limit || need != words) return "image is truncated or damaged";
    uint64_t sum = fnv(UINT64_C(14695981039346656037), in + IMG_HEADER_WORDS,
                       (long)(words - IMG_HEADER_WORDS) * 8);
    if (sum != in[H_CHECKSUM]) return "image checksum does not match (damaged)";
    return 0;
}

/* Replace the world with the image at IN (which may be the scratch space).
 * On success returns NULL and points *extra at the host's extra bytes, which
 * stay valid only until the next allocation. On failure the world is left
 * empty: the caller must boot afresh. */
const char *image_load(const uint64_t *in, long nbytes, const char **extra, long *extra_len)
{
    const char *why = image_validate(in, nbytes);
    if (why) return why;
    int ng = (int)in[H_NGLOBALS];
    uint64_t hw = in[H_HEAP_WORDS];
    const uint64_t *g = in + IMG_HEADER_WORDS;

    mem_reset();
    mem_install(g + ng, hw);
    for (int i = 0; i < ng; i++) *mem_global(i) = g[i];
    gensym_counter = in[H_GENSYM];
    if (!heap_check()) { mem_reset(); return "image contents are inconsistent"; }

    *extra = (const char *)(g + ng + hw);
    *extra_len = (long)in[H_EXTRA_BYTES];
    return 0;
}
