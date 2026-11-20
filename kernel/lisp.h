/* lisp.h — object representation and the Lisp core's internal interface.
 *
 * Every Lisp value is a 64-bit word (obj). The low 3 bits are a tag:
 *
 *   000  fixnum      61-bit signed integer in the high bits
 *   001  cons        heap index of a 2-word cell (car, cdr)
 *   010  object      heap index of a headed object (string, vector, closure...)
 *   011  immediate   NIL, characters, and internal markers
 *   100  symbol      heap index of a headed symbol object
 *   101  header      first word of a headed object; never a value
 *
 * Pointers are word *indices* into the current semispace, not machine
 * addresses, so a heap image can be saved and reloaded anywhere (M3).
 */
#ifndef LISP_H
#define LISP_H

#include <stdint.h>

typedef uint64_t obj;
typedef int64_t fixnum_t;

#ifdef __wasm__
void *memcpy(void *dst, const void *src, __SIZE_TYPE__ n);   /* kernel/klib.c */
void *memset(void *dst, int c, __SIZE_TYPE__ n);
void *memmove(void *dst, const void *src, __SIZE_TYPE__ n);
#else
#include <string.h>
#endif

/* ---- tags ---------------------------------------------------------------- */

enum { TAG_FIXNUM = 0, TAG_CONS = 1, TAG_OBJECT = 2, TAG_IMM = 3, TAG_SYMBOL = 4, TAG_HEADER = 5 };

#define TAG(x)            ((unsigned)((x) & 7))
#define IDX(x)            ((uint64_t)(x) >> 3)
#define MAKE_PTR(i, tag)  (((obj)(i) << 3) | (tag))
#define IS_PTR(x)         (TAG(x) == TAG_CONS || TAG(x) == TAG_OBJECT || TAG(x) == TAG_SYMBOL)

/* immediates: payload << 8 | subtag << 3 | TAG_IMM */
enum { IMM_NIL = 0, IMM_CHAR = 1, IMM_UNBOUND = 2, IMM_FORWARD = 3 };

#define MAKE_IMM(sub, v)  (((obj)(v) << 8) | ((obj)(sub) << 3) | TAG_IMM)
#define IMM_SUB(x)        ((unsigned)((x) >> 3) & 31)
#define NIL               MAKE_IMM(IMM_NIL, 0)
#define UNBOUND           MAKE_IMM(IMM_UNBOUND, 0)
#define FORWARDED         MAKE_IMM(IMM_FORWARD, 0)   /* GC: car of a moved cons */
#define MAKE_CHAR(c)      MAKE_IMM(IMM_CHAR, (uint32_t)(c) & 0xffffff)
#define CHARP(x)          (TAG(x) == TAG_IMM && IMM_SUB(x) == IMM_CHAR)
#define CHAR_CODE(x)      ((int)((x) >> 8))

/* fixnums */
#define FIXNUM_MAX        ((INT64_C(1) << 60) - 1)
#define FIXNUM_MIN        (-(INT64_C(1) << 60))
#define MAKE_FIX(n)       ((obj)((uint64_t)(fixnum_t)(n) << 3))
#define FIX(x)            ((fixnum_t)(x) >> 3)
#define FIXNUMP(x)        (TAG(x) == TAG_FIXNUM)

/* headed objects: header = len << 8 | type << 3 | TAG_HEADER */
enum { HT_STRING = 1, HT_VECTOR, HT_SYMBOL, HT_CLOSURE, HT_MACRO, HT_PRIMITIVE, HT_FORWARD,
       HT_CFUNC, HT_CONDITION, HT_BIGNUM, HT_FLOAT, HT_ARRAY, HT_WSTRING, HT_RATIO,
       HT_LAST = HT_RATIO };

/* raw objects hold bytes, not values: the collector copies them without
 * looking inside. The header's length counts bytes. */
#define RAW_TYPE(t)       ((t) == HT_STRING || (t) == HT_BIGNUM || (t) == HT_FLOAT || (t) == HT_WSTRING)

#define MAKE_HEADER(t, n) (((obj)(n) << 8) | ((obj)(t) << 3) | TAG_HEADER)
#define HDR_TYPE(h)       ((unsigned)((h) >> 3) & 31)
#define HDR_LEN(h)        ((uint64_t)(h) >> 8)

extern obj *heap;

#define CAR(x)            heap[IDX(x)]
#define CDR(x)            heap[IDX(x) + 1]
#define HDR(x)            heap[IDX(x)]
#define FIELD(x, i)       heap[IDX(x) + 1 + (i)]
#define OBJ_TYPE(x)       HDR_TYPE(HDR(x))

#define CONSP(x)          (TAG(x) == TAG_CONS)
#define LISTP(x)          (CONSP(x) || (x) == NIL)
#define SYMBOLP(x)        (TAG(x) == TAG_SYMBOL || (x) == NIL)
#define OBJECTP(x, t)     (TAG(x) == TAG_OBJECT && OBJ_TYPE(x) == (t))
#define BSTRINGP(x)       OBJECTP(x, HT_STRING)     /* one byte per character (and bitmaps, bytecode) */
#define WSTRINGP(x)       OBJECTP(x, HT_WSTRING)    /* four bytes per character (M15) */
#define STRINGP(x)        (TAG(x) == TAG_OBJECT && (OBJ_TYPE(x) == HT_STRING || OBJ_TYPE(x) == HT_WSTRING))
#define VECTORP(x)        OBJECTP(x, HT_VECTOR)
#define BIGNUMP(x)        OBJECTP(x, HT_BIGNUM)
#define INTEGERP(x)       (FIXNUMP(x) || BIGNUMP(x))
#define FLOATP(x)         OBJECTP(x, HT_FLOAT)
#define RATIOP(x)         OBJECTP(x, HT_RATIO)     /* numerator/denominator, in lowest terms (M18) */
#define RATIONALP(x)      (INTEGERP(x) || RATIOP(x))
#define NUMBERP(x)        (INTEGERP(x) || FLOATP(x) || RATIOP(x))
enum { RAT_NUM, RAT_DEN, RAT_NFIELDS };
#define ARRAYP(x)         OBJECTP(x, HT_ARRAY)   /* an array header, not a simple vector */
#define CLOSUREP(x)       OBJECTP(x, HT_CLOSURE)
#define CFUNCP(x)         OBJECTP(x, HT_CFUNC)
#define MACROP(x)         (OBJECTP(x, HT_MACRO) || (CFUNCP(x) && (FIX(FIELD(x, CF_INFO)) & CFI_MACRO)))
#define PRIMITIVEP(x)     OBJECTP(x, HT_PRIMITIVE)
#define CONDITIONP(x)     OBJECTP(x, HT_CONDITION)

/* safe accessors for code that walks user-supplied forms */
#define SCAR(x)           (CONSP(x) ? CAR(x) : NIL)
#define SCDR(x)           (CONSP(x) ? CDR(x) : NIL)

/* the length in characters of either kind of string */
#define STR_LEN(x)        ((int)(OBJ_TYPE(x) == HT_WSTRING ? HDR_LEN(HDR(x)) / 4 : HDR_LEN(HDR(x))))
#define STR_DATA(x)       ((char *)&FIELD(x, 0))   /* a narrow string's bytes; invalid after any allocation */
#define WSTR_DATA(x)      ((uint32_t *)&FIELD(x, 0))   /* a wide string's characters */
static inline int str_ref(obj s, int i)
{
    return OBJ_TYPE(s) == HT_WSTRING ? (int)WSTR_DATA(s)[i] : (unsigned char)STR_DATA(s)[i];
}
/* => 0 if C does not fit (a narrow string and C >= 256) */
static inline int str_set(obj s, int i, int c)
{
    if (OBJ_TYPE(s) == HT_WSTRING) { WSTR_DATA(s)[i] = (uint32_t)c; return 1; }
    if (c >= 256) return 0;
    STR_DATA(s)[i] = (char)c;
    return 1;
}
#define VEC_LEN(x)        ((int)HDR_LEN(HDR(x)))
#define VEC_REF(x, i)     FIELD(x, i)

/* symbols (NIL is an immediate, not a heap symbol) */
enum { SYM_NAME, SYM_VALUE, SYM_FUNCTION, SYM_PLIST, SYM_FLAGS, SYM_PACKAGE, SYM_NFIELDS };
#define SYM(x, f)         FIELD(x, f)
#define SYMF_FORM_MASK    0xff     /* special form id */
#define SYMF_CONSTANT     0x100
#define SYMF_SPECIAL      0x200    /* dynamically bound (DEFVAR) */
#define SYMF_EXTERNAL     0x400    /* exported from its home package */
#define SYM_FLAGS_OF(x)   FIX(SYM(x, SYM_FLAGS))

/* interpreted closures and macros */
enum { FN_LAMBDA_LIST, FN_BODY, FN_ENV, FN_NAME, FN_NFIELDS };

/* compiled functions. CF_INFO is a fixnum:
 *   bits 0-7 required args, 8-15 optional args, 16 &rest, 17 macro,
 *   18-33 number of local slots (parameters included) */
enum { CF_CODE, CF_CONSTS, CF_NAME, CF_INFO, CF_ENV, CF_NFIELDS };
#define CFI_REQ(i)    ((int)((i) & 0xff))
#define CFI_OPT(i)    ((int)(((i) >> 8) & 0xff))
#define CFI_REST      (1 << 16)
#define CFI_MACRO     (1 << 17)
#define CFI_LOCALS(i) ((int)(((i) >> 18) & 0xffff))

/* conditions */
/* COND_DATA: the condition object a program made (an instance of a class
 * from DEFINE-CONDITION), or NIL for the kernel's own errors (M13) */
enum { COND_TYPE, COND_MESSAGE, COND_IRRITANTS, COND_DATA, COND_NFIELDS };

/* ---- memory (mem.c) ------------------------------------------------------ */

extern uint64_t heap_used, heap_limit, heap_max;
extern uint64_t gc_count, gc_every;

void     mem_set_limit(uint64_t words);
uint64_t alloc_words(uint64_t n);   /* may collect; returns a heap index */
void     gc(void);
void     gc_global(obj *p);          /* register a permanent root */

/* Temporary roots. Any C variable holding an obj that is used after a call
 * that may allocate must be protected. Unprotect in reverse order. */
#define MAX_ROOTS 4096
extern obj *gc_roots[MAX_ROOTS];
extern int gc_nroots;
#define PROTECT1(a)             (gc_roots[gc_nroots++] = &(a))
#define PROTECT2(a, b)          (PROTECT1(a), PROTECT1(b))
#define PROTECT3(a, b, c)       (PROTECT2(a, b), PROTECT1(c))
#define PROTECT4(a, b, c, d)    (PROTECT2(a, b), PROTECT2(c, d))
#define PROTECT5(a, b, c, d, e) (PROTECT4(a, b, c, d), PROTECT1(e))
#define UNPROTECT(n)            (gc_nroots -= (n))

/* the evaluator's stack is scanned by the collector */
#define STACK_MAX (1 << 18)
extern obj lisp_stack[STACK_MAX];
extern int lisp_sp;

/* special-variable bindings: pairs (symbol, saved value), scanned by the GC */
#define BIND_MAX (1 << 14)
extern obj bind_stack[3 * BIND_MAX];      /* symbol, old value, stack height */
/* Stack overflow is signalled at stack_limit, which leaves room for the
 * debugger; while it runs on a nearly full stack the limit is STACK_MAX. */
#define STACK_DEFAULT_LIMIT (STACK_MAX - 32768)
extern int stack_limit;
extern int bind_sp;

/* ---- errors -------------------------------------------------------------- */

extern int  lisp_err;            /* nonzero while an error is pending */
extern char lisp_err_msg[512];
extern obj  lisp_err_irritant;   /* UNBOUND = none */
extern obj  lisp_err_condition;  /* set instead of msg/irritant by ERROR */
obj  lisp_error(const char *msg, obj irritant);   /* returns NIL */
obj  lisp_signal(obj condition);                  /* returns NIL */
obj  lisp_take_condition(void);  /* turn the pending error into a condition and clear it */
obj  make_condition(obj type, obj message, obj irritants);
void lisp_fatal(const char *msg);

/* ---- objects (obj.c) ----------------------------------------------------- */

obj  alloc_object(unsigned type, uint64_t len, unsigned tag);
obj  cons(obj a, obj d);
obj  list2(obj a, obj b);
obj  make_string(const char *s, int len);      /* bytes as Latin-1; s must not point into the heap */
obj  make_string_utf8(const char *s, int len); /* UTF-8 text; s must not point into the heap */
obj  make_vector(int n, obj init);
obj  make_closure(unsigned type, obj ll, obj body, obj env, obj name);
obj  make_primitive(int index);
obj  make_symbol(obj name);
obj  intern(const char *s, int len);           /* s must not point into the heap */
obj  intern_c(const char *s);
obj  nreverse(obj list);
fixnum_t list_length(obj list);                /* -1 if improper or circular */
int  k_memeq(const void *a, const void *b, int n);

extern obj obarray;
extern obj S_T, S_QUOTE, S_FUNCTION, S_LAMBDA, S_OPTIONAL, S_REST, S_BODY,
           S_UNQUOTE, S_UNQUOTE_SPLICING, S_APPEND, S_LIST, S_PROGN, S_LET, S_LETSTAR,
           S_ERROR;

void obj_init(void);

/* ---- characters and strings beyond ASCII (text.c, M15) -------------------- */

int  utf8_decode(const char *s, int len, int *pos);   /* one character, advancing *pos */
int  utf8_encode(int c, char *out);                   /* => 1-4 bytes */
int  utf8_ascii_p(const char *s, int len);
int  utf8_count(const char *s, int len);
uint32_t utf8_hash(const char *s, int len);
obj  make_wide_string(int n);
obj  make_string_of(int n, int wide);                 /* N NULs, narrow or wide */
obj  make_string_codes(const int *codes, int n);
int  str_wide_p(obj s, int start, int end);           /* any character >= 256 there? */
int  str_utf8(obj s, char *buf, int cap);             /* => the bytes it needs */
int  str_utf8_length(obj s);
int  str_eq_utf8(obj s, const char *p, int len);
int  str_equal(obj a, obj b);
uint32_t str_hash(obj s);
int  char_upcase(int c);
int  char_downcase(int c);
int  char_upper_p(int c);
int  char_lower_p(int c);
int  char_alpha_p(int c);
int  char_graphic_p(int c);

/* ---- special forms (eval.c) ---------------------------------------------- */

enum {
    SF_NONE, SF_QUOTE, SF_FUNCTION, SF_IF, SF_PROGN, SF_SETQ, SF_LAMBDA,
    SF_DEFUN, SF_DEFMACRO, SF_LET, SF_LETSTAR, SF_COND, SF_AND, SF_OR, SF_WHILE,
};

/* ---- evaluator (eval.c) -------------------------------------------------- */

enum { RUN_DONE, RUN_MORE, RUN_ERROR, RUN_YIELD };   /* YIELD: waiting for input */

void eval_init(void);
void eval_start(obj form);
int  eval_run(long budget, long *used);
void eval_abort(void);
int  eval_active(void);
extern obj eval_result;
extern obj eval_error;            /* the condition after RUN_ERROR */
extern obj toplevel_evaluator;    /* function used to evaluate REPL forms, or NIL */
extern uint64_t eval_steps;
extern int yield_request;        /* set by %WAIT-FOR-EVENT: stop running until woken */
extern int abort_request;        /* set by the kernel on Abort (F1): throw to :ABORT */
extern double lisp_wake_at;      /* when a waiting program wants to run again (ms) */
int vm_throw_tag(obj tag, obj value);   /* 1 if a CATCH for TAG took it */
void bindings_out(void);                /* special bindings out of / into the symbols */
void bindings_in(void);
extern int yield_kind;                  /* why the program yielded: YIELD_* */
enum { YIELD_RUN, YIELD_INPUT, YIELD_SLEEP };

/* ---- processes (eval.c, scheduled by kernel.c) ----------------------------
 * A process is a Lisp vector (see world/processes.lisp):
 *   #(:process name function args state whostate wake-at
 *     saved-stack saved-bindings saved-registers input-p result)
 * While a process is switched out, its stack, special bindings and the
 * machine registers live in the heap, inside the process. */
enum { PR_MARK, PR_NAME, PR_FUNCTION, PR_ARGS, PR_STATE, PR_WHOSTATE, PR_WAKE,
       PR_STACK, PR_BINDS, PR_REGS, PR_INPUT, PR_RESULT, PR_STEPS, PR_NFIELDS };   /* PR_STEPS: evaluator steps used, for Peek */
enum { PS_NEW, PS_RUNNABLE, PS_INPUT, PS_SLEEP, PS_DEAD };
void process_start(obj p);   /* begin (apply function args) */
void process_save(obj p);    /* the running computation into P; the machine is left empty */
void process_load(obj p);    /* P's computation into the empty machine */

/* ---- bytecode (vm.c) -------------------------------------------------------- */

/* operand formats: "" none, "b" u8, "k" u16 constant index, "s" s16 jump,
 * "bb" two u8, "bs" u8 then s16 */
typedef struct { const char *name; const char *operands; } opinfo_t;
extern const opinfo_t opcodes[];
extern const int nopcodes;

/* ---- primitives (prims.c) ------------------------------------------------ */

typedef obj (*primfn_t)(obj *args, int nargs);
typedef struct { const char *name; primfn_t fn; int min, max; } prim_t;   /* max -1 = any */
enum { PRIM_FUNCALL, PRIM_APPLY, PRIM_EVAL, PRIM_INTERPRET };   /* handled by the machine */
extern const prim_t prims[];
extern const int nprims;
void prims_init(void);
extern double lisp_now_ms;

/* ---- output (print.c) ---------------------------------------------------- */

typedef struct sink { void (*write)(struct sink *s, const char *p, int n); } sink_t;

void lisp_set_output(void (*fn)(const char *p, int n));
extern sink_t *lisp_stdout;       /* tracks the column for fresh-line */
void lisp_write(const char *p, int n);
void lisp_puts(const char *s);
void lisp_fresh_line(void);
void print_obj(obj x, int escape, sink_t *out);

/* ---- reader (read.c) ----------------------------------------------------- */

enum { READ_OK, READ_EOF, READ_INCOMPLETE, READ_ERROR };
int lisp_read(const char *s, int len, int *pos, obj *out);   /* *out must be a root */
extern int read_fixups;          /* the last read left #S, #n= or #. markers (M16) */
int token_number_p(const char *tok, int n);   /* would the reader take TOK for a number? */

/* ---- world images (image.c) ---------------------------------------------- */

long image_save(const char *extra, long extra_len, const uint64_t **data);   /* bytes, 0 if busy */
const char *image_load(const uint64_t *in, long nbytes, const char **extra, long *extra_len);
uint64_t *mem_scratch(void);
uint64_t mem_semispace_words(void);
extern int world_save_requested;     /* set by (disk-save) */
extern int world_forget_requested;   /* set by (forget-world) */
extern uint64_t world_generation;    /* bumped whenever a top-level form finishes */

/* ---- REPL (repl.c) ------------------------------------------------------- */

void lisp_boot(void (*output)(const char *p, int n));          /* = lisp_init + lisp_load_world */
void lisp_init(void (*output)(const char *p, int n));
void lisp_load_world(void);
const char *lisp_boot_image(void (*output)(const char *p, int n), const uint64_t *in, long nbytes,
                            const char **extra, long *extra_len);
int  repl_feed(const char *s, int len);   /* text to read and evaluate, a form at a time */
int  repl_incomplete(void);              /* the text so far ends inside a form */
int  repl_busy(void);
void repl_run(long budget);
void repl_abort(void);
int  repl_error_count(void);
int  repl_pending_input(void);
void lisp_report_condition(obj c);   /* print ">>Error: ..." to the host */

/* ---- integers of any size (bignum.c) ------------------------------------- */

obj num_add(obj x, obj y);
obj num_sub(obj x, obj y);
obj num_mul(obj x, obj y);
obj num_quotient(obj x, obj y);          /* truncating, on integers; with a float, true division */
obj num_div(obj x, obj y);               /* true division: a ratio for integers that do not divide (M18) */
obj make_ratio(obj n, obj d);            /* N/D in lowest terms; an integer when D divides N */
obj num_numerator(obj x);
obj num_denominator(obj x);
obj num_rational_truncate(obj x);        /* a ratio's integer part, toward zero */
obj num_rem(obj x, obj y);
int num_cmp(obj x, obj y);               /* -1, 0, 1 */
int num_minusp(obj x);
int big_eql(obj x, obj y);
uint64_t big_hash(obj x);
int big_to_decimal(obj x, char *buf, int cap);
obj big_from_decimal(const char *digits, int n, int neg);
double big_to_double(obj x, int *overflow);   /* correctly rounded */
obj big_from_double(double d);                /* D must be integral and finite */
obj num_logop(int op, obj x, obj y);          /* 0 and, 1 or, 2 xor: two's complement */
obj num_ash(obj x, fixnum_t count);
obj num_integer_length(obj x, int count_ones);

/* ---- arrays (array.c) -------------------------------------------------- */

enum { AR_DIMS, AR_DATA, AR_FILL, AR_OFFSET, AR_FLAGS, AR_NFIELDS };
enum { AR_ADJUSTABLE = 1, AR_BITS = 2 };       /* AR_FLAGS */
int bit_array_p(obj a);
obj make_bit_vector(const char *bits, int n);  /* bytes 0 and 1 */
int array_rank(obj a);
int array_length(obj a);                       /* rank 1: the fill pointer, or the size */
obj array_ref(obj a, obj *subs, int nsub);
obj array_set(obj a, obj *subs, int nsub, obj v);
obj array_from_contents(int rank, obj contents);   /* #nA */

/* ---- packages (package.c) ----------------------------------------------- */

/* PK_EXTERNALS: symbols the package exports though their home is another
 * package (an imported symbol exported again, M17) */
enum { PK_TAG, PK_NAME, PK_NICKNAMES, PK_TABLE, PK_USE, PK_EXTERNALS, PK_NFIELDS };
enum { PS_INTERNAL = 1, PS_EXTERNAL, PS_INHERITED };
extern obj lisp_package, keyword_package, user_package, package_list, S_STAR_PACKAGE;
void packages_init(void);
int  packagep(obj x);
obj  current_package(void);
obj  find_package(const char *s, int n);
obj  pkg_find_symbol(obj pkg, const char *s, int n, int *status);   /* UNBOUND if none */
obj  pkg_intern(obj pkg, const char *s, int n);                      /* S not in the heap */
int  symbol_accessible(obj sym);       /* prints without a package prefix */
int  symbol_external(obj sym);

/* ---- floats (float.c) ------------------------------------------------------
 * One kind of float, a double: a raw 8-byte heap object. The bignum.c
 * operations hand any float argument to flo_arith, so floats work wherever
 * integers do, with float contagion. */

enum { FOP_ADD, FOP_SUB, FOP_MUL, FOP_DIV };
obj    make_float(double d);
double float_value(obj x);
double num_to_double(obj x);                  /* any number; error if too large */
obj    flo_arith(int op, obj x, obj y);
int    flo_cmp(obj x, obj y);
int    flo_eql(obj x, obj y);
uint64_t flo_hash(obj x);
int    flo_to_string(double d, char *buf, int cap);
int    flo_parse(const char *tok, int n, double *out);   /* 1 if TOK is a float */
obj    flo_truncate(obj x);                   /* the integer toward zero */

#endif
