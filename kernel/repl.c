/* repl.c — boot, errors, and the read-eval-print driver shared by every host.
 *
 * Text arrives with repl_feed() (a line, or a whole file) and waits in a
 * buffer; repl_run(budget) reads one form, evaluates it a bounded number of
 * steps at a time (so a host can interleave evaluation with drawing the
 * screen), and only then reads the next. So a form can change how the next
 * one is read: (in-package ...) at the top of a file works.
 */
#include "lisp.h"
#include "boot_lisp.h"   /* generated from world/boot.lisp by tools/embed.py */

#define NO_IRRITANT UNBOUND
#define INBUF_MAX 65536

int  lisp_err;
char lisp_err_msg[512];
obj  lisp_err_irritant = NIL;
obj  lisp_err_condition = NIL;

int repl_echo = 1;               /* print the value of each form */
static char inbuf[INBUF_MAX];
static int inlen;                /* text in inbuf */
static int rpos;                 /* where the next form starts */
static int waiting;              /* the text from rpos is an unfinished form */
static int errors;

obj lisp_error(const char *msg, obj irritant)
{
    if (!lisp_err) {
        int i = 0;
        for (; msg[i] && i < (int)sizeof lisp_err_msg - 1; i++) lisp_err_msg[i] = msg[i];
        lisp_err_msg[i] = 0;
        lisp_err_irritant = irritant;
        lisp_err_condition = NIL;
        lisp_err = 1;
    }
    return NIL;
}

obj lisp_signal(obj condition)
{
    if (!lisp_err) {
        lisp_err_condition = condition;
        lisp_err = 1;
    }
    return NIL;
}

obj lisp_take_condition(void)
{
    lisp_err = 0;
    if (lisp_err_condition != NIL) {
        obj c = lisp_err_condition;
        lisp_err_condition = NIL;
        return c;
    }
    obj irritants = lisp_err_irritant, msg = NIL;
    lisp_err_irritant = NIL;
    PROTECT2(irritants, msg);
    irritants = irritants == NO_IRRITANT ? NIL : cons(irritants, NIL);
    int len = 0;
    while (lisp_err_msg[len]) len++;
    msg = make_string_utf8(lisp_err_msg, len);
    obj c = make_condition(S_ERROR, msg, irritants);
    UNPROTECT(2);
    return c;
}

void lisp_fatal(const char *msg)
{
    lisp_fresh_line();
    lisp_puts(">>Fatal: ");
    lisp_puts(msg);
    lisp_puts("\n");
    __builtin_trap();
}

void lisp_report_condition(obj c)
{
    lisp_fresh_line();
    lisp_puts(">>Error: ");
    if (CONDITIONP(c)) {
        if (FIELD(c, COND_TYPE) != S_ERROR) {
            print_obj(FIELD(c, COND_TYPE), 1, lisp_stdout);
            lisp_puts(": ");
        }
        print_obj(FIELD(c, COND_MESSAGE), 0, lisp_stdout);
        for (obj i = FIELD(c, COND_IRRITANTS); CONSP(i); i = CDR(i)) {
            lisp_puts(" ");
            print_obj(CAR(i), 1, lisp_stdout);
        }
    } else {
        print_obj(c, 1, lisp_stdout);
    }
    lisp_puts("\n");
    errors++;
}

static void report_error(void)
{
    lisp_report_condition(lisp_take_condition());
}

/* Text to read and evaluate. => 0; repl_incomplete() says, once the forms
 * before it have run, whether the text ends inside a form. */
int repl_feed(const char *s, int len)
{
    if (rpos > 0) {                            /* drop what has been read */
        for (int i = rpos; i < inlen; i++) inbuf[i - rpos] = inbuf[i];
        inlen -= rpos;
        rpos = 0;
    }
    if (inlen + len > INBUF_MAX) {
        lisp_error("Input too long", NO_IRRITANT);
        report_error();
        inlen = 0;
        return 0;
    }
    memcpy(inbuf + inlen, s, (__SIZE_TYPE__)len);
    inlen += len;
    waiting = 0;
    return 0;
}

/* read the next form and start evaluating it; 0 if there is none (yet) */
static int start_next(void)
{
    if (waiting || rpos >= inlen) return 0;
    obj form = NIL;
    PROTECT1(form);
    int pos = rpos;
    int st = lisp_read(inbuf, inlen, &pos, &form);
    int started = 0;
    if (st == READ_OK) {
        rpos = pos;
        if (read_fixups) {             /* #S(...), #1=, #.: Lisp finishes them, then evaluates */
            obj f = intern_c("%READ-FIXUP-EVAL");
            if (SYM(f, SYM_FUNCTION) != UNBOUND) {
                PROTECT1(f);
                form = list2(S_QUOTE, form);
                form = list2(f, form);
                UNPROTECT(1);
            }
        }
        eval_start(form);
        started = 1;
    } else if (st == READ_EOF) {
        inlen = rpos = 0;
    } else if (st == READ_INCOMPLETE) {
        waiting = 1;
    } else {
        report_error();   /* READ_ERROR: drop the rest of the input */
        inlen = rpos = 0;
    }
    UNPROTECT(1);
    return started;
}

int repl_incomplete(void) { return waiting; }

int repl_busy(void) { return eval_active() || (!waiting && rpos < inlen); }
int repl_pending_input(void) { return rpos < inlen; }
int repl_error_count(void) { return errors; }

void repl_run(long budget)
{
    while (budget > 0) {
        if (!eval_active() && !start_next()) return;
        long used = 0;
        int st = eval_run(budget, &used);
        budget -= used > 0 ? used : 1;
        if (st == RUN_DONE && repl_echo) {
            lisp_fresh_line();
            print_obj(eval_result, 1, lisp_stdout);
            lisp_write("\n", 1);
        } else if (st == RUN_ERROR) {
            lisp_report_condition(eval_error);
            eval_error = NIL;
        }
        if (st == RUN_DONE || st == RUN_ERROR) world_generation++;
    }
}

void repl_abort(void)
{
    eval_abort();
    inlen = rpos = 0;
    waiting = 0;
    lisp_err = 0;
}

/* The world source the kernel boots from, also part of an image's fingerprint. */
const char *const world_source = boot_lisp;
const long world_source_len = (long)sizeof boot_lisp - 1;

/* Register the roots and create the kernel's own objects (symbols, special
 * forms, primitives). Safe to call again after mem_reset(). */
void lisp_init(void (*output)(const char *p, int n))
{
    lisp_set_output(output);
    { extern obj file_request; gc_global(&file_request); }
    gc_global(&lisp_err_irritant);
    gc_global(&lisp_err_condition);
    obj_init();
    eval_init();
    prims_init();
    inlen = rpos = 0;
    waiting = 0;
}

/* Build the world from source: boot.lisp, then the compiler. */
void lisp_load_world(void)
{
    int echo = repl_echo;
    repl_echo = 0;
    /* feed the source in pieces that end at a newline, evaluating as we go */
    long len = (long)sizeof boot_lisp - 1, pos = 0;
    while (pos < len) {
        long n = len - pos;
        if (n > INBUF_MAX / 2) {
            n = INBUF_MAX / 2;
            while (n > 1 && boot_lisp[pos + n - 1] != '\n') n--;
        }
        repl_feed(boot_lisp + pos, (int)n);
        pos += n;
        while (repl_busy()) repl_run(1L << 30);
    }
    if (repl_incomplete()) {
        lisp_puts(">>Error: the world source ends inside a form\n");
        errors++;
        inlen = rpos = 0;
        waiting = 0;
    }
    SYM(S_STAR_PACKAGE, SYM_VALUE) = user_package;   /* the Listener starts in CL-USER */
    repl_echo = echo;
}

void lisp_boot(void (*output)(const char *p, int n))
{
    lisp_init(output);
    lisp_load_world();
}

/* Boot from a saved image; if it is rejected, boot from source instead and
 * return the reason. */
const char *lisp_boot_image(void (*output)(const char *p, int n), const uint64_t *in, long nbytes,
                            const char **extra, long *extra_len)
{
    uint64_t every = gc_every;
    gc_every = 0;                 /* IN may be the scratch space: no collection now */
    lisp_init(output);
    const char *why = image_load(in, nbytes, extra, extra_len);
    gc_every = every;
    if (why) {
        *extra = 0;
        *extra_len = 0;
        lisp_init(output);
        lisp_load_world();
    }
    return why;
}
