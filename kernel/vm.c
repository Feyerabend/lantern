/* vm.c — the bytecode VM, non-local exits, special bindings and conditions.
 *
 * Compiled functions run on the same lisp_stack as the interpreter, so the
 * two can call each other freely and the whole machine state is still just
 * the stack plus a few registers (see machine.h). Each VM instruction counts
 * as one evaluator step for the per-frame budget.
 *
 * Calls: the caller pushes the function, then the arguments, then CALL n.
 * vm_call() dispatches on what the function is:
 *   compiled   -> builds a VM frame (no C recursion)
 *   primitive  -> called directly; FUNCALL and APPLY are done in place
 *   interpreted closure, EVAL -> handed to the interpreter, with an
 *                 F_VM_RESUME frame so its value comes back here
 *
 * Non-local exits use "records" on the stack, linked through catch_top:
 *   CATCH    (catch tag ...)            target of THROW with an EQ tag
 *   UWP      (unwind-protect ...)       cleanup code runs while unwinding
 *   HANDLER  (handler-case ...)         target for signalled conditions
 * A record is 6 words: kind, tag, handler pc, fp, binding-stack height, link.
 *
 * Special variables use shallow binding: BIND saves the old value on
 * bind_stack and stores the new one in the symbol; unwinding restores.
 * Each binding takes three words: symbol, old value, and the stack height
 * when it was made, so that returning from a frame (the debugger) can undo
 * exactly the bindings made inside it.
 *
 * The debugger (M8) works on live frames: %FRAMES lists them, and
 * %FRAME-RETURN / %FRAME-REINVOKE leave a frame early, running the
 * unwind-protect cleanups inside it.
 */
#include "machine.h"

int vm_fp = -1, vm_pc;

/* Multiple values (M11). VALUES leaves all its values in mv_list and sets
 * mv_valid; MVLIST, right after the form that produced them, takes the list.
 * Every other instruction clears mv_valid, except those that pass a form's
 * values on (RET out of a function, JMP out of a branch, UNBIND out of a
 * LET, UNCATCH out of a CATCH), so values never outlive the expression they
 * came from. Both are saved with a process. */
int mv_valid;
obj mv_list = NIL;

obj p_values(obj *a, int n)
{
    obj l = NIL;
    PROTECT1(l);
    for (int i = n - 1; i >= 0; i--) l = cons(a[i], l);
    UNPROTECT(1);
    mv_list = l;
    mv_valid = 1;
    return n ? a[0] : NIL;
}
int catch_top = -1;
obj bind_stack[3 * BIND_MAX];
int stack_limit = STACK_DEFAULT_LIMIT;
int bind_sp;

enum {
    OP_CONST, OP_NIL, OP_T, OP_FIX, OP_LREF, OP_LSET, OP_GREF, OP_GSET, OP_GFUNC,
    OP_CENV, OP_ENVREF, OP_ENVSET, OP_MKENV, OP_MKCLOSURE, OP_POP, OP_DUP,
    OP_JMP, OP_JMPNIL, OP_JMPT, OP_JMPNIL_KEEP, OP_JMPT_KEEP,
    OP_CALL, OP_TAILCALL, OP_RET, OP_OPTARG,
    OP_CATCH, OP_UWP, OP_HANDLER, OP_UNCATCH, OP_UNPROTECT, OP_END_UWP, OP_THROW,
    OP_BIND, OP_UNBIND,
    OP_CAR, OP_CDR, OP_CONS, OP_EQ, OP_NOT, OP_CONSP,
    OP_ADD, OP_SUB, OP_LT, OP_GT, OP_NUMEQ, OP_LE, OP_GE, OP_ADD1, OP_SUB1,
    OP_MVLIST,
    OP_COUNT
};

const opinfo_t opcodes[] = {
    { "CONST", "k" }, { "PUSHNIL", "" }, { "PUSHT", "" }, { "FIX", "b" },
    { "LREF", "b" }, { "LSET", "b" }, { "GREF", "k" }, { "GSET", "k" }, { "GFUNC", "k" },
    { "CENV", "" }, { "ENVREF", "bb" }, { "ENVSET", "bb" }, { "MKENV", "b" },
    { "MKCLOSURE", "k" }, { "POP", "" }, { "DUP", "" },
    { "JMP", "s" }, { "JMPNIL", "s" }, { "JMPT", "s" }, { "JMPNIL-KEEP", "s" }, { "JMPT-KEEP", "s" },
    { "CALL", "b" }, { "TAILCALL", "b" }, { "RET", "" }, { "OPTARG", "bs" },
    { "CATCH", "s" }, { "UWP", "s" }, { "HANDLER", "s" }, { "UNCATCH", "" },
    { "UNPROTECT", "" }, { "END-UWP", "" }, { "THROW", "" },
    { "BIND", "k" }, { "UNBIND", "b" },
    { "CAR", "" }, { "CDR", "" }, { "CONS", "" }, { "EQ", "" }, { "NOT", "" }, { "CONSP", "" },
    { "ADD", "" }, { "SUB", "" }, { "LT", "" }, { "GT", "" }, { "NUMEQ", "" }, { "LE", "" },
    { "GE", "" }, { "ADD1", "" }, { "SUB1", "" },
    { "MVLIST", "" },
};
const int nopcodes = sizeof opcodes / sizeof opcodes[0];
_Static_assert(sizeof opcodes / sizeof opcodes[0] == OP_COUNT, "opcode table out of sync");

enum { REC_KIND, REC_TAG, REC_PC, REC_FP, REC_BIND, REC_LINK, REC_SIZE };
enum { K_CATCH = 1, K_UWP, K_HANDLER };

#define ST lisp_stack
#define SP lisp_sp

/* ---- special bindings ------------------------------------------------------ */

static void bind(obj sym, obj val)
{
    if (bind_sp >= 3 * BIND_MAX) { lisp_error("Too many special bindings", UNBOUND); return; }
    bind_stack[bind_sp++] = sym;
    bind_stack[bind_sp++] = SYM(sym, SYM_VALUE);
    bind_stack[bind_sp++] = MAKE_FIX(lisp_sp);
    SYM(sym, SYM_VALUE) = val;
}

static void unbind_to(int height)
{
    while (bind_sp > height) {
        bind_sp -= 3;
        SYM(bind_stack[bind_sp], SYM_VALUE) = bind_stack[bind_sp + 1];
    }
}

/* undo the bindings made inside the frame whose function slot is at BASE: a
 * binding made in that frame was made with the stack above BASE (its locals
 * start at BASE + 3); one made by the caller just before the call is at BASE */
static void unbind_above(int base)
{
    while (bind_sp > 0 && FIX(bind_stack[bind_sp - 1]) > base) {
        bind_sp -= 3;
        SYM(bind_stack[bind_sp], SYM_VALUE) = bind_stack[bind_sp + 1];
    }
}

/* (%progv-bind symbols values): bind each symbol as a special variable, for
 * PROGV (M17); a symbol without a value is made unbound. => the height of
 * the binding stack before, for %PROGV-UNBIND. The bindings belong to the
 * caller's frame, as if it had bound them itself: they are recorded at the
 * stack height of the call, so a return from the caller undoes them too. */
obj p_progv_bind(obj *a, int n)
{
    (void)n;
    int height = bind_sp, at = (int)(a - lisp_stack) - 1;
    obj syms = a[0], vals = a[1];
    for (; CONSP(syms); syms = CDR(syms)) {
        obj sym = CAR(syms);
        if (TAG(sym) != TAG_SYMBOL) { unbind_to(height); return lisp_error("PROGV: not a symbol:", sym); }
        if (bind_sp >= 3 * BIND_MAX) { unbind_to(height); return lisp_error("Too many special bindings", UNBOUND); }
        bind_stack[bind_sp++] = sym;
        bind_stack[bind_sp++] = SYM(sym, SYM_VALUE);
        bind_stack[bind_sp++] = MAKE_FIX(at);
        SYM(sym, SYM_VALUE) = CONSP(vals) ? CAR(vals) : UNBOUND;
        vals = SCDR(vals);
    }
    return MAKE_FIX(height);
}

obj p_progv_unbind(obj *a, int n)
{
    (void)n;
    if (FIXNUMP(a[0]) && FIX(a[0]) >= 0) unbind_to((int)FIX(a[0]));
    return NIL;
}

/* ---- non-local exits --------------------------------------------------------- */

/* Transfer control to record TARGET with VALUE, running any unwind-protect
 * cleanups on the way. A cleanup runs with [target, value, 1] on the stack;
 * its END-UWP continues the unwinding. */
static void unwind_to(int target, obj value)
{
    for (int r = catch_top; r != target; r = (int)FIX(ST[r + REC_LINK])) {
        if (r < 0) lisp_fatal("vm: unwind target not on the stack");
        if (FIX(ST[r + REC_KIND]) == K_UWP) {
            catch_top = (int)FIX(ST[r + REC_LINK]);
            unbind_to((int)FIX(ST[r + REC_BIND]));
            vm_fp = (int)FIX(ST[r + REC_FP]);
            vm_pc = (int)FIX(ST[r + REC_PC]);
            SP = r;
            ST[SP++] = MAKE_FIX(target);
            ST[SP++] = value;
            ST[SP++] = MAKE_FIX(1);
            mode = M_VM;
            return;
        }
    }
    catch_top = (int)FIX(ST[target + REC_LINK]);
    unbind_to((int)FIX(ST[target + REC_BIND]));
    vm_fp = (int)FIX(ST[target + REC_FP]);
    vm_pc = (int)FIX(ST[target + REC_PC]);
    SP = target;
    ST[SP++] = value;
    mode = M_VM;
}

static int find_catch(obj tag)
{
    for (int r = catch_top; r >= 0; r = (int)FIX(ST[r + REC_LINK]))
        if (FIX(ST[r + REC_KIND]) == K_CATCH && ST[r + REC_TAG] == tag) return r;
    return -1;
}

int vm_signal(obj condition)
{
    for (int r = catch_top; r >= 0; r = (int)FIX(ST[r + REC_LINK]))
        if (FIX(ST[r + REC_KIND]) == K_HANDLER) {
            unwind_to(r, condition);
            return 1;
        }
    return 0;
}

/* Taking the special bindings in effect out of the symbols (the symbols get
 * their global values back, the binding stack keeps the bound ones), and
 * putting them back. Used to save a process or an image. */
void bindings_out(void)
{
    for (int i = bind_sp - 3; i >= 0; i -= 3) {        /* innermost first */
        obj sym = bind_stack[i], v = SYM(sym, SYM_VALUE);
        SYM(sym, SYM_VALUE) = bind_stack[i + 1];
        bind_stack[i + 1] = v;
    }
}

void bindings_in(void)
{
    for (int i = 0; i < bind_sp; i += 3) {              /* outermost first */
        obj sym = bind_stack[i], v = SYM(sym, SYM_VALUE);
        SYM(sym, SYM_VALUE) = bind_stack[i + 1];
        bind_stack[i + 1] = v;
    }
}

int vm_throw_tag(obj tag, obj value)
{
    int r = find_catch(tag);
    if (r < 0) return 0;
    unwind_to(r, value);
    return 1;
}

/* ---- leaving a frame early (the debugger) -------------------------------------------- */

int frame_request;          /* FRAME_RETURN or FRAME_REINVOKE, done by eval_run */
int frame_target;           /* the frame's fp */
obj frame_value;            /* the value, or (function . arguments); a GC root */

/* Leave the frame at FP: return VALUE from it (FRAME_RETURN), or call it
 * again, VALUE being (function . arguments) (FRAME_REINVOKE). Records inside
 * the frame are dropped; an unwind-protect cleanup runs first, with
 * [fp, value, 1 + how] on the stack, and its END-UWP comes back here. */
void vm_leave_frame(int fp, int how, obj value)
{
    int base = fp - 3;
    for (int r = catch_top; r >= base; r = (int)FIX(ST[r + REC_LINK])) {
        if (FIX(ST[r + REC_KIND]) == K_UWP) {
            catch_top = (int)FIX(ST[r + REC_LINK]);
            unbind_to((int)FIX(ST[r + REC_BIND]));
            vm_fp = (int)FIX(ST[r + REC_FP]);
            vm_pc = (int)FIX(ST[r + REC_PC]);
            SP = r;
            ST[SP++] = MAKE_FIX(fp);
            ST[SP++] = value;
            ST[SP++] = MAKE_FIX(1 + how);
            mode = M_VM;
            return;
        }
    }
    int r = catch_top;
    while (r >= base) r = (int)FIX(ST[r + REC_LINK]);
    catch_top = r;
    unbind_above(base);
    int caller_fp = (int)FIX(ST[fp - 2]), caller_pc = (int)FIX(ST[fp - 1]);
    SP = base;
    vm_fp = caller_fp;
    vm_pc = caller_pc;
    if (how == FRAME_RETURN) {
        if (caller_fp < 0) { R_val = value; mode = M_RETURN; }
        else { ST[SP++] = value; mode = M_VM; }
        return;
    }
    fixnum_t len = list_length(CDR(value));
    if (len < 0 || !room((int)len + 1)) return;
    ST[SP++] = CAR(value);
    for (obj a = CDR(value); CONSP(a); a = CDR(a)) ST[SP++] = CAR(a);
    vm_call((int)len);
}

/* is FP a frame on the stack (reachable from the running one)? */
static int frame_ok(obj x)
{
    if (!FIXNUMP(x)) return 0;
    for (int fp = vm_fp; fp >= 0; fp = (int)FIX(ST[fp - 2]))
        if (fp == FIX(x)) return 1;
    return 0;
}

#define FRAME_ARG(x) do { if (!frame_ok(x)) return lisp_error("Not a frame on this stack:", x); } while (0)

/* (%frames) => the fps of the running frames, innermost (the caller's) first */
obj p_frames(obj *a, int n)
{
    (void)a; (void)n;
    obj list = NIL, tail = NIL;
    PROTECT2(list, tail);
    for (int fp = vm_fp; fp >= 0; fp = (int)FIX(ST[fp - 2])) {
        obj cell = cons(MAKE_FIX(fp), NIL);
        if (list == NIL) list = cell; else CDR(tail) = cell;
        tail = cell;
    }
    UNPROTECT(2);
    return list;
}

obj p_frame_function(obj *a, int n)
{
    (void)n;
    FRAME_ARG(a[0]);
    return ST[FIX(a[0]) - 3];
}

/* the local slots (parameters first) */
obj p_frame_locals(obj *a, int n)
{
    (void)n;
    FRAME_ARG(a[0]);
    int fp = (int)FIX(a[0]);
    obj f = ST[fp - 3];
    int nloc = CFI_LOCALS(FIX(FIELD(f, CF_INFO)));
    obj list = NIL;
    PROTECT1(list);
    for (int i = fp + nloc - 1; i >= fp; i--) list = cons(ST[i] == UNBOUND ? NIL : ST[i], list);
    UNPROTECT(1);
    return list;
}

/* the arguments to call the frame's function with again */
obj p_frame_args(obj *a, int n)
{
    (void)n;
    FRAME_ARG(a[0]);
    int fp = (int)FIX(a[0]);
    fixnum_t info = FIX(FIELD(ST[fp - 3], CF_INFO));
    int fixed = CFI_REQ(info) + CFI_OPT(info);
    obj list = (info & CFI_REST) ? ST[fp + fixed] : NIL;
    while (fixed > CFI_REQ(info) && ST[fp + fixed - 1] == UNBOUND) fixed--;   /* optionals not given */
    PROTECT1(list);
    for (int i = fp + fixed - 1; i >= fp; i--) list = cons(ST[i], list);
    UNPROTECT(1);
    return list;
}

obj p_frame_return(obj *a, int n)
{
    (void)n;
    FRAME_ARG(a[0]);
    frame_request = FRAME_RETURN;
    frame_target = (int)FIX(a[0]);
    frame_value = a[1];
    return NIL;
}

/* (%frame-reinvoke fp function args) */
obj p_frame_reinvoke(obj *a, int n)
{
    (void)n;
    FRAME_ARG(a[0]);
    obj v = cons(a[1], a[2]);
    frame_request = FRAME_REINVOKE;
    frame_target = (int)FIX(a[0]);
    frame_value = v;
    return NIL;
}

void vm_reset(void)
{
    unbind_to(0);
    catch_top = -1;
    vm_fp = -1;
}

/* ---- calls --------------------------------------------------------------------- */

/* Enter compiled function F with N arguments at ST[base..]. */
static void enter(obj f, int n, int base)
{
    fixnum_t info = FIX(FIELD(f, CF_INFO));
    int req = CFI_REQ(info), opt = CFI_OPT(info), rest = (info & CFI_REST) != 0;
    int nloc = CFI_LOCALS(info);
    if (n < req) { lisp_error("Too few arguments to", f); return; }
    if (!rest && n > req + opt) { lisp_error("Too many arguments to", f); return; }
    if (!room(nloc + 8)) return;

    int np = req + opt + rest;
    if (rest) {
        obj list = NIL;
        PROTECT1(list);
        for (int i = SP - 1; i >= base + req + opt; i--) list = cons(ST[i], list);
        UNPROTECT(1);
        if (SP > base + req + opt) SP = base + req + opt;
        while (SP < base + req + opt) ST[SP++] = UNBOUND;
        ST[SP++] = list;
    } else {
        while (SP < base + req + opt) ST[SP++] = UNBOUND;   /* missing optionals */
    }
    for (int i = np - 1; i >= 0; i--) ST[base + 2 + i] = ST[base + i];
    ST[base] = MAKE_FIX(vm_fp);
    ST[base + 1] = MAKE_FIX(vm_pc);
    vm_fp = base + 2;
    SP = vm_fp + np;
    while (SP < vm_fp + nloc) ST[SP++] = NIL;
    vm_pc = 0;
    mode = M_VM;
}

/* Hand the call to the interpreter; its value comes back via F_VM_RESUME. */
static void via_interpreter(int base)
{
    obj list = NIL;
    PROTECT1(list);
    for (int i = SP - 1; i >= base; i--) list = cons(ST[i], list);
    UNPROTECT(1);
    R_fn = ST[base - 1];
    R_args = list;
    SP = base - 1;
    if (vm_fp >= 0) frame2(F_VM_RESUME, MAKE_FIX(vm_fp), MAKE_FIX(vm_pc));
    vm_fp = -1;
    mode = M_APPLY;
}

/* drop the slot just below the N values on top of the stack */
static void shift_down(int n)
{
    int base = SP - n;
    for (int i = 0; i < n; i++) ST[base - 1 + i] = ST[base + i];
    SP--;
}

void vm_call(int n)
{
    for (;;) {
        int base = SP - n;
        obj f = ST[base - 1];

        if (TAG(f) == TAG_SYMBOL) {
            obj g = SYM(f, SYM_FUNCTION);
            if (g == UNBOUND) { lisp_error("Undefined function", f); return; }
            ST[base - 1] = g;
            continue;
        }
        if (CFUNCP(f)) { enter(f, n, base); return; }
        if (PRIMITIVEP(f)) {
            int index = (int)FIX(FIELD(f, 0));
            if (index == PRIM_FUNCALL) {
                if (n < 1) { lisp_error("FUNCALL needs a function", UNBOUND); return; }
                shift_down(n);
                n--;
                continue;
            }
            if (index == PRIM_APPLY) {
                if (n < 1) { lisp_error("APPLY needs a function", UNBOUND); return; }
                if (n >= 2) {
                    obj list = ST[SP - 1];
                    fixnum_t len = list_length(list);
                    if (len < 0) { lisp_error("Last argument to APPLY is not a list", list); return; }
                    if (!room((int)len)) return;
                    SP--;
                    for (; CONSP(list); list = CDR(list)) ST[SP++] = CAR(list);
                    n = n - 1 + (int)len;
                }
                shift_down(n);
                n--;
                continue;
            }
            if (index == PRIM_EVAL || index == PRIM_INTERPRET) { via_interpreter(base); return; }

            const prim_t *p = &prims[index];
            if (n < p->min) { lisp_error("Too few arguments to", f); return; }
            if (p->max >= 0 && n > p->max) { lisp_error("Too many arguments to", f); return; }
            obj v = p->fn(&ST[base], n);
            SP = base - 1;
            if (vm_fp < 0) { R_val = v; mode = M_RETURN; }
            else { ST[SP++] = v; mode = M_VM; }
            return;
        }
        if (CLOSUREP(f) || OBJECTP(f, HT_MACRO)) { via_interpreter(base); return; }
        lisp_error("Not a function", f);
        return;
    }
}

void vm_resume(int fp, int pc)
{
    vm_fp = fp;
    vm_pc = pc;
    ST[SP++] = R_val;
    mode = M_VM;
}

/* ---- the interpreter loop -------------------------------------------------------- */

#define TOP     ST[SP - 1]
#define U8()    (code[vm_pc++])
#define U16()   (vm_pc += 2, (int)(code[vm_pc - 2] | code[vm_pc - 1] << 8))
#define S16()   (vm_pc += 2, (int)(int16_t)(code[vm_pc - 2] | code[vm_pc - 1] << 8))
#define PUSH(x) do { if (SP >= stack_limit) { lisp_error("Stack overflow (recursion too deep)", UNBOUND); goto out; } \
                     ST[SP++] = (x); } while (0)
#define FAIL(msg, x) do { lisp_error(msg, x); goto out; } while (0)

/* Fixnums inline; bignums, results that do not fit, and type errors go to
 * bignum.c (SLOW may allocate: the operands are read before it does, and the
 * result is stored through the stack, which the collector keeps current). */
#define ARITH2(expr, slow) do {                                                  \
        obj b = ST[SP - 1], a = ST[SP - 2];                                      \
        if (FIXNUMP(a) && FIXNUMP(b)) {                                          \
            fixnum_t x = FIX(a), y = FIX(b), r = (expr);                         \
            if (r >= FIXNUM_MIN && r <= FIXNUM_MAX) { SP--; TOP = MAKE_FIX(r); break; } \
        }                                                                        \
        obj r = slow(a, b);                                                      \
        if (lisp_err) goto out;                                                  \
        SP--;                                                                    \
        TOP = r;                                                                 \
    } while (0)

#define COMPARE2(op) do {                                                        \
        obj b = ST[SP - 1], a = ST[SP - 2];                                      \
        int c = FIXNUMP(a) && FIXNUMP(b) ? (FIX(a) > FIX(b)) - (FIX(a) < FIX(b)) : num_cmp(a, b); \
        if (lisp_err) goto out;                                                  \
        SP--;                                                                    \
        TOP = c op 0 ? S_T : NIL;                                                \
    } while (0)

static void push_record(int kind, obj tag, int pc)
{
    int r = SP;
    ST[r + REC_KIND] = MAKE_FIX(kind);
    ST[r + REC_TAG] = tag;
    ST[r + REC_PC] = MAKE_FIX(pc);
    ST[r + REC_FP] = MAKE_FIX(vm_fp);
    ST[r + REC_BIND] = MAKE_FIX(bind_sp);
    ST[r + REC_LINK] = MAKE_FIX(catch_top);
    SP += REC_SIZE;
    catch_top = r;
}

long vm_run(long budget)
{
    long n = 0;
    const uint8_t *code = 0;
    obj consts = NIL;
    uint64_t seen_gc = gc_count;
    int reload = 1;

    while (n < budget && mode == M_VM && !lisp_err && !yield_request && !abort_request && !frame_request) {
        if (reload || gc_count != seen_gc) {
            obj fn = ST[vm_fp - 3];
            code = (const uint8_t *)STR_DATA(FIELD(fn, CF_CODE));
            consts = FIELD(fn, CF_CONSTS);
            seen_gc = gc_count;
            reload = 0;
        }
        n++;
        int op = code[vm_pc++];
        if (mv_valid && op != OP_RET && op != OP_MVLIST && op != OP_JMP && op != OP_UNBIND && op != OP_UNCATCH)
            mv_valid = 0;                    /* those pass a form's values on unchanged */
        switch (op) {
        case OP_CONST: { int k = U16(); PUSH(VEC_REF(consts, k)); break; }
        case OP_NIL:   PUSH(NIL); break;
        case OP_T:     PUSH(S_T); break;
        case OP_FIX:   { int v = (int8_t)U8(); PUSH(MAKE_FIX(v)); break; }
        case OP_LREF:  { int i = U8(); PUSH(ST[vm_fp + i]); break; }
        case OP_LSET:  { int i = U8(); ST[vm_fp + i] = ST[--SP]; break; }
        case OP_GREF: {
            obj s = VEC_REF(consts, U16());
            obj v = SYM(s, SYM_VALUE);
            if (v == UNBOUND) FAIL("Unbound variable", s);
            PUSH(v);
            break;
        }
        case OP_GSET: {
            obj s = VEC_REF(consts, U16());
            if (SYM_FLAGS_OF(s) & SYMF_CONSTANT) FAIL("Cannot set a constant", s);
            SYM(s, SYM_VALUE) = ST[--SP];
            break;
        }
        case OP_GFUNC: {
            obj s = VEC_REF(consts, U16());
            obj f = SYM(s, SYM_FUNCTION);
            if (f == UNBOUND) FAIL("Undefined function", s);
            PUSH(f);
            break;
        }
        case OP_CENV: PUSH(FIELD(ST[vm_fp - 3], CF_ENV)); break;
        case OP_ENVREF: {
            int d = U8(), i = U8();
            obj e = TOP;
            while (d--) e = VEC_REF(e, 0);
            TOP = VEC_REF(e, i);
            break;
        }
        case OP_ENVSET: {
            int d = U8(), i = U8();
            obj e = ST[SP - 1], v = ST[SP - 2];
            SP -= 2;
            while (d--) e = VEC_REF(e, 0);
            VEC_REF(e, i) = v;
            break;
        }
        case OP_MKENV: {
            int k = U8();
            obj v = make_vector(k + 1, NIL);
            VEC_REF(v, 0) = TOP;
            TOP = v;
            break;
        }
        case OP_MKCLOSURE: {
            int k = U16();
            obj c = alloc_object(HT_CFUNC, CF_NFIELDS, TAG_OBJECT);
            obj t = VEC_REF(FIELD(ST[vm_fp - 3], CF_CONSTS), k);   /* re-read after allocating */
            for (int i = 0; i < CF_NFIELDS; i++) FIELD(c, i) = FIELD(t, i);
            FIELD(c, CF_ENV) = TOP;
            TOP = c;
            break;
        }
        case OP_POP: SP--; break;
        case OP_DUP: { obj t = TOP; PUSH(t); break; }

        case OP_JMP:         { int o = S16(); vm_pc += o; break; }
        case OP_JMPNIL:      { int o = S16(); if (ST[--SP] == NIL) vm_pc += o; break; }
        case OP_JMPT:        { int o = S16(); if (ST[--SP] != NIL) vm_pc += o; break; }
        case OP_JMPNIL_KEEP: { int o = S16(); if (TOP == NIL) vm_pc += o; else SP--; break; }
        case OP_JMPT_KEEP:   { int o = S16(); if (TOP != NIL) vm_pc += o; else SP--; break; }

        case OP_CALL: {
            int k = U8();
            vm_call(k);
            reload = 1;
            break;
        }
        case OP_TAILCALL: {
            int k = U8();
            int dest = vm_fp - 3, src = SP - k - 1;
            int caller_fp = (int)FIX(ST[vm_fp - 2]), caller_pc = (int)FIX(ST[vm_fp - 1]);
            for (int i = 0; i <= k; i++) ST[dest + i] = ST[src + i];
            SP = dest + k + 1;
            vm_fp = caller_fp;
            vm_pc = caller_pc;
            vm_call(k);
            reload = 1;
            break;
        }
        case OP_RET: {
            obj v = TOP;
            int caller_fp = (int)FIX(ST[vm_fp - 2]), caller_pc = (int)FIX(ST[vm_fp - 1]);
            SP = vm_fp - 3;
            vm_fp = caller_fp;
            vm_pc = caller_pc;
            if (caller_fp < 0) { R_val = v; mode = M_RETURN; }
            else ST[SP++] = v;
            reload = 1;
            break;
        }
        case OP_OPTARG: {
            int i = U8(), o = S16();
            if (ST[vm_fp + i] != UNBOUND) vm_pc += o;
            break;
        }

        case OP_CATCH:
        case OP_UWP:
        case OP_HANDLER: {
            int o = S16();
            obj tag = NIL;
            if (op == OP_CATCH) tag = ST[--SP];
            if (SP + REC_SIZE > stack_limit) FAIL("Stack overflow (recursion too deep)", UNBOUND);
            push_record(op == OP_CATCH ? K_CATCH : op == OP_UWP ? K_UWP : K_HANDLER, tag, vm_pc + o);
            break;
        }
        case OP_UNCATCH: {
            obj v = ST[--SP];
            int r = SP - REC_SIZE;
            catch_top = (int)FIX(ST[r + REC_LINK]);
            SP = r;
            ST[SP++] = v;
            break;
        }
        case OP_UNPROTECT: {
            obj v = ST[--SP];
            int r = SP - REC_SIZE;
            catch_top = (int)FIX(ST[r + REC_LINK]);
            SP = r;
            ST[SP++] = MAKE_FIX(0);
            ST[SP++] = v;
            ST[SP++] = MAKE_FIX(0);   /* normal exit */
            break;
        }
        case OP_END_UWP: {
            obj mark = ST[--SP], v = ST[--SP], target = ST[--SP];
            if (mark == MAKE_FIX(1)) { unwind_to((int)FIX(target), v); reload = 1; }
            else if (mark == MAKE_FIX(1 + FRAME_RETURN) || mark == MAKE_FIX(1 + FRAME_REINVOKE)) {
                vm_leave_frame((int)FIX(target), (int)FIX(mark) - 1, v);
                reload = 1;
            }
            else ST[SP++] = v;
            break;
        }
        case OP_THROW: {
            obj v = ST[--SP], tag = ST[--SP];
            int r = find_catch(tag);
            if (r < 0) FAIL("No catch for tag", tag);
            unwind_to(r, v);
            reload = 1;
            break;
        }
        case OP_BIND:   { obj s = VEC_REF(consts, U16()); bind(s, ST[--SP]); break; }
        case OP_UNBIND: { int k = U8(); unbind_to(bind_sp - 3 * k); break; }

        case OP_CAR:
        case OP_CDR: {
            obj x = TOP;
            if (x == NIL) break;
            if (!CONSP(x)) FAIL("Not a list:", x);
            TOP = op == OP_CAR ? CAR(x) : CDR(x);
            break;
        }
        case OP_CONS: {
            obj c = cons(ST[SP - 2], ST[SP - 1]);
            SP--;
            TOP = c;
            break;
        }
        case OP_EQ:    SP--; TOP = TOP == ST[SP] ? S_T : NIL; break;
        case OP_NOT:   TOP = TOP == NIL ? S_T : NIL; break;
        case OP_CONSP: TOP = CONSP(TOP) ? S_T : NIL; break;
        case OP_ADD:   ARITH2(x + y, num_add); break;
        case OP_SUB:   ARITH2(x - y, num_sub); break;
        case OP_LT:    COMPARE2(<); break;
        case OP_GT:    COMPARE2(>); break;
        case OP_NUMEQ: COMPARE2(==); break;
        case OP_LE:    COMPARE2(<=); break;
        case OP_GE:    COMPARE2(>=); break;
        case OP_ADD1:
        case OP_SUB1: {
            obj x = TOP;
            fixnum_t r = FIXNUMP(x) ? FIX(x) + (op == OP_ADD1 ? 1 : -1) : 0;
            if (FIXNUMP(x) && r >= FIXNUM_MIN && r <= FIXNUM_MAX) { TOP = MAKE_FIX(r); break; }
            obj v = op == OP_ADD1 ? num_add(x, MAKE_FIX(1)) : num_sub(x, MAKE_FIX(1));
            if (lisp_err) goto out;
            TOP = v;
            break;
        }
        case OP_MVLIST: {
            obj l = mv_valid ? mv_list : NIL;
            if (!mv_valid) l = cons(TOP, NIL);
            mv_valid = 0;
            mv_list = NIL;
            TOP = l;
            break;
        }
        default:
            lisp_fatal("vm: bad opcode");
        }
    }
out:
    return n;
}
