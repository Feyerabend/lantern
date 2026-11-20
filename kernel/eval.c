/* eval.c — the evaluator: an explicit-stack machine.
 *
 * Nothing here recurses on the C stack. The machine has three modes:
 *
 *   EVAL    evaluate R_expr in R_env
 *   APPLY   apply R_fn to the argument list R_args
 *   RETURN  deliver R_val to the frame on top of lisp_stack
 *
 * Continuation frames live on lisp_stack (scanned by the GC): the fields are
 * pushed first, then a fixnum frame type, so FR(0) is the type and FR(1),
 * FR(2)... are the fields. Forms in tail position (last form of a body, the
 * branches of IF, the last clause of AND/OR) are evaluated without pushing a
 * frame, so tail calls run in constant space.
 *
 * Because the machine state is just registers plus lisp_stack, a computation
 * can run for a bounded number of steps and resume later (eval_run), and
 * errors or Abort simply reset the stack.
 *
 * Language notes:
 *  - Lisp-2: (f x) uses F's function cell; #'f / FUNCTION gets it as a value.
 *  - Lexical environments are alists of (symbol . value); globals live in the
 *    symbol's value cell.
 *  - IF takes several else forms (an implicit PROGN), as in Lisp Machine Lisp.
 *  - Macros are "displacing": the call form is replaced in place by its
 *    expansion the first time it is evaluated. LET and LET* are rewritten to
 *    LAMBDA applications the same way.
 */
#include "machine.h"

obj lisp_stack[STACK_MAX];
int lisp_sp;

obj R_expr = NIL, R_env = NIL, R_val = NIL, R_fn = NIL, R_args = NIL;
obj eval_result = NIL;
obj eval_error = NIL;
obj toplevel_evaluator = NIL;
uint64_t eval_steps;

int mode;
static int active;
int yield_request, abort_request, yield_kind;
double lisp_wake_at;

int room(int n)
{
    if (lisp_sp + n > stack_limit) {
        lisp_error("Stack overflow (recursion too deep)", UNBOUND);
        return 0;
    }
    return 1;
}

void frame2(int t, obj a, obj b)
{
    if (!room(3)) return;
    lisp_stack[lisp_sp++] = b;
    lisp_stack[lisp_sp++] = a;
    lisp_stack[lisp_sp++] = MAKE_FIX(t);
}

static void frame3(int t, obj a, obj b, obj c)
{
    if (!room(4)) return;
    lisp_stack[lisp_sp++] = c;
    lisp_stack[lisp_sp++] = b;
    lisp_stack[lisp_sp++] = a;
    lisp_stack[lisp_sp++] = MAKE_FIX(t);
}

static void frame4(int t, obj a, obj b, obj c, obj d)
{
    if (!room(5)) return;
    lisp_stack[lisp_sp++] = d;
    lisp_stack[lisp_sp++] = c;
    lisp_stack[lisp_sp++] = b;
    lisp_stack[lisp_sp++] = a;
    lisp_stack[lisp_sp++] = MAKE_FIX(t);
}

static void ret(obj v) { R_val = v; mode = M_RETURN; }
static void eval_in(obj x, obj env) { R_expr = x; R_env = env; mode = M_EVAL; }

/* Evaluate the forms of BODY; the last one in tail position. */
static void eval_body(obj body, obj env)
{
    if (body == NIL) { ret(NIL); return; }
    if (!CONSP(body)) { lisp_error("Malformed body", body); return; }
    if (CDR(body) != NIL) frame2(F_PROGN, CDR(body), env);
    eval_in(CAR(body), env);
}

static obj lookup(obj sym, obj env)
{
    for (; env != NIL; env = CDR(env))
        if (CAR(CAR(env)) == sym) return CAR(env);
    return NIL;
}

static void assign(obj sym, obj val, obj env)
{
    obj b = lookup(sym, env);
    if (b != NIL) { CDR(b) = val; return; }
    if (SYM_FLAGS_OF(sym) & SYMF_CONSTANT) { lisp_error("Cannot set a constant", sym); return; }
    SYM(sym, SYM_VALUE) = val;
}

static int settable(obj sym)
{
    if (TAG(sym) != TAG_SYMBOL) { lisp_error("Not a variable", sym); return 0; }
    if (SYM_FLAGS_OF(sym) & SYMF_CONSTANT) { lisp_error("Cannot set a constant", sym); return 0; }
    return 1;
}

/* ---- lambda lists ------------------------------------------------------- */

/* Default values of &optional parameters must be constants in M1:
 * (x 10), (x 'foo), (x "s"). */
static obj optional_default(obj spec)
{
    obj d = SCAR(SCDR(spec));
    if (CONSP(d) && CAR(d) == S_QUOTE) return SCAR(CDR(d));
    if (CONSP(d) || (TAG(d) == TAG_SYMBOL && !(SYM_FLAGS_OF(d) & SYMF_CONSTANT)))
        return lisp_error("&optional default must be a constant (for now)", spec);
    return TAG(d) == TAG_SYMBOL ? SYM(d, SYM_VALUE) : d;
}

static obj bind_params(obj ll, obj args, obj env)
{
    obj var = NIL, val = NIL, cell = NIL;
    PROTECT5(ll, args, env, var, val);
    PROTECT1(cell);
    int state = 0;   /* 0 required, 1 &optional, 2 &rest, 3 after &rest var */

    for (; CONSP(ll); ll = CDR(ll)) {
        obj p = CAR(ll);
        if (p == S_OPTIONAL) { state = 1; continue; }
        if (p == S_REST || p == S_BODY) { state = 2; continue; }
        if (state == 3) { lisp_error("Junk after the &rest variable", p); goto out; }
        if (state == 2) {
            var = p;
            val = args;
            args = NIL;
            state = 3;
        } else {
            var = p;
            val = NIL;
            if (state == 1 && CONSP(p)) {
                var = CAR(p);
                val = optional_default(p);
                if (lisp_err) goto out;
            }
            if (CONSP(args)) {
                val = CAR(args);
                args = CDR(args);
            } else if (state == 0) {
                lisp_error("Too few arguments", ll);
                goto out;
            }
        }
        if (TAG(var) != TAG_SYMBOL) { lisp_error("Bad lambda-list parameter", var); goto out; }
        cell = cons(var, val);
        env = cons(cell, env);
    }
    if (args != NIL) lisp_error("Too many arguments", args);
out:
    UNPROTECT(6);
    return env;
}

/* (apply f a b '(c d)) -> (a b c d) */
static obj spread(obj args)
{
    if (args == NIL) return NIL;
    if (CDR(args) == NIL) {
        if (!LISTP(CAR(args))) return lisp_error("Last argument to APPLY is not a list", CAR(args));
        return CAR(args);
    }
    obj rest = NIL;
    PROTECT2(args, rest);
    rest = spread(CDR(args));
    rest = cons(CAR(args), rest);
    UNPROTECT(2);
    return rest;
}

/* ---- special forms --------------------------------------------------------- */

/* (let ((a 1) b) body...) => ((lambda (a b) body...) 1 nil), displaced in place. */
static void rewrite_let(obj x)
{
    obj bindings = SCAR(SCDR(x)), vars = NIL, inits = NIL, t = NIL;
    PROTECT5(x, bindings, vars, inits, t);
    for (; CONSP(bindings); bindings = CDR(bindings)) {
        obj b = CAR(bindings);
        vars = cons(CONSP(b) ? CAR(b) : b, vars);
        b = CAR(bindings);
        inits = cons(CONSP(b) ? SCAR(CDR(b)) : NIL, inits);
    }
    vars = nreverse(vars);
    inits = nreverse(inits);
    t = cons(vars, SCDR(SCDR(x)));
    t = cons(S_LAMBDA, t);
    CAR(x) = t;
    CDR(x) = inits;
    UNPROTECT(5);
}

/* (let* (b1 b2...) body...) => (let (b1) (let* (b2...) body...)) */
static void rewrite_letstar(obj x)
{
    obj bindings = SCAR(SCDR(x)), t = NIL, u = NIL;
    PROTECT4(x, bindings, t, u);
    if (CONSP(bindings) && CONSP(CDR(bindings))) {
        t = cons(CDR(bindings), SCDR(SCDR(x)));
        t = cons(S_LETSTAR, t);          /* (let* (b2...) body...) */
        t = cons(t, NIL);
        u = cons(CAR(bindings), NIL);    /* (b1) */
        CDR(x) = cons(u, t);
    }
    CAR(x) = S_LET;
    UNPROTECT(4);
}

static void special_form(int sf, obj x)
{
    obj rest = SCDR(x);
    switch (sf) {
    case SF_QUOTE:
        ret(SCAR(rest));
        return;

    case SF_FUNCTION: {
        obj f = SCAR(rest);
        if (TAG(f) == TAG_SYMBOL) {
            if (SYM(f, SYM_FUNCTION) == UNBOUND) { lisp_error("Undefined function", f); return; }
            ret(SYM(f, SYM_FUNCTION));
        } else if (CONSP(f) && CAR(f) == S_LAMBDA) {
            ret(make_closure(HT_CLOSURE, SCAR(CDR(f)), SCDR(CDR(f)), R_env, NIL));
        } else {
            lisp_error("Bad argument to FUNCTION", f);
        }
        return;
    }

    case SF_IF:
        if (!CONSP(rest)) { lisp_error("IF needs a test", x); return; }
        frame2(F_IF, SCDR(rest), R_env);
        eval_in(CAR(rest), R_env);
        return;

    case SF_PROGN:
        eval_body(rest, R_env);
        return;

    case SF_SETQ:
        if (rest == NIL) { ret(NIL); return; }
        if (!settable(SCAR(rest))) return;
        frame3(F_SETQ, CAR(rest), SCDR(SCDR(rest)), R_env);
        eval_in(SCAR(SCDR(rest)), R_env);
        return;

    case SF_LAMBDA:
        ret(make_closure(HT_CLOSURE, SCAR(rest), SCDR(rest), R_env, NIL));
        return;

    case SF_DEFUN:
    case SF_DEFMACRO: {
        if (TAG(SCAR(rest)) != TAG_SYMBOL) { lisp_error("Bad function name", SCAR(rest)); return; }
        obj f = make_closure(sf == SF_DEFUN ? HT_CLOSURE : HT_MACRO,
                             SCAR(SCDR(rest)), SCDR(SCDR(rest)), R_env, CAR(rest));
        obj name = CAR(SCDR(R_expr));    /* re-read: the allocation may have moved things */
        SYM(name, SYM_FUNCTION) = f;
        ret(name);
        return;
    }

    case SF_LET:
        rewrite_let(x);
        eval_in(R_expr, R_env);
        return;

    case SF_LETSTAR:
        rewrite_letstar(x);
        eval_in(R_expr, R_env);
        return;

    case SF_COND:
        if (rest == NIL) { ret(NIL); return; }
        if (!CONSP(CAR(rest))) { lisp_error("Bad COND clause", CAR(rest)); return; }
        frame2(F_COND, rest, R_env);
        eval_in(CAR(CAR(rest)), R_env);
        return;

    case SF_AND:
    case SF_OR:
        if (rest == NIL) { ret(sf == SF_AND ? S_T : NIL); return; }
        if (SCDR(rest) != NIL) frame2(sf == SF_AND ? F_AND : F_OR, CDR(rest), R_env);
        eval_in(CAR(rest), R_env);
        return;

    case SF_WHILE:
        frame2(F_WHILE_TEST, x, R_env);
        eval_in(SCAR(rest), R_env);
        return;
    }
    lisp_error("Unknown special form", CAR(x));
}

/* ---- machine steps ------------------------------------------------------- */

static void step_eval(void)
{
    obj x = R_expr;

    if (TAG(x) == TAG_SYMBOL) {
        obj b = lookup(x, R_env);
        if (b != NIL) { ret(CDR(b)); return; }
        obj v = SYM(x, SYM_VALUE);
        if (v == UNBOUND) { lisp_error("Unbound variable", x); return; }
        ret(v);
        return;
    }
    if (!CONSP(x)) { ret(x); return; }

    obj op = CAR(x);
    if (TAG(op) == TAG_SYMBOL) {
        int sf = (int)(SYM_FLAGS_OF(op) & SYMF_FORM_MASK);
        if (sf) { special_form(sf, x); return; }
        obj f = SYM(op, SYM_FUNCTION);
        if (f == UNBOUND) { lisp_error("Undefined function", op); return; }
        if (MACROP(f)) {
            frame2(F_MACRO, R_env, x);
            R_fn = f;
            R_args = CDR(x);
            mode = M_APPLY;
            return;
        }
        R_fn = f;
    } else if (CONSP(op) && CAR(op) == S_LAMBDA) {
        R_fn = make_closure(HT_CLOSURE, SCAR(CDR(op)), SCDR(CDR(op)), R_env, NIL);
        x = R_expr;
    } else {
        lisp_error("Illegal function", op);
        return;
    }

    obj args = CDR(x);
    if (args == NIL) { R_args = NIL; mode = M_APPLY; return; }
    if (!CONSP(args)) { lisp_error("Malformed call", x); return; }
    frame4(F_EVARGS, R_fn, CDR(args), R_env, NIL);
    eval_in(CAR(args), R_env);
}

static void call_primitive(int index)
{
    const prim_t *p = &prims[index];
    fixnum_t n = list_length(R_args);
    if (n < 0) { lisp_error("Improper argument list", R_args); return; }
    if (n < p->min) { lisp_error("Too few arguments to", R_fn); return; }
    if (p->max >= 0 && n > p->max) { lisp_error("Too many arguments to", R_fn); return; }
    if (!room((int)n)) return;
    int base = lisp_sp;
    for (obj a = R_args; a != NIL; a = CDR(a)) lisp_stack[lisp_sp++] = CAR(a);
    obj v = p->fn(&lisp_stack[base], (int)n);
    lisp_sp = base;
    ret(v);
}

static void step_apply(void)
{
    for (;;) {
        obj f = R_fn;
        if (TAG(f) == TAG_SYMBOL) {
            if (SYM(f, SYM_FUNCTION) == UNBOUND) { lisp_error("Undefined function", f); return; }
            R_fn = SYM(f, SYM_FUNCTION);
            continue;
        }
        if (PRIMITIVEP(f)) {
            int index = (int)FIX(FIELD(f, 0));
            switch (index) {
            case PRIM_FUNCALL:
                if (!CONSP(R_args)) { lisp_error("FUNCALL needs a function", UNBOUND); return; }
                R_fn = CAR(R_args);
                R_args = CDR(R_args);
                continue;
            case PRIM_APPLY:
                if (!CONSP(R_args)) { lisp_error("APPLY needs a function", UNBOUND); return; }
                R_fn = CAR(R_args);
                R_args = spread(CDR(R_args));
                if (lisp_err) return;
                continue;
            case PRIM_EVAL:
            case PRIM_INTERPRET:
                if (list_length(R_args) != 1) { lisp_error("EVAL takes one argument", R_args); return; }
                eval_in(CAR(R_args), NIL);
                return;
            }
            call_primitive(index);
            return;
        }
        if (CFUNCP(f)) {                 /* compiled: push function and args, enter the VM */
            fixnum_t n = list_length(R_args);
            if (n < 0) { lisp_error("Improper argument list", R_args); return; }
            if (!room((int)n + 1)) return;
            lisp_stack[lisp_sp++] = f;
            for (obj a = R_args; a != NIL; a = CDR(a)) lisp_stack[lisp_sp++] = CAR(a);
            vm_fp = -1;
            vm_call((int)n);
            return;
        }
        if (CLOSUREP(f) || OBJECTP(f, HT_MACRO)) {
            obj env = bind_params(FIELD(f, FN_LAMBDA_LIST), R_args, FIELD(f, FN_ENV));
            if (lisp_err) return;
            eval_body(FIELD(R_fn, FN_BODY), env);
            return;
        }
        lisp_error("Not a function", f);
        return;
    }
}

static void step_return(void)
{
    switch ((int)FIX(FR(0))) {
    case F_IF: {
        obj branches = FR(1), env = FR(2);
        POP(2);
        if (R_val != NIL) eval_in(SCAR(branches), env);
        else eval_body(SCDR(branches), env);
        return;
    }
    case F_PROGN: {
        obj rest = FR(1), env = FR(2);
        if (!CONSP(rest)) { lisp_error("Malformed body", rest); return; }
        if (CDR(rest) == NIL) POP(2);
        else FR(1) = CDR(rest);
        eval_in(CAR(rest), env);
        return;
    }
    case F_SETQ: {
        assign(FR(1), R_val, FR(3));
        obj rest = FR(2);
        if (rest == NIL) { POP(3); mode = M_RETURN; return; }
        if (!settable(SCAR(rest))) return;
        FR(1) = CAR(rest);
        FR(2) = SCDR(SCDR(rest));
        eval_in(SCAR(SCDR(rest)), FR(3));
        return;
    }
    case F_EVARGS: {
        obj acc = cons(R_val, FR(4));
        FR(4) = acc;
        obj rest = FR(2);
        if (rest == NIL) {
            R_fn = FR(1);
            R_args = nreverse(FR(4));
            POP(4);
            mode = M_APPLY;
            return;
        }
        if (!CONSP(rest)) { lisp_error("Malformed call", rest); return; }
        FR(2) = CDR(rest);
        eval_in(CAR(rest), FR(3));
        return;
    }
    case F_MACRO: {
        /* displace the call form with its expansion */
        obj x = FR(2);
        if (CONSP(R_val)) {
            CAR(x) = CAR(R_val);
            CDR(x) = CDR(R_val);
        } else {
            obj body = cons(R_val, NIL);
            x = FR(2);
            CAR(x) = S_PROGN;
            CDR(x) = body;
        }
        obj env = FR(1);
        POP(2);
        eval_in(x, env);
        return;
    }
    case F_COND: {
        obj clauses = FR(1), env = FR(2);
        if (R_val != NIL) {
            obj body = SCDR(CAR(clauses));
            POP(2);
            if (body == NIL) mode = M_RETURN;
            else eval_body(body, env);
            return;
        }
        obj rest = SCDR(clauses);
        if (rest == NIL) { POP(2); ret(NIL); return; }
        if (!CONSP(CAR(rest))) { lisp_error("Bad COND clause", CAR(rest)); return; }
        FR(1) = rest;
        eval_in(CAR(CAR(rest)), env);
        return;
    }
    case F_AND:
    case F_OR: {
        int is_and = FIX(FR(0)) == F_AND;
        obj rest = FR(1), env = FR(2);
        if (is_and ? R_val == NIL : R_val != NIL) { POP(2); mode = M_RETURN; return; }
        if (SCDR(rest) == NIL) POP(2);
        else FR(1) = CDR(rest);
        eval_in(SCAR(rest), env);
        return;
    }
    case F_WHILE_TEST: {
        obj form = FR(1), env = FR(2);
        if (R_val == NIL) { POP(2); ret(NIL); return; }
        obj body = SCDR(SCDR(form));
        if (body == NIL) { eval_in(SCAR(SCDR(form)), env); return; }
        FR(0) = MAKE_FIX(F_WHILE_BODY);
        eval_body(body, env);
        return;
    }
    case F_WHILE_BODY:
        FR(0) = MAKE_FIX(F_WHILE_TEST);
        eval_in(SCAR(SCDR(FR(1))), FR(2));
        return;
    case F_VM_RESUME: {
        int fp = (int)FIX(FR(1)), pc = (int)FIX(FR(2));
        POP(2);
        vm_resume(fp, pc);
        return;
    }
    }
    lisp_fatal("eval: bad frame");
}

/* ---- driver ---------------------------------------------------------------- */

/* Start evaluating FORM: through the toplevel evaluator (the compiler) once
 * one is installed, otherwise with the interpreter. */
void eval_start(obj form)
{
    lisp_sp = 0;
    lisp_err = 0;
    vm_reset();
    lisp_stack[lisp_sp++] = MAKE_FIX(F_TOP);
    if (toplevel_evaluator != NIL) {
        PROTECT1(form);
        R_args = cons(form, NIL);
        UNPROTECT(1);
        R_fn = toplevel_evaluator;
        mode = M_APPLY;
    } else {
        R_expr = form;
        R_env = NIL;
        mode = M_EVAL;
    }
    active = 1;
}

static void reset(void)
{
    mv_valid = 0;
    mv_list = NIL;
    frame_request = 0;
    frame_value = NIL;
    lisp_sp = 0;
    gc_nroots = 0;
    vm_reset();
    R_expr = R_env = R_val = R_fn = R_args = NIL;
    active = 0;
}

/* An error no handler took: if this process has a debugger (*DEBUGGER* is
 * not NIL, world/debugger.lisp) and is not already deep in debuggers, call
 * (%INVOKE-DEBUGGER condition) on top of the stack, so that the frames that
 * failed are still there to look at. It never returns normally: it leaves
 * through a restart, %FRAME-RETURN or %FRAME-REINVOKE. */
static int invoke_debugger(obj c)
{
    obj level = SYM(intern_c("*DEBUG-LEVEL*"), SYM_VALUE);
    if (SYM(intern_c("*DEBUGGER*"), SYM_VALUE) == NIL || !FIXNUMP(level) || FIX(level) >= 3) return 0;
    obj f = SYM(intern_c("%INVOKE-DEBUGGER"), SYM_FUNCTION);
    if (f == UNBOUND) return 0;
    if (lisp_sp + 1024 > stack_limit) stack_limit = STACK_MAX;    /* a stack overflow: use the reserve */
    if (lisp_sp + 64 > STACK_MAX) return 0;
    lisp_stack[lisp_sp++] = f;
    lisp_stack[lisp_sp++] = c;
    vm_call(1);
    return !lisp_err;
}

static int no_debugger;      /* the pending error is an Abort nobody caught */
int signal_hook_done;        /* %UNHANDLED: the handlers have had this error */

/* An error, while handlers are established (HANDLER-BIND, HANDLER-CASE:
 * *HANDLER-CLUSTERS* is not NIL): call (%SIGNAL-HOOK condition) on top of
 * the stack, as for the debugger, so that the handlers run before anything
 * unwinds. A handler that takes the condition leaves with a throw; if none
 * does, the hook ends with (%UNHANDLED condition), which signals it again,
 * past the hook, to the debugger. */
static int call_signal_hook(obj c)
{
    obj clusters = SYM(intern_c("*HANDLER-CLUSTERS*"), SYM_VALUE);
    if (clusters == NIL || clusters == UNBOUND) return 0;
    obj f = SYM(intern_c("%SIGNAL-HOOK"), SYM_FUNCTION);
    if (f == UNBOUND) return 0;
    if (lisp_sp + 1024 > stack_limit) stack_limit = STACK_MAX;    /* a stack overflow: use the reserve */
    if (lisp_sp + 64 > STACK_MAX) return 0;
    lisp_stack[lisp_sp++] = f;
    lisp_stack[lisp_sp++] = c;
    vm_call(1);
    return !lisp_err;
}

int eval_run(long budget, long *used)
{
    long n = 0;
    stack_limit = lisp_sp + 1024 > STACK_DEFAULT_LIMIT ? STACK_MAX : STACK_DEFAULT_LIMIT;
    while (n < budget) {
        if (frame_request) {
            int how = frame_request;
            obj v = frame_value;
            frame_request = 0;
            frame_value = NIL;
            gc_nroots = 0;
            vm_leave_frame(frame_target, how, v);
            continue;
        }
        if (lisp_err) {
            /* turn the error into a condition; a HANDLER record may take it */
            gc_nroots = 0;
            obj c = lisp_take_condition();
            int hooked = signal_hook_done;
            signal_hook_done = 0;
            if (!hooked && !lisp_err && call_signal_hook(c)) continue;
            if (!lisp_err && vm_signal(c)) continue;
            if (!lisp_err && !no_debugger && invoke_debugger(c)) continue;
            no_debugger = 0;
            lisp_err = 0;
            eval_error = c;
            reset();
            eval_steps += (uint64_t)n;
            *used = n;
            return RUN_ERROR;
        }
        if (abort_request) {
            /* Abort: throw to the innermost (catch :abort ...), the listener's */
            abort_request = 0;
            yield_request = 0;
            if (!vm_throw_tag(intern_c(":ABORT"), intern_c(":ABORTED"))) {
                lisp_error("Aborted", UNBOUND);
                no_debugger = 1;
            }
            continue;
        }
#ifndef __wasm__
        if (gc_nroots) lisp_fatal("eval: temporary roots leaked between steps");
#endif
        n++;
        switch (mode) {
        case M_EVAL:  step_eval(); break;
        case M_APPLY: step_apply(); break;
        case M_VM:    n += vm_run(budget - n); break;
        default:
            if (FIX(FR(0)) == F_TOP) {
                eval_result = R_val;
                reset();
                eval_steps += (uint64_t)n;
                *used = n;
                return RUN_DONE;
            }
            step_return();
        }
        if (yield_request) {
            yield_request = 0;
            eval_steps += (uint64_t)n;
            *used = n;
            return RUN_YIELD;
        }
    }
    eval_steps += (uint64_t)n;
    *used = n;
    return RUN_MORE;   /* a pending error is handled at the start of the next call */
}

void eval_abort(void) { reset(); }

/* ---- processes ------------------------------------------------------------- */

void process_start(obj p)
{
    reset();
    lisp_err = 0;
    lisp_stack[lisp_sp++] = MAKE_FIX(F_TOP);
    R_fn = VEC_REF(p, PR_FUNCTION);
    R_args = VEC_REF(p, PR_ARGS);
    mode = M_APPLY;
    active = 1;
}

static obj copy_out(obj *from, int n)
{
    obj v = make_vector(n, NIL);
    for (int i = 0; i < n; i++) VEC_REF(v, i) = from[i];
    return v;
}

void process_save(obj p)
{
    obj stack = NIL, binds = NIL, regs = NIL;
    PROTECT4(p, stack, binds, regs);
    bindings_out();                  /* the symbols get their global values back */
    stack = copy_out(lisp_stack, lisp_sp);
    binds = copy_out(bind_stack, bind_sp);
    regs = make_vector(12, NIL);
    VEC_REF(regs, 0) = MAKE_FIX(mode);
    VEC_REF(regs, 1) = R_expr;
    VEC_REF(regs, 2) = R_env;
    VEC_REF(regs, 3) = R_val;
    VEC_REF(regs, 4) = R_fn;
    VEC_REF(regs, 5) = R_args;
    VEC_REF(regs, 6) = MAKE_FIX(vm_fp);
    VEC_REF(regs, 7) = MAKE_FIX(vm_pc);
    VEC_REF(regs, 8) = MAKE_FIX(catch_top);
    VEC_REF(regs, 9) = MAKE_FIX(active);
    VEC_REF(regs, 10) = MAKE_FIX(mv_valid);
    VEC_REF(regs, 11) = mv_list;
    VEC_REF(p, PR_STACK) = stack;
    VEC_REF(p, PR_BINDS) = binds;
    VEC_REF(p, PR_REGS) = regs;
    UNPROTECT(4);
    bind_sp = 0;                     /* already out of the symbols */
    reset();
}

void process_load(obj p)
{
    obj stack = VEC_REF(p, PR_STACK), binds = VEC_REF(p, PR_BINDS), regs = VEC_REF(p, PR_REGS);
    if (!VECTORP(stack) || !VECTORP(binds) || !VECTORP(regs) || VEC_LEN(regs) != 12)
        lisp_fatal("process_load: the process has no saved state");
    lisp_sp = VEC_LEN(stack);
    for (int i = 0; i < lisp_sp; i++) lisp_stack[i] = VEC_REF(stack, i);
    bind_sp = VEC_LEN(binds);
    for (int i = 0; i < bind_sp; i++) bind_stack[i] = VEC_REF(binds, i);
    bindings_in();
    mode = (int)FIX(VEC_REF(regs, 0));
    R_expr = VEC_REF(regs, 1);
    R_env = VEC_REF(regs, 2);
    R_val = VEC_REF(regs, 3);
    R_fn = VEC_REF(regs, 4);
    R_args = VEC_REF(regs, 5);
    vm_fp = (int)FIX(VEC_REF(regs, 6));
    vm_pc = (int)FIX(VEC_REF(regs, 7));
    catch_top = (int)FIX(VEC_REF(regs, 8));
    active = (int)FIX(VEC_REF(regs, 9));
    mv_valid = (int)FIX(VEC_REF(regs, 10));
    mv_list = VEC_REF(regs, 11);
    VEC_REF(p, PR_STACK) = VEC_REF(p, PR_BINDS) = VEC_REF(p, PR_REGS) = NIL;
}
int eval_active(void) { return active; }

void eval_init(void)
{
    static obj *const roots[] = { &R_expr, &R_env, &R_val, &R_fn, &R_args, &eval_result,
                                  &eval_error, &toplevel_evaluator, &frame_value, &mv_list };
    for (unsigned i = 0; i < sizeof roots / sizeof roots[0]; i++) gc_global(roots[i]);

    static const struct { const char *name; int id; } forms[] = {
        { "QUOTE", SF_QUOTE }, { "FUNCTION", SF_FUNCTION }, { "IF", SF_IF },
        { "PROGN", SF_PROGN }, { "SETQ", SF_SETQ }, { "LAMBDA", SF_LAMBDA },
        { "DEFUN", SF_DEFUN }, { "DEFMACRO", SF_DEFMACRO }, { "LET", SF_LET },
        { "LET*", SF_LETSTAR }, { "COND", SF_COND }, { "AND", SF_AND },
        { "OR", SF_OR }, { "WHILE", SF_WHILE },
    };
    for (unsigned i = 0; i < sizeof forms / sizeof forms[0]; i++) {
        obj s = intern_c(forms[i].name);
        SYM(s, SYM_FLAGS) = MAKE_FIX(SYM_FLAGS_OF(s) | forms[i].id);
    }
}
