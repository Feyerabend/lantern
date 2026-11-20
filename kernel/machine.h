/* machine.h — state shared by the interpreter (eval.c) and the bytecode VM (vm.c).
 * Not part of the public interface; see lisp.h for that.
 */
#ifndef MACHINE_H
#define MACHINE_H

#include "lisp.h"

/* machine modes */
enum { M_EVAL, M_APPLY, M_RETURN, M_VM };
extern int mode;

/* interpreter registers (all GC roots) */
extern obj R_expr, R_env, R_val, R_fn, R_args;

/* interpreter continuation frames; F_VM_RESUME returns into a VM frame */
enum {
    F_TOP = 1, F_IF, F_PROGN, F_SETQ, F_EVARGS, F_MACRO, F_COND, F_AND, F_OR,
    F_WHILE_TEST, F_WHILE_BODY, F_VM_RESUME,
};

#define FR(i)  lisp_stack[lisp_sp - 1 - (i)]
#define POP(n) (lisp_sp -= (n) + 1)

int  room(int n);
void frame2(int t, obj a, obj b);

/* ---- VM ------------------------------------------------------------------ */

/* VM registers. vm_fp is the index of local 0 of the running compiled
 * function, or -1 when control belongs to the interpreter. A VM frame is:
 *
 *   fp-3  the function object
 *   fp-2  caller's fp (fixnum; -1 = return to the interpreter)
 *   fp-1  caller's pc (fixnum)
 *   fp+0  parameters, then other locals, then the operand stack
 */
extern int vm_fp, vm_pc;
extern int catch_top;

void vm_call(int nargs);          /* function and args on the stack */
long vm_run(long budget);         /* run while mode == M_VM */
void vm_resume(int fp, int pc);   /* return from the interpreter into the VM */
int  vm_signal(obj condition);    /* 1 if a handler took it */
void vm_reset(void);

/* leaving a frame early, for the debugger */
enum { FRAME_RETURN = 1, FRAME_REINVOKE = 2 };
extern int frame_request, frame_target;
extern obj frame_value;
void vm_leave_frame(int fp, int how, obj value);

/* multiple values */
extern int mv_valid;
extern obj mv_list;

#endif
