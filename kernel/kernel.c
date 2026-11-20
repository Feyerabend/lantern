/* kernel.c — the browser-facing kernel: exports, the frame loop, the mouse
 * cursor, and the machine's one process.
 *
 * After booting the world, the kernel schedules the Lisp processes in
 * *ALL-PROCESSES*; the first is the Listener, (LISTENER-TOP-LEVEL) from
 * world/listener.lisp. Abort (F1) is not queued: it makes the evaluator throw
 * to the Listener's (CATCH :ABORT).
 *
 * Everything on the screen is drawn by Lisp; the kernel only adds the mouse
 * cursor when it converts the framebuffer to RGBA for the host.
 */
#include "kernel.h"
#include "lisp.h"

kstate_t K;

#define STEPS_PER_FRAME 800000      /* about 5 ms of Lisp per frame */

/* ---- presentation ----------------------------------------------------- */

static uint32_t present[SCREEN_MAX_W * SCREEN_MAX_H];   /* RGBA, little-endian */
static int cursor_moved = 1;

/* Mouse cursor: '#' = ink, 'o' = paper outline, '.' = transparent.
 * Hotspot is the top-left pixel. */
static const char *const cursor_shape[] = {
    "#..........",
    "##.........",
    "#o#........",
    "#oo#.......",
    "#ooo#......",
    "#oooo#.....",
    "#ooooo#....",
    "#oooooo#...",
    "#ooooooo#..",
    "#oooooooo#.",
    "#ooooo#####",
    "#oo#oo#....",
    "#o#.#oo#...",
    "##..#oo#...",
    "#....#oo#..",
    ".....#oo#..",
    "......##...",
};
#define CURSOR_H ((int)(sizeof cursor_shape / sizeof cursor_shape[0]))

static inline uint32_t grey(uint8_t ink)
{
    uint32_t g = 255u - ink;
    return 0xFF000000u | (g << 16) | (g << 8) | g;
}

static void compose(void)
{
    for (int i = 0; i < SCREEN_W * SCREEN_H; i++) present[i] = grey(fb[i]);

    for (int r = 0; r < CURSOR_H; r++) {
        int y = K.mouse_y + r;
        if (y < 0 || y >= SCREEN_H) continue;
        for (int c = 0; cursor_shape[r][c]; c++) {
            int x = K.mouse_x + c;
            char ch = cursor_shape[r][c];
            if (x < 0 || x >= SCREEN_W || ch == '.') continue;
            present[y * SCREEN_W + x] = grey(ch == '#' ? INK_FULL : INK_PAPER);
        }
    }
}

/* ---- host output: Lisp's *HOST-STREAM* goes to the browser console ------ */

static char out_line[512];
static int out_len;

static void host_output(const char *p, int n)
{
    for (int i = 0; i < n; i++) {
        if (p[i] == '\n' || out_len == (int)sizeof out_line - 1) {
            out_line[out_len] = 0;
            k_log(out_line);
            out_len = 0;
            if (p[i] == '\n') continue;
        }
        out_line[out_len++] = p[i];
    }
}

/* ---- processes ------------------------------------------------------------
 *
 * The processes are the Lisp list *ALL-PROCESSES* (world/processes.lisp).
 * Each frame the scheduler gives every runnable process a slice of evaluator
 * steps, round robin, until the frame's budget is spent or nobody wants to
 * run. A process is switched in (its stack copied into the machine), run,
 * and switched out again (copied back into the heap), so between frames the
 * machine is empty and the whole state of every process is in the heap: a
 * saved world keeps its processes.
 *
 * The process with INPUT-P set (the Screen Manager, world/windows.lisp) gets
 * the keyboard and mouse events; if it dies, the kernel starts a new one.
 * Abort goes to the process in *SELECTED-PROCESS* (the selected window's). */

#define SLICE_STEPS 200000

static int abort_pending;          /* F1 pressed, not yet delivered */
static double listener_restart_at;
static uint32_t rr;                 /* where the round robin starts */

static obj processes(void) { return SYM(intern_c("*ALL-PROCESSES*"), SYM_VALUE); }

static obj nth_process(int i)
{
    obj l = processes();
    while (i-- > 0 && CONSP(l)) l = CDR(l);
    return CONSP(l) ? CAR(l) : NIL;
}

static int is_process(obj p)
{
    return VECTORP(p) && VEC_LEN(p) == PR_NFIELDS;
}

static int state_of(obj p) { return (int)FIX(VEC_REF(p, PR_STATE)); }
static void set_state(obj p, int s) { VEC_REF(p, PR_STATE) = MAKE_FIX(s); }

static obj input_process(void)
{
    for (obj l = processes(); CONSP(l); l = CDR(l))
        if (is_process(CAR(l)) && VEC_REF(CAR(l), PR_INPUT) != NIL && state_of(CAR(l)) != PS_DEAD)
            return CAR(l);
    return NIL;
}

/* make the Screen Manager process and put it first in *ALL-PROCESSES* */
static void make_listener(void)
{
    obj sym = intern_c("SYSTEM-TOP-LEVEL");
    if (SYM(sym, SYM_FUNCTION) == UNBOUND) {
        k_log("kernel: SYSTEM-TOP-LEVEL is not defined");
        listener_restart_at = 1e300;
        return;
    }
    /* every value is computed before it is stored: an allocation may move P */
    obj p = make_vector(PR_NFIELDS, NIL), v = NIL;
    PROTECT2(p, v);
    v = make_string("Screen Manager", 14); VEC_REF(p, PR_NAME) = v;
    v = make_string("Input", 5);     VEC_REF(p, PR_WHOSTATE) = v;
    v = intern_c(":PROCESS");        VEC_REF(p, PR_MARK) = v;
    v = intern_c("SYSTEM-TOP-LEVEL"); VEC_REF(p, PR_FUNCTION) = v;
    VEC_REF(p, PR_STATE) = MAKE_FIX(PS_NEW);
    VEC_REF(p, PR_WAKE) = MAKE_FIX(0);
    VEC_REF(p, PR_STEPS) = MAKE_FIX(0);
    VEC_REF(p, PR_INPUT) = S_T;
    v = cons(p, processes());
    obj all = intern_c("*ALL-PROCESSES*");
    SYM(all, SYM_VALUE) = v;
    UNPROTECT(2);
}

/* who gets Abort: the selected window's process, else the input process */
static obj abort_target(void)
{
    obj p = SYM(intern_c("*SELECTED-PROCESS*"), SYM_VALUE);
    return is_process(p) ? p : input_process();
}

static int runnable(obj p, double now)
{
    if (abort_pending && state_of(p) != PS_DEAD && p == abort_target()) return 1;
    switch (state_of(p)) {
    case PS_NEW:
    case PS_RUNNABLE: return 1;
    case PS_INPUT:
        if (VEC_REF(p, PR_INPUT) != NIL && (event_pending() || abort_pending)) return 1;
        return now >= (double)FIX(VEC_REF(p, PR_WAKE));
    case PS_SLEEP:    return now >= (double)FIX(VEC_REF(p, PR_WAKE));
    default:          return 0;
    }
}

/* Run process number I for up to BUDGET steps. Returns the steps used and
 * sets *wants_more if it was preempted (it would like more time). */
static long run_process(int i, long budget, double now, int *wants_more)
{
    obj p = nth_process(i);
    SYM(intern_c("*CURRENT-PROCESS*"), SYM_VALUE) = p;
    if (state_of(p) == PS_NEW) process_start(p);
    else process_load(p);
    set_state(p, PS_RUNNABLE);                /* what it shows while it runs */
    if (abort_pending && p == abort_target()) { abort_request = 1; abort_pending = 0; }

    long used = 0;
    int st = eval_run(budget, &used);
    abort_request = 0;

    p = nth_process(i);                       /* the collector may have moved it */
    *wants_more = 0;
    {
        obj s = VEC_REF(p, PR_STEPS);
        fixnum_t total = (FIXNUMP(s) ? FIX(s) : 0) + (used > 0 ? used : 1);
        VEC_REF(p, PR_STEPS) = MAKE_FIX(total > FIXNUM_MAX / 2 ? 0 : total);
    }
    if (state_of(p) == PS_DEAD) {             /* killed itself */
        eval_abort();
    } else if (st == RUN_YIELD) {
        set_state(p, yield_kind == YIELD_RUN ? PS_RUNNABLE : yield_kind == YIELD_INPUT ? PS_INPUT : PS_SLEEP);
        VEC_REF(p, PR_WAKE) = MAKE_FIX((fixnum_t)lisp_wake_at);
        process_save(p);
    } else if (st == RUN_MORE) {
        set_state(p, PS_RUNNABLE);
        *wants_more = 1;
        process_save(p);
    } else {                                  /* finished, or an error nobody handled */
        set_state(p, PS_DEAD);
        if (st == RUN_ERROR) {
            VEC_REF(p, PR_RESULT) = eval_error;
            lisp_report_condition(eval_error);
        } else {
            VEC_REF(p, PR_RESULT) = eval_result;
        }
        if (VEC_REF(p, PR_INPUT) != NIL) {
            k_log("kernel: the Screen Manager stopped; starting a new one");
            listener_restart_at = now + 1000;
        }
    }
    SYM(intern_c("*CURRENT-PROCESS*"), SYM_VALUE) = NIL;
    return used > 0 ? used : 1;
}

/* drop dead processes from *ALL-PROCESSES* */
static void bury_the_dead(void)
{
    obj sym = intern_c("*ALL-PROCESSES*");
    while (CONSP(SYM(sym, SYM_VALUE)) && (!is_process(CAR(SYM(sym, SYM_VALUE))) || state_of(CAR(SYM(sym, SYM_VALUE))) == PS_DEAD))
        SYM(sym, SYM_VALUE) = CDR(SYM(sym, SYM_VALUE));
    for (obj l = SYM(sym, SYM_VALUE); CONSP(l) && CONSP(CDR(l)); ) {
        obj p = CAR(CDR(l));
        if (!is_process(p) || state_of(p) == PS_DEAD) CDR(l) = CDR(CDR(l));
        else l = CDR(l);
    }
}

static void schedule(double now)
{
    if (input_process() == NIL && now >= listener_restart_at) make_listener();
    long budget = STEPS_PER_FRAME;
    int again = 1;
    while (budget > 0 && again) {
        again = 0;
        fixnum_t n = list_length(processes());
        if (n <= 0) break;
        for (fixnum_t k = 0; k < n && budget > 0; k++) {
            int i = (int)((rr + (uint32_t)k) % (uint32_t)n);
            obj p = nth_process(i);
            if (!is_process(p) || !runnable(p, now)) continue;
            int more;
            budget -= run_process(i, budget < SLICE_STEPS ? budget : SLICE_STEPS, now, &more);
            if (more) again = 1;
        }
    }
    rr++;
    bury_the_dead();
}

/* ---- exports ------------------------------------------------------------ */

static void log_init(const char *how)
{
    char buf[96];
    kbuf_t b;
    kb_init(&b, buf, sizeof buf);
    kb_str(&b, "kernel: init ");
    kb_int(&b, SCREEN_W, 0);
    kb_char(&b, 'x');
    kb_int(&b, SCREEN_H, 0);
    kb_str(&b, how);
    k_log(buf);
}

static void boot_screen(void)
{
    K.mouse_x = SCREEN_W / 2;
    K.mouse_y = SCREEN_H / 2;
    fb_clear(INK_PAPER);
    fb_dirty = 1;
}

/* Boot the world from source. */
KEXPORT("kernel_init")
void kernel_init(void)
{
    boot_screen();
    lisp_boot(host_output);
    log_init("");
}

/* ---- world images ----------------------------------------------------------- */

/* Where the host should put an image of NBYTES before kernel_init_from_image
 * (the idle semispace); 0 if it is too big. */
KEXPORT("kernel_image_buffer")
uint64_t *kernel_image_buffer(long nbytes)
{
    if (nbytes <= 0 || (uint64_t)nbytes > mem_semispace_words() * 8) return 0;
    return mem_scratch();
}

/* Boot from the image in kernel_image_buffer(). Returns 1 if it was used,
 * 0 if it was rejected and the world was booted from source instead. */
KEXPORT("kernel_init_from_image")
int kernel_init_from_image(long nbytes)
{
    boot_screen();
    const char *extra;
    long extra_len;
    const char *why = lisp_boot_image(host_output, mem_scratch(), nbytes, &extra, &extra_len);
    if (why) {
        k_log(why);
        log_init(", saved world rejected, booted from source");
        return 0;
    }
    /* the processes carry on where they were; the screen must be redrawn. Wake
     * times were on the old page's clock, which has started again: wake all */
    for (obj l = processes(); CONSP(l); l = CDR(l))
        if (is_process(CAR(l))) VEC_REF(CAR(l), PR_WAKE) = MAKE_FIX(0);
    SYM(intern_c("*WORLD-RESTORED*"), SYM_VALUE) = S_T;
    event_t ev = { EV_REDRAW, 0, 0, 0 };
    event_push(&ev);
    log_init(", world restored from image");
    return 1;
}

static const uint64_t *image_data;

/* Take an image of the world while the Listener waits for input. Returns its
 * size in bytes, 0 while a computation runs; the bytes are at
 * kernel_image_data() until the next frame. */
KEXPORT("kernel_image_capture")
long kernel_image_capture(void)
{
    return image_save(0, 0, &image_data);     /* between frames the machine is empty */
}

KEXPORT("kernel_image_data")
const uint64_t *kernel_image_data(void) { return image_data; }

/* bit 0: (disk-save) asked for a save; bit 1: (forget-world) asked to drop it */
KEXPORT("kernel_world_requests")
int kernel_world_requests(void)
{
    int r = (world_save_requested ? 1 : 0) | (world_forget_requested ? 2 : 0);
    world_save_requested = world_forget_requested = 0;
    return r;
}

/* changes whenever the Listener finishes evaluating something */
KEXPORT("kernel_world_generation")
int kernel_world_generation(void) { return (int)world_generation; }

/* Between frames every process is in the heap, so the world can be saved at
 * any time; running computations simply carry on after a restore. */
KEXPORT("kernel_busy")
int kernel_busy(void) { return 0; }

/* ---- files, through the host (M9; the Lisp side is in gprims.c) ----------------
 *
 * After a frame the host calls kernel_file_request(): 0 if nothing waits,
 * else the operation (1 list, 2 read, 3 write), with the name and text in
 * kernel_file_name()/kernel_file_text() and their lengths. When the host is
 * done it copies any reply into kernel_file_reply_buffer(n) and calls
 * kernel_file_reply(id, status, n): status 0 is success, else the reply is an
 * error message. One request at a time; the bytes are passed unchanged. */

#define FILE_MAX (1 << 20)
extern obj file_request;
extern int file_server;
static char file_name[256], file_text[FILE_MAX], file_reply[FILE_MAX];
static long file_name_len, file_text_len;
static int file_id;

static void file_answer(int id, int status, const char *text, long len)
{
    obj s = NIL, r = NIL;
    PROTECT2(s, r);
    s = make_string(text, (int)len);          /* bytes: world/editor.lisp reads them as UTF-8 */
    r = cons(MAKE_FIX(status), s);
    r = cons(MAKE_FIX(id), r);
    SYM(intern_c("*FILE-REPLY*"), SYM_VALUE) = r;
    UNPROTECT(2);
}

KEXPORT("kernel_file_request")
int kernel_file_request(void)
{
    if (file_request == NIL) return 0;
    obj v = file_request, name = VEC_REF(v, 2), text = VEC_REF(v, 3);
    int op = (int)FIX(VEC_REF(v, 1)), raw = op & 256;
    op &= 255;
    file_id = (int)FIX(VEC_REF(v, 0));
    file_request = NIL;
    /* the name and the text go to the host as UTF-8 (M15) */
    file_name_len = str_utf8(name, file_name, (int)sizeof file_name);
    if (raw && BSTRINGP(text)) {               /* bytes, as they are (M18) */
        file_text_len = STR_LEN(text);
        if (file_text_len <= FILE_MAX) memcpy(file_text, STR_DATA(text), (__SIZE_TYPE__)file_text_len);
    } else {
        file_text_len = str_utf8(text, file_text, FILE_MAX);
    }
    if (file_name_len >= (long)sizeof file_name || file_text_len > FILE_MAX) {
        file_answer(file_id, 1, "too long for the kernel's file buffer", 37);
        return 0;
    }
    return op;
}

KEXPORT("kernel_file_id") int kernel_file_id(void) { return file_id; }
KEXPORT("kernel_file_name") const char *kernel_file_name(void) { return file_name; }
KEXPORT("kernel_file_name_length") long kernel_file_name_length(void) { return file_name_len; }
KEXPORT("kernel_file_text") const char *kernel_file_text(void) { return file_text; }
KEXPORT("kernel_file_text_length") long kernel_file_text_length(void) { return file_text_len; }

KEXPORT("kernel_file_reply_buffer")
char *kernel_file_reply_buffer(long n) { return n >= 0 && n <= FILE_MAX ? file_reply : 0; }

KEXPORT("kernel_file_reply")
void kernel_file_reply(int id, int status, long n)
{
    if (n < 0 || n > FILE_MAX) { status = 1; n = 0; }
    file_answer(id, status, file_reply, n);
}

/* how the host keeps the world's source files: 1 a server (make serve), 2 the
 * browser's storage, 3 a folder the user chose; 0 none */
KEXPORT("kernel_set_file_server")
void kernel_set_file_server(int on) { file_server = on; }

KEXPORT("kernel_post_event")
int kernel_post_event(uint32_t type, int32_t a, int32_t b, uint32_t mods)
{
    K.mods = mods;
    switch (type) {
    case EV_MOUSE_MOVE:
        K.mouse_x = a < 0 ? 0 : a >= SCREEN_W ? SCREEN_W - 1 : a;
        K.mouse_y = b < 0 ? 0 : b >= SCREEN_H ? SCREEN_H - 1 : b;
        cursor_moved = 1;
        break;
    case EV_MOUSE_DOWN: K.buttons |=  (1u << (a & 3)); break;
    case EV_MOUSE_UP:   K.buttons &= ~(1u << (a & 3)); break;
    case EV_BLUR:       K.buttons = 0; K.mods = 0; break;
    case EV_KEY_DOWN:
        if (a == KEY_ABORT) { abort_pending = 1; return 1; }   /* not queued */
        break;
    case EV_KEY_UP:
        return 1;                                              /* nobody wants these */
    }
    event_t ev = { type, a, b, mods };
    return event_push(&ev);
}

KEXPORT("kernel_frame")
int kernel_frame(double now_ms)
{
    if (K.frames == 0 && K.boot_ms == 0) K.boot_ms = now_ms;
    K.now_ms = now_ms;
    lisp_now_ms = now_ms;

    schedule(now_ms);

    if (!fb_dirty && !cursor_moved) return 0;
    compose();
    fb_dirty = cursor_moved = 0;
    K.frames++;
    return 1;
}

KEXPORT("kernel_present_buffer")
uint32_t *kernel_present_buffer(void) { return present; }

KEXPORT("kernel_screen_width")
int kernel_screen_width(void) { return SCREEN_W; }

KEXPORT("kernel_screen_height")
int kernel_screen_height(void) { return SCREEN_H; }

/* The host asks for a new screen size (window size divided by zoom). The
 * Listener redraws everything when it sees the EV_RESIZE event. */
KEXPORT("kernel_resize")
void kernel_resize(int w, int h)
{
    if (w < SCREEN_MIN_W) w = SCREEN_MIN_W;
    if (h < SCREEN_MIN_H) h = SCREEN_MIN_H;
    if (w > SCREEN_MAX_W) w = SCREEN_MAX_W;
    if (h > SCREEN_MAX_H) h = SCREEN_MAX_H;
    if (w == screen_w && h == screen_h) return;
    screen_w = w;
    screen_h = h;
    if (K.mouse_x >= w) K.mouse_x = w - 1;
    if (K.mouse_y >= h) K.mouse_y = h - 1;
    fb_clear(INK_PAPER);
    fb_dirty = 1;
    event_t ev = { EV_RESIZE, w, h, 0 };
    event_push(&ev);
}
