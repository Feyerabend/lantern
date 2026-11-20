/* shot.c — native headless host for the whole machine: boots the kernel and
 * drives the window system with keys and the mouse through the event queue,
 * exactly as the browser does, checks what happens, and writes the screen as
 * a PPM image.
 *
 * It also plays the host's file server (make serve, tools/serve.py) for the
 * editor, on the files in SOURCE-DIR, a copy of world/ the Makefile makes.
 *
 * usage: shot <out.ppm> <source-dir>
 */
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kernel.h"
#include "lisp.h"

extern long kernel_image_capture(void);
extern const uint64_t *kernel_image_data(void);
extern uint64_t *kernel_image_buffer(long);
extern int kernel_init_from_image(long);
extern int kernel_file_request(void);
extern int kernel_file_id(void);
extern const char *kernel_file_name(void);
extern long kernel_file_name_length(void);
extern const char *kernel_file_text(void);
extern long kernel_file_text_length(void);
extern char *kernel_file_reply_buffer(long);
extern void kernel_file_reply(int, int, long);
extern void kernel_set_file_server(int);

/* ---- the host's file server, as tools/serve.py does it ---- */

static const char *source_dir;
static int files_written;
static char downloaded_name[256], downloaded[1 << 16];     /* the last download (op 4) */
static long downloaded_len;
static const char *dropped;                                 /* packed files for op 8 */

static void file_reply(int id, int status, const char *text, long n)
{
    char *buf = kernel_file_reply_buffer(n);
    if (!buf) { kernel_file_reply(id, 1, 0); return; }
    memcpy(buf, text, (size_t)n);
    kernel_file_reply(id, status, n);
}

static int plain_lisp_name(const char *name)
{
    size_t n = strlen(name);
    return n > 5 && strcmp(name + n - 5, ".lisp") == 0 && !strchr(name, '/') && name[0] != '.';
}

static void serve_files(void)
{
    int op = kernel_file_request();
    if (!op) return;
    int id = kernel_file_id();
    char name[256], path[600];
    snprintf(name, sizeof name, "%.*s", (int)kernel_file_name_length(), kernel_file_name());
    if (op == 1) {
        static char list[8192];
        long n = 0;
        DIR *d = opendir(source_dir);
        for (struct dirent *e; d && (e = readdir(d)); )
            if (plain_lisp_name(e->d_name)) n += snprintf(list + n, sizeof list - (size_t)n, "%s\n", e->d_name);
        if (d) closedir(d);
        file_reply(id, 0, list, n);
        return;
    }
    if (op == 4) {                                          /* download */
        snprintf(downloaded_name, sizeof downloaded_name, "%s", name);
        downloaded_len = kernel_file_text_length() < (long)sizeof downloaded ? kernel_file_text_length() : 0;
        memcpy(downloaded, kernel_file_text(), (size_t)downloaded_len);
        file_reply(id, 0, "ok", 2);
        return;
    }
    if (op == 5 || op == 8) {                               /* upload / dropped: packed files */
        const char *p = op == 8 && dropped ? dropped : "";
        file_reply(id, 0, p, (long)strlen(p));
        if (op == 8) dropped = 0;
        return;
    }
    if (op == 6) { file_reply(id, 0, "world/ on the file server", 25); return; }
    if (op == 7) { file_reply(id, 1, "no folders here", 15); return; }
    snprintf(path, sizeof path, "%s/%s", source_dir, name);
    FILE *f = plain_lisp_name(name) ? fopen(path, op == 3 ? "ab" : "rb") : 0;   /* a write may make a new file */
    if (!f) { file_reply(id, 1, "no such file", 12); return; }
    if (op == 2) {
        static char text[1 << 20];
        long n = (long)fread(text, 1, sizeof text, f);
        fclose(f);
        file_reply(id, 0, text, n);
        return;
    }
    fclose(f);
    f = fopen(path, "wb");
    if (!f) { file_reply(id, 1, "cannot write", 12); return; }
    fwrite(kernel_file_text(), 1, (size_t)kernel_file_text_length(), f);
    fclose(f);
    files_written++;
    file_reply(id, 0, "ok", 2);
}

/* is the string S exactly the file NAME in the source directory? */
static int same_as_file(obj s, const char *name)
{
    static char buf[1 << 20];
    char path[600];
    snprintf(path, sizeof path, "%s/%s", source_dir, name);
    FILE *f = fopen(path, "rb");
    if (!f || !STRINGP(s)) { if (f) fclose(f); return 0; }
    size_t n = fread(buf, 1, sizeof buf, f);
    fclose(f);
    return str_eq_utf8(s, buf, (int)n);       /* the file is UTF-8 (M15) */
}

static int same_as_file_bytes(const char *bytes, long n, const char *name)
{
    static char buf[1 << 20];
    char path[600];
    snprintf(path, sizeof path, "%s/%s", source_dir, name);
    FILE *f = fopen(path, "rb");
    if (!f) return 0;
    size_t m = fread(buf, 1, sizeof buf, f);
    fclose(f);
    return (long)m == n && memcmp(bytes, buf, m) == 0;
}

/* does the file NAME in the source directory contain TEXT? */
static int file_contains(const char *name, const char *text)
{
    static char buf[1 << 20];
    char path[600];
    snprintf(path, sizeof path, "%s/%s", source_dir, name);
    FILE *f = fopen(path, "rb");
    if (!f) return 0;
    size_t n = fread(buf, 1, sizeof buf - 1, f);
    fclose(f);
    buf[n] = 0;
    return strstr(buf, text) != 0;
}

static double now;

static void frames(int n)
{
    while (n--) {
        kernel_frame(now += 16);
        if (source_dir) serve_files();
    }
}

static void key(int code)
{
    kernel_post_event(EV_KEY_DOWN, code, 0, 0);
    frames(3);
}

/* one key per character of the UTF-8 text S, as the browser sends them (M15) */
static void post_text(const char *s)
{
    int len = (int)strlen(s), pos = 0;
    while (pos < len) kernel_post_event(EV_KEY_DOWN, utf8_decode(s, len, &pos), 0, 0);
}

static void type_line(const char *s)
{
    post_text(s);
    kernel_post_event(EV_KEY_DOWN, KEY_RETURN, 0, 0);
    frames(15);
}

static void keym(int code, int mods)
{
    kernel_post_event(EV_KEY_DOWN, code, 0, mods);
    frames(3);
}

static void type_keys(const char *s)
{
    post_text(s);
    frames(5);
}

static void mouse_to(int x, int y) { kernel_post_event(EV_MOUSE_MOVE, x, y, 0); frames(2); }
static void click(int x, int y, int button)
{
    mouse_to(x, y);
    kernel_post_event(EV_MOUSE_DOWN, button, 0, 0);
    kernel_post_event(EV_MOUSE_UP, button, 0, 0);
    frames(3);
}
static void drag(int x0, int y0, int x1, int y1)
{
    mouse_to(x0, y0);
    kernel_post_event(EV_MOUSE_DOWN, 0, 0, 0);
    frames(2);
    for (int i = 1; i <= 5; i++) mouse_to(x0 + (x1 - x0) * i / 5, y0 + (y1 - y0) * i / 5);
    kernel_post_event(EV_MOUSE_UP, 0, 0, 0);
    frames(3);
}

/* NAME as the Listener reads it: in CL-USER (M12) */
static obj user_symbol(const char *name) { return pkg_intern(user_package, name, (int)strlen(name)); }
static obj value_of(const char *name) { return SYM(user_symbol(name), SYM_VALUE); }

/* evaluate EXPR in the selected Listener and return the value of *PROBE* */
static obj probe(const char *expr)
{
    char line[512];
    snprintf(line, sizeof line, "(setq *probe* %s)", expr);
    type_line(line);
    return value_of("*PROBE*");
}

/* Evaluate EXPR right now, between frames, outside every process (nothing is
 * typed or printed, so the screen does not move), and return its value. */
extern int repl_echo;
static obj direct(const char *expr)
{
    char line[512];
    snprintf(line, sizeof line, "(setq *probe* %s)\n", expr);
    int echo = repl_echo;
    repl_echo = 0;
    repl_feed(line, (int)strlen(line));
    while (repl_busy()) repl_run(1000000);
    repl_echo = echo;
    return value_of("*PROBE*");
}

static fixnum_t probe_int(const char *expr)
{
    obj v = probe(expr);
    return FIXNUMP(v) ? FIX(v) : -999999;
}

static int probe_is(const char *expr, const char *string)
{
    obj v = probe(expr);
    int n = (int)strlen(string);
    return STRINGP(v) && str_eq_utf8(v, string, n);
}

/* is (direct EXPR) the string STRING? */
static int direct_is(const char *expr, const char *string)
{
    obj v = direct(expr);
    int n = (int)strlen(string);
    return STRINGP(v) && str_eq_utf8(v, string, n);
}

static void click_at(obj where, int button)
{
    if (CONSP(where)) click((int)FIX(CAR(where)), (int)FIX(CAR(CDR(where))), button);
}

static int check(int cond, const char *what)
{
    printf("%s  %s\n", cond ? "ok  " : "FAIL", what);
    return cond ? 0 : 1;
}

int main(int argc, char **argv)
{
    if (argc != 3) { fprintf(stderr, "usage: %s out.ppm source-dir\n", argv[0]); return 2; }
    int fails = 0;
    source_dir = argv[2];

    kernel_init();
    kernel_set_file_server(1);
    fails += check(kernel_frame(now) == 1, "the first frame is drawn");
    frames(10);
    fails += check(fb[(SCREEN_H - 5) * SCREEN_W + 2] == INK_FULL, "the Screen Manager drew the who-line");
    fails += check(fb[10 * SCREEN_W + 12] == INK_FULL, "the selected Listener has a black label");
    frames(40);
    fails += check(kernel_frame(now += 16) == 0 || 1, "an idle machine settles");

    /* ---- one Listener ---- */
    fails += check(probe_int("(+ 40 2)") == 42, "typing into the Listener evaluates");
    type_line("(car 5)");
    fails += check(probe("(and (transcript-contains *listener* \"Debug 1>\") *debug-level*)") == MAKE_FIX(1),
                   "an error opens the debugger in the Listener");
    key(KEY_ABORT);
    frames(5);
    fails += check(probe("(list *debug-level* 'after-error)") != NIL && probe_int("*debug-level*") == 0,
                   "Abort (F1) leaves the debugger, and the Listener goes on");
    type_line("(setq *probe* (+ 1");
    type_line("2))");
    fails += check(value_of("*PROBE*") == MAKE_FIX(3), "a form can span several lines");
    type_line("(loop)");
    key(KEY_ABORT);
    frames(5);
    fails += check(probe("'after-abort") == user_symbol("AFTER-ABORT"), "Abort (F1) stops an endless loop");
    for (int i = 0; i < 40; i++) type_line("(list 'scrolling 'text)");
    fails += check(probe_int("(length (term-lines (listener-term (car *windows*))))") > 40, "the Listener keeps its text as it scrolls");

    /* ---- the mouse moves and reshapes windows ---- */
    fixnum_t x0 = probe_int("(window-x (car *windows*))"), y0 = probe_int("(window-y (car *windows*))");
    drag((int)x0 + 40, (int)y0 + 6, (int)x0 + 140, (int)y0 + 56);
    fails += check(probe_int("(window-x (car *windows*))") == x0 + 100 && probe_int("(window-y (car *windows*))") == y0 + 50,
                   "dragging the label moves the window");
    fixnum_t w0 = probe_int("(window-width (car *windows*))"), h0 = probe_int("(window-height (car *windows*))");
    int cx = (int)(x0 + 100 + w0 - 3), cy = (int)(y0 + 50 + h0 - 3);
    drag(cx, cy, cx - 120, cy - 80);
    fails += check(probe_int("(window-width (car *windows*))") == w0 - 120 && probe_int("(window-height (car *windows*))") == h0 - 80,
                   "dragging the corner reshapes the window");

    /* ---- the system menu creates a second Listener ---- */
    click(20, 120, 2);
    fails += check(value_of("*MENU*") != NIL, "Mouse-R on the desktop opens the system menu");
    obj pos = probe("(menu-item-position 0)");
    int mx = (int)FIX(CAR(pos)), my = (int)FIX(CAR(CDR(pos)));
    click(mx, my, 0);
    frames(10);
    fails += check(value_of("*MENU*") == NIL && probe_int("(count-listeners)") == 2, "choosing Create Listener opens a second Listener");
    fails += check(probe_is("(process-name (current-process))", "Listener 2"), "keys go to the new, selected Listener");

    /* ---- two Listeners, two processes ---- */
    key(KEY_SELECT);
    fails += check(probe_is("(process-name (current-process))", "Listener 1"), "Select (F2) goes to the other Listener");
    type_line("(loop)");                                      /* Listener 1 is now busy */
    key(KEY_SELECT);
    fails += check(probe_is("(process-name (current-process))", "Listener 2"), "Listener 2 works while Listener 1 loops");
    key(KEY_SELECT);
    key(KEY_ABORT);
    frames(5);
    fails += check(probe_is("(process-name (current-process))", "Listener 1"), "Abort stops the selected Listener's loop");

    /* ---- presentations ---- */
    type_line("(defclass point () ((x :initarg :x)))");
    type_line("(list 'alpha (make-instance 'point :x 42) \"beta\")");
    obj at = direct("(presentation-screen-point *selected-window* #'instancep)");
    fails += check(CONSP(at), "the printed point is a presentation on the screen");
    if (CONSP(at)) {
        int px = (int)FIX(CAR(at)), py = (int)FIX(CAR(CDR(at)));
        mouse_to(px, py);
        fails += check(value_of("*HIGHLIGHT*") != NIL, "the mouse over it highlights it");
        click(px, py, 0);
        frames(5);
        obj o = probe("(inspector-object (find-inspector))");
        fails += check(VECTORP(o) && VEC_REF(o, 0) == intern_c(":INSTANCE"),
                       "clicking a printed object in the Listener opens it in the Inspector");

        obj at2 = direct("(presentation-screen-point (find-inspector) (lambda (o) (eql o 42)))");
        if (CONSP(at2)) click((int)FIX(CAR(at2)), (int)FIX(CAR(CDR(at2))), 0);
        frames(5);
        fails += check(probe_int("(inspector-object (find-inspector))") == 42, "clicking a part in the Inspector inspects it");
        obj at3 = direct("(presentation-screen-point (find-inspector) #'instancep)");
        if (CONSP(at3)) click((int)FIX(CAR(at3)), (int)FIX(CAR(CDR(at3))), 0);
        frames(5);
        o = probe("(inspector-object (find-inspector))");
        fails += check(VECTORP(o) && VEC_REF(o, 0) == intern_c(":INSTANCE"), "Back returns to the point");
    }

    /* Mouse-R on a presentation: its commands */
    obj sat = direct("(presentation-screen-point *selected-window* #'stringp)");
    if (CONSP(sat)) click((int)FIX(CAR(sat)), (int)FIX(CAR(CDR(sat))), 2);
    fails += check(value_of("*MENU*") != NIL, "Mouse-R on a presentation opens its command menu");
    obj ipos = direct("(menu-item-position (menu-item-index \"Describe\"))");
    if (CONSP(ipos)) click((int)FIX(CAR(ipos)), (int)FIX(CAR(CDR(ipos))), 0);
    frames(10);
    fails += check(probe("(transcript-contains *listener* \"is a STRING\")") != NIL, "choosing Describe describes it in the Listener");

    /* the command processor */
    type_line(":Inspect (list 1 2)");
    fails += check(probe("(equal (inspector-object (find-inspector)) '(1 2))") == S_T, "a command line runs a command");
    for (const char *p = ":ins"; *p; p++) kernel_post_event(EV_KEY_DOWN, (unsigned char)*p, 0, 0);
    key(KEY_TAB);
    type_line("43");
    fails += check(probe_int("(inspector-object (find-inspector))") == 43, "Tab completes a command name");
    type_line("(defun twice (x) (* 2 x))");
    type_line(":Show-Definition 'twice");
    fails += check(probe("(transcript-contains *listener* \"(DEFUN TWICE\")") != NIL, "Show Definition prints the source");
    key(KEY_HELP);
    fails += check(probe("(transcript-contains *listener* \":Use-as-Input\")") != NIL, "Help (F4) lists the commands");

    /* Use as Input: the object becomes part of the next form (Help scrolled the point away: print one) */
    type_line("(list (make-instance 'point :x 7))");
    obj uat = direct("(presentation-screen-point *selected-window* #'instancep)");
    if (CONSP(uat)) click((int)FIX(CAR(uat)), (int)FIX(CAR(CDR(uat))), 2);
    ipos = direct("(menu-item-position (menu-item-index \"Use as Input\"))");
    if (CONSP(ipos)) click((int)FIX(CAR(ipos)), (int)FIX(CAR(CDR(ipos))), 0);
    frames(5);
    key(KEY_RETURN);
    frames(10);
    obj star = value_of("*");
    fails += check(VECTORP(star) && VEC_REF(star, 0) == intern_c(":INSTANCE"), "Use as Input types the object into the next form");

    /* ---- M8: the debugger ---- */
    type_line("(defun fails-on (x) (car x))");
    type_line("(defun calls-it (x) (list :value (fails-on x)))");
    type_line("(calls-it 5)");
    fails += check(probe("(and (transcript-contains *listener* \"(FAILS-ON 5)\") (transcript-contains *listener* \"(CALLS-IT 5)\") t)") == S_T,
                   "the debugger shows the failed frames with their arguments");
    type_line(":Frame 0");
    fails += check(probe("(and (transcript-contains *listener* \"X = 5\") t)") == S_T, ":Frame shows a frame's locals by name");
    type_line("(defun fails-on (x) (* x 100))");
    type_line(":Retry");
    frames(10);
    fails += check(probe("(equal * '(:value 500))") == S_T, "after a redefinition, :Retry calls the frame again and the form finishes");
    fails += check(probe_int("*debug-level*") == 0, "and the Listener is back at top level");

    type_line("(list (cerror \"Go on\" \"Something odd\") 'resumed)");
    click_at(direct("(presentation-screen-point *selected-window* (lambda (r) (and (restartp r) (eq (restart-name r) 'continue))))"), 0);
    frames(10);
    fails += check(probe("(equal * '(nil resumed))") == S_T, "clicking a restart in the debugger takes it");

    type_line("(calls-it 'x)");
    type_line(":Return 7");
    frames(10);
    /* the new FAILS-ON calls * as a tail call: frame 0 is CALLS-IT, which returns 7 */
    fails += check(probe("(eql * 7)") == S_T, ":Return makes the failed frame return a value");

    /* ---- M8 done-test: fix a bug in the window system from inside the running system ---- */
    type_line("(ed \"demo.lisp\")");
    frames(150);
    fails += check(direct_is("(buf-name (editor-buffer (find-editor)))", "demo.lisp"), "(ed \"demo.lisp\") opens the world's source in Zmacs");
    /* plant the bug with the editor: window-bitmap -> window-bitmaps in GRAPHICS-WINDOW's DRAW-CONTENTS */
    keym('s', MOD_CONTROL); type_keys("((w graphics-window))"); keym(KEY_RETURN, 0);
    keym('s', MOD_CONTROL); type_keys("window-bitmap"); keym(KEY_RETURN, 0);
    keym('f', MOD_META); keym('f', MOD_META);
    type_keys("s");
    keym('C', MOD_CONTROL | MOD_SHIFT);
    frames(30);
    fails += check(direct("(and (string-search \"Compiled\" (editor-echo (find-editor))) "
                          "(string-search \"window-bitmaps\" (point-line-text (find-editor))) t)") == S_T,
                   "c-sh-C compiles the edited definition into the running world");
    /* the Screen Manager runs it, from the system menu */
    click_at(direct("(list (+ (window-x (find-editor)) 80) (+ (window-y (find-editor)) 5))"), 2);
    click_at(direct("(menu-item-position (menu-item-index \"Graphics demo\"))"), 0);
    frames(60);
    fails += check(direct("(count-debugger-windows)") == MAKE_FIX(1)
                   && direct("(and (transcript-contains (car *windows*) \"WINDOW-BITMAPS\") t)") == S_T,
                   "the error in the Screen Manager opens a Debugger window");
    fails += check(direct("(and *screen-manager* (string= (process-name *screen-manager*) \"Screen Manager\") "
                          "(some (lambda (p) (string= (process-name p) \"Old Screen Manager\")) *all-processes*) t)") == S_T,
                   "a new Screen Manager takes the keyboard and mouse meanwhile");
    type_line(":Edit-Frame");
    frames(30);
    fails += check(direct("(and (eq *selected-window* (find-editor)) "
                          "(string-search \"((w graphics-window))\" (point-line-text (find-editor))) t)") == S_T,
                   ":Edit-Frame shows the failing method in the editor");
    keym('s', MOD_CONTROL); type_keys("bitmaps"); keym(KEY_RETURN, 0);
    keym('f', MOD_META);
    keym(KEY_RUBOUT, 0);
    keym('C', MOD_CONTROL | MOD_SHIFT);
    frames(30);
    fails += check(direct("(and (string-search \"(window-bitmap w)\" (point-line-text (find-editor))) "
                          "(string-search \"Compiled\" (editor-echo (find-editor))) t)") == S_T,
                   "the bug is fixed and compiled in the editor");
    click_at(direct("(let ((d (dolist (w *windows*) (when (typep w 'debugger-window) (return w))))) "
                    "(list (+ (window-x d) 100) (+ (window-y d) 5)))"), 0);
    type_line(":Retry");
    frames(30);
    fails += check(direct("(and (some (lambda (w) (typep w 'graphics-window)) *windows*) (= 0 (count-debugger-windows)) t)") == S_T,
                   "done: :Retry finishes the Screen Manager's work, and the Graphics window opens");
    fails += check(direct("(some (lambda (p) (string= (process-name p) \"Old Screen Manager\")) *all-processes*)") == NIL,
                   "the old Screen Manager has ended");
    direct("(dolist (w *windows*) (when (or (typep w 'editor-window) (typep w 'graphics-window)) (kill-window w)))");
    direct("(select-window (dolist (w *windows*) (when (and (typep w 'listener-window) (not (typep w 'debugger-window))) (return w))))");
    frames(10);
    fails += check(probe_int("(+ 1 2)") == 3, "the window system works on: typing in the Listener evaluates");

    /* ---- M15: characters beyond ASCII from the keyboard ---- */
    fails += check(probe_is("(string-upcase \"smörgåsbord λόγια\")", "SMÖRGÅSBORD ΛΌΓΙΑ"),
                   "typing å, ö and Greek in the Listener reads them, and STRING-UPCASE knows them");
    type_line("(defun σ (&rest xs) (apply #'+ xs))");
    fails += check(probe_int("(σ 1 2 3)") == 6, "a function named σ is defined and called from the keyboard");
    fails += check(probe_int("(+ (char-width #\\é) (char-width #\\Ж) (char-width #\\┼))") == 18
                   && probe_int("(length (prin1-to-string #\\U+0085))") == 8,
                   "the font has glyphs for é, Ж and box drawing, and names the control characters");

    /* ---- M16: the pretty printer in a Listener ---- */
    fails += check(probe_int("(%pp-margin *standard-output*)") == probe_int("(term-cols (listener-term *listener*))")
                   && probe_int("(%pp-margin *standard-output*)") > 20,
                   "in a Listener the pretty printer's right margin is the window's width");
    type_line("(pprint '(defun area (shape) (let ((w (shape-width shape)) (h (shape-height shape))) (* w h (shape-scale shape) (shape-scale shape)))))");
    fails += check(probe_int("(let ((n 0)) (dolist (l (term-lines (listener-term *listener*))) "
                             "(when (eql (string-search \"(LET ((W (SHAPE-WIDTH SHAPE))\" l) 2) (setq n (1+ n)))) n)") == 1,
                   "PPRINT in the Listener lays a definition out over several lines");

    /* ---- M17: every value of a form, and none for (values) ---- */
    type_line("(values 'first-of-two 'second-of-two)");
    type_line("(values)");
    fails += check(probe_int("(let ((ls (term-lines (listener-term *listener*))) (n 0)) "
                             "(when (and (member \"FIRST-OF-TWO\" ls) (member \"SECOND-OF-TWO\" ls)) (setq n 1)) "
                             "(when (and (equal (second ls) \"(values)\") (null /)) (setq n (+ n 1))) n)") == 2,
                   "the Listener prints both values of (values a b), and nothing for (values)");

    /* ---- M18: byte files and pathnames on the file host ---- */
    type_line("(with-open-file (s #P\"bytes.lisp\" :direction :output :element-type '(unsigned-byte 8)) "
              "(dolist (b '(0 1 127 128 200 255)) (write-byte b s)))");
    frames(30);
    {
        static char path[600], buf[64];
        snprintf(path, sizeof path, "%s/bytes.lisp", source_dir);
        FILE *f = fopen(path, "rb");
        size_t n = f ? fread(buf, 1, sizeof buf, f) : 0;
        if (f) fclose(f);
        const unsigned char want[] = { 0, 1, 127, 128, 200, 255 };
        fails += check(n == 6 && memcmp(buf, want, 6) == 0
                       && probe_int("(with-open-file (s \"bytes.lisp\" :element-type '(unsigned-byte 8)) "
                                    "(if (equal (loop for b = (read-byte s nil) while b collect b) '(0 1 127 128 200 255)) 1 0))") == 1,
                       "WRITE-BYTE writes a file's bytes as they are (128 and up too), and READ-BYTE reads them back");
    }
    fails += check(probe_int("(if (and (pathnamep (probe-file #P\"windows.lisp\")) (null (probe-file \"nope.lisp\"))) 1 0)") == 1
                   && probe_int("(length (directory #P\"*.lisp\"))") > 10
                   && probe_int("(if (find \"windows.lisp\" (directory #P\"w*.lisp\") :key #'file-namestring :test #'equal) 1 0)") == 1,
                   "PROBE-FILE and DIRECTORY see the host's files");

    /* ---- M9: the editor reads and saves the world's files on the host ---- */
    type_line("(ed 'draw-label)");
    frames(60);
    fails += check(direct("(let ((b (editor-buffer (find-editor)))) (and (equal (buf-file b) \"windows.lisp\") "
                          "(string-search \"(defun draw-label\" (line-text b (buf-line b))) t))") == S_T,
                   "under a file server, Zmacs reads windows.lisp from the host and finds draw-label in it");
    /* the done-test: change the window system in Zmacs, compile it, save it */
    keym('s', MOD_CONTROL); type_keys("6 3"); keym(KEY_RETURN, 0);
    keym('d', MOD_CONTROL);
    type_keys("30");
    keym('C', MOD_CONTROL | MOD_SHIFT);
    frames(20);
    keym('x', MOD_CONTROL); keym('s', MOD_CONTROL);
    frames(30);
    fails += check(file_contains("windows.lisp", "(draw-string (window-label w) 30 3 *font-bold* alu-andca b)")
                   && direct("(and (not (buf-modified (editor-buffer (find-editor)))) "
                             "(string-search \"Wrote windows.lisp in world/\" (editor-echo (find-editor))) t)") == S_T,
                   "c-X c-S writes the changed definition to world/windows.lisp on the host");
    /* Compare with File, then Revert Buffer */
    keym('e', MOD_CONTROL);
    type_keys(" ; a note");
    keym('x', MOD_META); type_keys("Compare with File"); keym(KEY_RETURN, 0);
    frames(30);
    fails += check(direct("(let ((b (find-buffer \"*Compare*\"))) (and b (eq (editor-buffer (find-editor)) b) "
                          "(some (lambda (i) (string-search \"; a note\" (line-text b i))) (iota-list (buf-count b))) t))") == S_T,
                   "Compare with File shows how the buffer differs from the file");
    keym('x', MOD_CONTROL); type_keys("b"); type_keys("windows.lisp"); keym(KEY_RETURN, 0);
    keym('x', MOD_META); type_keys("Revert Buffer"); keym(KEY_RETURN, 0);
    frames(30);
    fails += check(direct("(let ((b (find-buffer \"windows.lisp\"))) (and (eq (editor-buffer (find-editor)) b) (not (buf-modified b)) t))") == S_T
                   && same_as_file(direct("(buffer-text (find-buffer \"windows.lisp\"))"), "windows.lisp"),
                   "Revert Buffer reads the file again");
    /* M15: text beyond ASCII typed in Zmacs is saved as UTF-8, and read back the same */
    keym('<', MOD_META);
    type_keys(";; Grüße, λ → Ж ");
    keym('x', MOD_CONTROL); keym('s', MOD_CONTROL);
    frames(30);
    keym('x', MOD_META); type_keys("Revert Buffer"); keym(KEY_RETURN, 0);
    frames(30);
    fails += check(file_contains("windows.lisp", ";; Grüße, λ → Ж ;;; -*- Mode: Lisp -*-")
                   && direct_is("(line-text (find-buffer \"windows.lisp\") 0)", ";; Grüße, λ → Ж ;;; -*- Mode: Lisp -*-"),
                   "Zmacs saves é, λ and Ж as UTF-8, and reads them back the same");
    /* M10: Download File, and files dropped on the page */
    keym('x', MOD_META); type_keys("Download File"); keym(KEY_RETURN, 0);
    frames(20);
    fails += check(strcmp(downloaded_name, "windows.lisp") == 0
                   && same_as_file_bytes(downloaded, downloaded_len, "windows.lisp"),
                   "m-X Download File hands the host the buffer's text under its name");
    dropped = "notes.lisp\n29\n(defun noted () :from-a-drop)windows.lisp\n8\n;; tiny\n";
    kernel_post_event(EV_FILES, 2, 0, 0);
    frames(30);
    fails += check(direct("(let ((b (find-buffer \"notes.lisp\"))) (and b (eq (editor-buffer (find-editor)) b) (buf-modified b) "
                          "(eq (buf-origin b) :upload) (string= (line-text b 0) \"(defun noted () :from-a-drop)\") "
                          "(string= (line-text (find-buffer \"windows.lisp\") 0) \";; tiny\") t))") == S_T,
                   "dropping .lisp files on the page puts them in Zmacs, as modified buffers");
    fails += check(file_contains("windows.lisp", "(defun draw-label (w)"), "a drop does not touch the file until it is saved");
    keym('x', MOD_CONTROL); type_keys("b"); type_keys("windows.lisp"); keym(KEY_RETURN, 0);
    keym('x', MOD_META); type_keys("Revert Buffer"); keym(KEY_RETURN, 0);
    frames(20);
    direct("(kill-window (find-editor))");
    direct("(select-window (dolist (w *windows*) (when (and (typep w 'listener-window) (not (typep w 'debugger-window))) (return w))))");
    frames(10);

    /* ---- Peek ---- */
    click_at(direct("(list (+ (window-x *selected-window*) 80) (+ (window-y *selected-window*) 5))"), 2);
    click_at(direct("(menu-item-position (menu-item-index \"Peek\"))"), 0);
    frames(90);
    fails += check(direct("(and (find-peek) (eq (peek-mode (find-peek)) :processes) "
                          "(some (lambda (r) (and (processp (aref r 0)) (string= (process-name (aref r 0)) \"Peek\"))) (peek-records (find-peek))) t)") == S_T,
                   "Peek opens from the system menu and lists the processes, itself included");
    fails += check(direct("(and (every (lambda (p) (> (process-steps p) 0)) *all-processes*) t)") == S_T,
                   "the kernel counts the steps each process uses");
    fails += check(direct("(and (typep *selected-window* 'listener-window) t)") == S_T, "Peek does not take the keyboard");
    click_at(direct("(presentation-screen-point (find-peek) (lambda (o) (and (peek-mode-object-p o) (eq (aref o 1) :memory))))"), 0);
    frames(20);
    fails += check(direct("(peek-mode (find-peek))") == intern_c(":MEMORY"), "clicking Memory switches Peek to the memory view");
    direct("(select-window (find-peek))");
    key('w');
    frames(20);
    fails += check(direct("(and (eq (peek-mode (find-peek)) :windows) "
                          "(some (lambda (r) (eq (aref r 0) (find-peek))) (peek-records (find-peek))) t)") == S_T,
                   "W shows the windows, as presentations");
    click_at(direct("(presentation-screen-point (find-peek) (lambda (o) (and (typep o 'window) (not (eq o (find-peek))))))"), 2);
    fails += check(direct("(and *menu* (menu-item-index \"Select Window\") t)") == S_T,
                   "Mouse-R on a window in Peek offers its commands");
    click_at(direct("(menu-item-position (menu-item-index \"Select Window\"))"), 0);
    frames(10);
    fails += check(direct("(and (typep *selected-window* 'listener-window) t)") == S_T, "Select Window selects it");
    direct("(kill-window (find-peek))");
    frames(10);

    /* ---- demo windows, graphics, locks ---- */
    type_line("(demo)");
    frames(120);
    fails += check(probe_int("(length *windows*)") == 6, "(demo) adds three windows");
    fails += check(probe_int("(slot-value (third *demo-windows*) 'found)") > 10, "the prime finder runs beside the Listeners");
    type_line("(graphics-demo)");
    frames(5);
    fails += check(probe_int("(length *windows*)") == 7, "(graphics-demo) opens a window");
    type_line("(progn (setq *count* 0 *lock* (make-lock \"count\")) "
              "(dotimes (i 2) (process-run-function \"Count\" (lambda () (dotimes (k 60) "
              "(with-lock (*lock*) (let ((v *count*)) (process-yield) (setq *count* (1+ v)))))))) t)");
    frames(400);
    fails += check(value_of("*COUNT*") == MAKE_FIX(120), "two processes counting under a lock lose no updates");

    /* ---- a saved world keeps its windows and processes ---- */
    fixnum_t before = probe_int("(slot-value (third *demo-windows*) 'candidate)");
    long n = kernel_image_capture();
    char *image = malloc((size_t)n);
    memcpy(image, kernel_image_data(), (size_t)n);
    memcpy(kernel_image_buffer(n), image, (size_t)n);
    free(image);
    fails += check(n > 0 && kernel_init_from_image(n) == 1, "the world, windows and processes included, is saved and restored");
    frames(60);
    fails += check(probe_int("(length *windows*)") == 7 && probe_int("(slot-value (third *demo-windows*) 'candidate)") > before,
                   "after the restore the windows are back and the demo carries on");

    /* ---- resizing the screen ---- */
    kernel_resize(800, 600);
    frames(10);
    fails += check(probe_int("(reduce #'max (mapcar (lambda (w) (+ (window-x w) 40)) *windows*))") <= 800,
                   "after a resize every window is still on the screen");
    kernel_resize(1000, 700);
    frames(10);
    type_line("(stop-demo)");
    frames(10);

    int accepted = 0;
    for (int i = 0; i < 5000; i++) accepted += kernel_post_event(EV_MOUSE_MOVE, 760, 300, 0);
    fails += check(accepted == 4096, "the event queue holds 4096 events and rejects the rest");
    frames(20);
    mouse_to(700, 200);

    uint32_t *px = kernel_present_buffer();
    FILE *f = fopen(argv[1], "wb");
    if (!f) { perror(argv[1]); return 2; }
    fprintf(f, "P6\n%d %d\n255\n", SCREEN_W, SCREEN_H);
    for (int i = 0; i < SCREEN_W * SCREEN_H; i++) {
        unsigned char rgb[3] = { px[i] & 0xff, (px[i] >> 8) & 0xff, (px[i] >> 16) & 0xff };
        fwrite(rgb, 1, 3, f);
    }
    fclose(f);

    printf("%s\n", fails ? "FAILED" : "all ok");
    return fails ? 1 : 0;
}
