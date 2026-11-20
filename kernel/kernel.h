/* kernel.h — the C kernel's public interface.
 *
 * The kernel owns a framebuffer of "ink" values (0 = paper, 255 = full ink)
 * and an event queue. The host (host/host.js in the browser, tests/shot.c
 * natively) posts events and calls kernel_frame() once per display frame;
 * when it returns 1 the host copies kernel_present_buffer() (RGBA) to the
 * screen. Everything drawn on the screen is drawn by Lisp (world/listener.lisp)
 * through the primitives in kernel/gprims.c.
 *
 * Keep the event/key constants in sync with host/host.js.
 */
#ifndef KERNEL_H
#define KERNEL_H

#include <stdint.h>

/* The screen size is chosen by the host (window size / zoom) and can change
 * at run time; buffers are allocated for the largest size. */
#define SCREEN_MAX_W 2560
#define SCREEN_MAX_H 1600
#define SCREEN_MIN_W 480
#define SCREEN_MIN_H 360
extern int screen_w, screen_h;
#define SCREEN_W screen_w
#define SCREEN_H screen_h

#ifdef __wasm__
#define KEXPORT(name) __attribute__((export_name(name)))
#define KIMPORT(name) __attribute__((import_module("env"), import_name(name)))
#else
#define KEXPORT(name)
#define KIMPORT(name)
#endif

/* ---- events ---------------------------------------------------------- */

enum {
    EV_NONE       = 0,
    EV_MOUSE_MOVE = 1,  /* a = x, b = y (screen pixels) */
    EV_MOUSE_DOWN = 2,  /* a = button (0 L, 1 M, 2 R)  */
    EV_MOUSE_UP   = 3,  /* a = button                  */
    EV_KEY_DOWN   = 4,  /* a = key code (see below)    */
    EV_KEY_UP     = 5,  /* a = key code                */
    EV_FOCUS      = 6,
    EV_BLUR       = 7,  /* host lost focus: release buttons and modifiers */
    EV_RESIZE     = 8,  /* a = width, b = height: the screen changed size */
    EV_REDRAW     = 9,  /* the screen was lost (after a world restore): draw it again */
    EV_FILES      = 10, /* files were dropped on the page: the editor takes them (file op 8) */
};

/* Modifier bits, carried by every event. */
#define MOD_CONTROL 0x01
#define MOD_META    0x02  /* Alt / Option */
#define MOD_SUPER   0x04  /* Cmd          */
#define MOD_HYPER   0x08  /* unmapped for now */
#define MOD_SHIFT   0x10

/* Key codes: a typed character is its Unicode code; special keys are
 * above the last character, 0x10FFFF (M15). */
enum {
    KEY_RETURN  = 0x110100,
    KEY_RUBOUT  = 0x110101,  /* Backspace */
    KEY_TAB     = 0x110102,
    KEY_ALTMODE = 0x110103,  /* Escape    */
    KEY_ABORT   = 0x110104,  /* F1        */
    KEY_SELECT  = 0x110105,  /* F2        */
    KEY_RESUME  = 0x110106,  /* F3        */
    KEY_HELP    = 0x110107,  /* F4        */
    KEY_LEFT    = 0x110108,
    KEY_RIGHT   = 0x110109,
    KEY_UP      = 0x11010A,
    KEY_DOWN    = 0x11010B,
};

typedef struct {
    uint32_t type;
    int32_t  a, b;
    uint32_t mods;
} event_t;

int  event_push(const event_t *ev);   /* 0 if the queue is full */
int  event_pop(event_t *ev);          /* 0 if the queue is empty */
int  event_pending(void);

/* ---- global machine state ------------------------------------------- */

typedef struct {
    int      mouse_x, mouse_y;
    uint32_t buttons;     /* bit n = button n held */
    uint32_t mods;        /* MOD_* bits from the latest event */
    double   now_ms;      /* host clock */
    double   boot_ms;     /* host clock at first frame */
    uint32_t frames;      /* frames presented */
} kstate_t;

extern kstate_t K;

/* ---- framebuffer ----------------------------------------------------- */

#define INK_PAPER 0
#define INK_FULL  255

#define FONT_W 5
#define FONT_H 9
#define CHAR_W 6    /* advance */
#define LINE_H 11

extern uint8_t fb[SCREEN_MAX_W * SCREEN_MAX_H];   /* stride is SCREEN_W */
extern int fb_dirty;                              /* set when Lisp draws on the screen */

void fb_clear(uint8_t ink);
void fb_fill(int x, int y, int w, int h, uint8_t ink);
void fb_invert(int x, int y, int w, int h);
void fb_frame(int x, int y, int w, int h, int thickness, uint8_t ink);
void fb_checker(int x, int y, int w, int h, uint8_t ink);
void fb_pixel(int x, int y, uint8_t ink);
int  fb_char(int x, int y, int c, uint8_t ink);          /* returns new x */
int  fb_string(int x, int y, const char *s, uint8_t ink); /* returns new x */

/* ---- tiny libc replacement ------------------------------------------ */

typedef struct { char *p, *end; } kbuf_t;   /* bounded string builder */

void kb_init(kbuf_t *b, char *buf, int size);
void kb_char(kbuf_t *b, char c);
void kb_str(kbuf_t *b, const char *s);
void kb_int(kbuf_t *b, long v, int min_digits);
int  k_strlen(const char *s);
void k_log(const char *s);                  /* goes to the host console */

/* ---- exports ---------------------------------------------------------- */

void      kernel_init(void);
int       kernel_post_event(uint32_t type, int32_t a, int32_t b, uint32_t mods);
int       kernel_frame(double now_ms);
uint32_t *kernel_present_buffer(void);
int       kernel_screen_width(void);
int       kernel_screen_height(void);
void      kernel_resize(int w, int h);

#endif
