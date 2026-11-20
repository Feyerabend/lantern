/* fb.c — the "devices": framebuffer and event queue. */
#include "kernel.h"
#include "font5x9.h"   /* generated from fonts/5x9.txt by tools/mkfont.py */

uint8_t fb[SCREEN_MAX_W * SCREEN_MAX_H];
int screen_w = 1024, screen_h = 768;
int fb_dirty = 1;

/* ---- event ring buffer ------------------------------------------------ */

#define EVQ_SIZE 4096  /* power of two; large enough for a burst of pasted text */
static event_t evq[EVQ_SIZE];
static uint32_t evq_head, evq_tail;   /* head = next to pop, tail = next free */

int event_push(const event_t *ev)
{
    if (evq_tail - evq_head >= EVQ_SIZE) return 0;
    evq[evq_tail++ & (EVQ_SIZE - 1)] = *ev;
    return 1;
}

int event_pop(event_t *ev)
{
    if (evq_head == evq_tail) return 0;
    *ev = evq[evq_head++ & (EVQ_SIZE - 1)];
    return 1;
}

int event_pending(void) { return evq_head != evq_tail; }

/* ---- drawing (used by the kernel itself only for the boot screen) -------- */

static int clip(int *x, int *y, int *w, int *h)
{
    if (*x < 0) { *w += *x; *x = 0; }
    if (*y < 0) { *h += *y; *y = 0; }
    if (*x + *w > SCREEN_W) *w = SCREEN_W - *x;
    if (*y + *h > SCREEN_H) *h = SCREEN_H - *y;
    return *w > 0 && *h > 0;
}

void fb_clear(uint8_t ink)
{
    for (int i = 0; i < SCREEN_W * SCREEN_H; i++) fb[i] = ink;
}

void fb_fill(int x, int y, int w, int h, uint8_t ink)
{
    if (!clip(&x, &y, &w, &h)) return;
    for (int r = 0; r < h; r++) {
        uint8_t *p = fb + (y + r) * SCREEN_W + x;
        for (int c = 0; c < w; c++) p[c] = ink;
    }
}

void fb_invert(int x, int y, int w, int h)
{
    if (!clip(&x, &y, &w, &h)) return;
    for (int r = 0; r < h; r++) {
        uint8_t *p = fb + (y + r) * SCREEN_W + x;
        for (int c = 0; c < w; c++) p[c] = (uint8_t)(255 - p[c]);
    }
}

void fb_frame(int x, int y, int w, int h, int t, uint8_t ink)
{
    fb_fill(x, y, w, t, ink);
    fb_fill(x, y + h - t, w, t, ink);
    fb_fill(x, y, t, h, ink);
    fb_fill(x + w - t, y, t, h, ink);
}

/* 50% grey made of pixels; aligned to screen coordinates so adjacent
 * patches line up. */
void fb_checker(int x, int y, int w, int h, uint8_t ink)
{
    if (!clip(&x, &y, &w, &h)) return;
    for (int r = 0; r < h; r++) {
        uint8_t *p = fb + (y + r) * SCREEN_W + x;
        for (int c = 0; c < w; c++)
            p[c] = ((x + c + y + r) & 1) ? ink : INK_PAPER;
    }
}

void fb_pixel(int x, int y, uint8_t ink)
{
    if (x >= 0 && y >= 0 && x < SCREEN_W && y < SCREEN_H)
        fb[y * SCREEN_W + x] = ink;
}

int fb_char(int x, int y, int c, uint8_t ink)
{
    if (c < 32 || c > 126) c = '?';
    const uint8_t *g = font5x9[c - 32];
    for (int r = 0; r < FONT_H; r++)
        for (int col = 0; col < FONT_W; col++)
            if ((g[r] >> (FONT_W - 1 - col)) & 1)
                fb_pixel(x + col, y + r, ink);
    return x + CHAR_W;
}

int fb_string(int x, int y, const char *s, uint8_t ink)
{
    while (*s) x = fb_char(x, y, (unsigned char)*s++, ink);
    return x;
}
