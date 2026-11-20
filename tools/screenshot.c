/* screenshot.c — boot the kernel natively, type Lisp into the Listener, and
 * save the screen as a PPM, each pixel replicated SCALE times (crisp, for print).
 * Everything on the screen is drawn by the Lisp Listener.
 *
 * usage: screenshot out.ppm WIDTH HEIGHT SCALE [line...]
 * Each line is typed and followed by Return.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kernel.h"

int utf8_decode(const char *s, int len, int *pos);   /* kernel/text.c */

int main(int argc, char **argv)
{
    if (argc < 5) {
        fprintf(stderr, "usage: %s out.ppm width height scale [line...]\n", argv[0]);
        return 2;
    }
    int w = atoi(argv[2]), h = atoi(argv[3]), scale = atoi(argv[4]);
    if (scale < 1) scale = 1;

    kernel_init();
    kernel_resize(w, h);
    double t = 0;
    kernel_frame(t);

    for (int i = 5; i < argc; i++) {
        int len = (int)strlen(argv[i]), pos = 0;
        while (pos < len) {                        /* the line is UTF-8: one key per character */
            kernel_post_event(EV_KEY_DOWN, (uint32_t)utf8_decode(argv[i], len, &pos), 0, 0);
            kernel_frame(t += 10);
        }
        kernel_post_event(EV_KEY_DOWN, KEY_RETURN, 0, 0);
        for (int k = 0; k < 50; k++) kernel_frame(t += 16);   /* let it run */
    }
    kernel_post_event(EV_MOUSE_MOVE, w - 40, h - 60, 0);        /* park the mouse */
    kernel_frame(t += 600);                                     /* blinker visible */

    uint32_t *px = kernel_present_buffer();
    FILE *f = fopen(argv[1], "wb");
    if (!f) { perror(argv[1]); return 1; }
    fprintf(f, "P6\n%d %d\n255\n", SCREEN_W * scale, SCREEN_H * scale);
    for (int y = 0; y < SCREEN_H; y++)
        for (int r = 0; r < scale; r++)
            for (int x = 0; x < SCREEN_W; x++) {
                uint32_t c = px[y * SCREEN_W + x];
                unsigned char rgb[3] = { c & 0xff, (c >> 8) & 0xff, (c >> 16) & 0xff };
                for (int s = 0; s < scale; s++) fwrite(rgb, 1, 3, f);
            }
    fclose(f);
    return 0;
}
