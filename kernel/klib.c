/* klib.c — the few libc pieces the kernel needs. The wasm build has no libc. */
#include "kernel.h"

#ifdef __wasm__
/* The compiler may emit calls to these. Built with -mbulk-memory, the loops
 * below lower to memory.fill / memory.copy, so they cannot recurse. */
void *memset(void *dst, int c, __SIZE_TYPE__ n)
{
    unsigned char *d = dst;
    while (n--) *d++ = (unsigned char)c;
    return dst;
}

void *memcpy(void *dst, const void *src, __SIZE_TYPE__ n)
{
    unsigned char *d = dst;
    const unsigned char *s = src;
    while (n--) *d++ = *s++;
    return dst;
}

void *memmove(void *dst, const void *src, __SIZE_TYPE__ n)
{
    unsigned char *d = dst;
    const unsigned char *s = src;
    if (d < s) while (n--) *d++ = *s++;
    else { d += n; s += n; while (n--) *--d = *--s; }
    return dst;
}

KIMPORT("host_log") void host_log(const char *s, int len);
#else
#include <stdio.h>
static void host_log(const char *s, int len) { fprintf(stderr, "%.*s\n", len, s); }
#endif

void k_log(const char *s) { host_log(s, k_strlen(s)); }

int k_strlen(const char *s)
{
    int n = 0;
    while (s[n]) n++;
    return n;
}

void kb_init(kbuf_t *b, char *buf, int size)
{
    b->p = buf;
    b->end = buf + size - 1;
    *b->p = 0;
}

void kb_char(kbuf_t *b, char c)
{
    if (b->p < b->end) { *b->p++ = c; *b->p = 0; }
}

void kb_str(kbuf_t *b, const char *s)
{
    while (*s) kb_char(b, *s++);
}

void kb_int(kbuf_t *b, long v, int min_digits)
{
    char tmp[24];
    int n = 0;
    unsigned long u = v < 0 ? -(unsigned long)v : (unsigned long)v;
    do { tmp[n++] = (char)('0' + u % 10); u /= 10; } while (u);
    while (n < min_digits) tmp[n++] = '0';
    if (v < 0) kb_char(b, '-');
    while (n) kb_char(b, tmp[--n]);
}
