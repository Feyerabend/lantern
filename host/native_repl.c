/* native_repl.c — the Lisp core in a terminal, for fast iteration and tests.
 *
 * usage: lisp [options] [file.lisp | -e EXPR | -i]...
 *
 *   --gc-every N   collect garbage every N allocations (stress test; 1 = always)
 *   --heap WORDS   limit the heap (semispace) size, e.g. to force many collections
 *   --stats        print heap and evaluator statistics at exit
 *   --world FILE   start from the world image FILE if it exists (else from
 *                  source); (disk-save) saves to FILE, (forget-world) deletes it
 *   -e EXPR        evaluate EXPR and print its value
 *   -i             interactive listener (the default when nothing else is given)
 *   file.lisp      load a file silently; errors are reported
 *
 * Exit status is 1 if any error was reported.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "lisp.h"

extern int repl_echo;
static const char *world_path;

static void out(const char *p, int n) { fwrite(p, 1, (size_t)n, stdout); }

static double now_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6;
}

static char *read_file(const char *path, long *len)
{
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    *len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc((size_t)*len + 8);   /* malloc'd memory is 8-byte aligned */
    if (!buf || fread(buf, 1, (size_t)*len, f) != (size_t)*len) { fclose(f); free(buf); return NULL; }
    fclose(f);
    return buf;
}

/* Act on (disk-save) and (forget-world) once the machine is idle. */
static void world_requests(void)
{
    if (world_forget_requested) {
        world_forget_requested = 0;
        if (world_path && remove(world_path) == 0) fprintf(stderr, "; world file %s deleted\n", world_path);
    }
    if (!world_save_requested) return;
    world_save_requested = 0;
    if (!world_path) { fprintf(stderr, "; disk-save: start with --world FILE to choose where\n"); return; }
    const uint64_t *data;
    long n = image_save(NULL, 0, &data);
    if (n <= 0) { fprintf(stderr, "; disk-save: the world could not be saved\n"); return; }
    char tmp[4096];
    snprintf(tmp, sizeof tmp, "%s.tmp", world_path);
    FILE *f = fopen(tmp, "wb");
    if (!f || fwrite(data, 1, (size_t)n, f) != (size_t)n || fclose(f) != 0 || rename(tmp, world_path) != 0) {
        perror(world_path);
        return;
    }
    fprintf(stderr, "; world saved to %s (%ld KB)\n", world_path, n / 1024);
}

static void run_all(void)
{
    while (repl_busy()) {
        lisp_now_ms = now_ms();
        repl_run(1000000);
    }
    world_requests();
    fflush(stdout);
}

static int feed_all(const char *text, long len, const char *what)
{
    repl_feed(text, (int)len);
    run_all();
    if (repl_incomplete()) {
        fprintf(stderr, "%s: input ends inside a form\n", what);
        repl_abort();
        return 1;
    }
    return 0;
}

static int load_file(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); return 1; }
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc((size_t)len + 1);
    if (!buf || fread(buf, 1, (size_t)len, f) != (size_t)len) { perror(path); fclose(f); free(buf); return 1; }
    fclose(f);
    repl_echo = 0;
    int r = feed_all(buf, len, path);
    free(buf);
    return r;
}

static void interactive(void)
{
    char line[4096];
    repl_echo = 1;
    printf("Lantern -- type expressions; end of input (Ctrl-D) exits.\n");
    for (;;) {
        fputs(repl_pending_input() ? "  " : "> ", stdout);
        fflush(stdout);
        if (!fgets(line, sizeof line, stdin)) break;
        repl_feed(line, (int)strlen(line));
        run_all();
    }
    putchar('\n');
}

int main(int argc, char **argv)
{
    int stats = 0, failures = 0, did_something = 0, i = 1;
    setvbuf(stdout, NULL, _IOLBF, 0);   /* so a fatal message is not lost in the buffer */

    for (; i < argc; i++) {
        if (!strcmp(argv[i], "--gc-every") && i + 1 < argc) gc_every = strtoull(argv[++i], 0, 10);
        else if (!strcmp(argv[i], "--heap") && i + 1 < argc) mem_set_limit(strtoull(argv[++i], 0, 10));
        else if (!strcmp(argv[i], "--stats")) stats = 1;
        else if (!strcmp(argv[i], "--gc-verify")) { extern int gc_verify; gc_verify = 1; }
        else if (!strcmp(argv[i], "--world") && i + 1 < argc) world_path = argv[++i];
        else break;
    }

    double t0 = now_ms();
    lisp_now_ms = t0;
    long image_len = 0;
    char *image = world_path ? read_file(world_path, &image_len) : NULL;
    if (image) {
        const char *extra;
        long extra_len;
        const char *why = lisp_boot_image(out, (const uint64_t *)image, image_len, &extra, &extra_len);
        if (why) fprintf(stderr, "; world %s not used: %s; booted from source\n", world_path, why);
        else fprintf(stderr, "; world restored from %s (%ld KB)\n", world_path, image_len / 1024);
        free(image);
    } else {
        lisp_boot(out);
    }
    double boot_ms = now_ms() - t0;
    /* a world saved in the browser prints to the Listener: print here instead */
    { int echo = repl_echo; repl_echo = 0; feed_all("(setq *standard-output* *host-stream*)", 38, "boot"); repl_echo = echo; }

    for (; i < argc; i++) {
        did_something = 1;
        if (!strcmp(argv[i], "-e") && i + 1 < argc) {
            repl_echo = 1;
            failures += feed_all(argv[i + 1], (long)strlen(argv[i + 1]), "-e");
            i++;
        } else if (!strcmp(argv[i], "-i")) {
            interactive();
        } else {
            failures += load_file(argv[i]);
        }
    }
    if (!did_something) interactive();

    if (stats)
        fprintf(stderr, "boot %.1f ms, total %.1f ms, %llu steps, %llu collections, heap %llu/%llu words\n",
                boot_ms, now_ms() - t0, (unsigned long long)eval_steps, (unsigned long long)gc_count,
                (unsigned long long)heap_used, (unsigned long long)heap_limit);
    return failures || repl_error_count() ? 1 : 0;
}
