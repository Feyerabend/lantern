#!/usr/bin/env python3
"""Make the whole machine one HTML file: the page, host.js, and the kernel.

usage: single.py index.html host.js kernel.wasm [edition title] > lisp.html

The kernel goes in as base64 (host.js decodes KERNEL_WASM instead of fetching
kernel.wasm), so the file works opened straight from disk, without a server,
or from any plain web host. The world itself is in the kernel already.
With an edition and a title (plexi Plexi), the page is that edition of the
machine, with a world of its own (host.js, LANTERN_EDITION).
"""
import base64
import html
import sys


def main():
    page_path, host_path, wasm_path = sys.argv[1:4]
    edition, title = (sys.argv[4], sys.argv[5]) if len(sys.argv) > 5 else (None, None)
    page = open(page_path, encoding="utf-8").read()
    host = open(host_path, encoding="utf-8").read()
    wasm = base64.b64encode(open(wasm_path, "rb").read()).decode("ascii")
    if "</script" in host.lower():
        sys.exit("single.py: host.js contains </script, which would end the inline script")
    tag = '<script src="host.js"></script>'
    if tag not in page:
        sys.exit(f"single.py: {page_path} has no {tag}")
    inline = f'<script>const KERNEL_WASM = "{wasm}";</script>\n<script>\n{host}</script>'
    if edition:
        if not edition.isalnum():
            sys.exit(f"single.py: bad edition {edition!r}")
        inline = f'<script>const LANTERN_EDITION = "{edition}";</script>\n' + inline
        if "<title>Lantern</title>" not in page:
            sys.exit(f"single.py: {page_path} has no <title>Lantern</title>")
        page = page.replace("<title>Lantern</title>", f"<title>{html.escape(title)}</title>")
    sys.stdout.write(page.replace(tag, inline))


if __name__ == "__main__":
    main()
