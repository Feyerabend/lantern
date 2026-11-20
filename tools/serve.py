#!/usr/bin/env python3
"""The development server: the machine's page, and its source files.

usage: serve.py PORT WEB-DIR SOURCE-DIR

  GET  /...             files from WEB-DIR (the page, host.js, kernel.wasm)
  GET  /files/          the names of the *.lisp files in SOURCE-DIR, one per line
  GET  /files/NAME      a file's bytes
  PUT  /files/NAME      replace a file's bytes (the file must exist)

The editor in the machine (world/editor.lisp) uses /files/ to read and save
world/*.lisp. To keep that private, the server listens on 127.0.0.1 only,
answers only requests addressed to localhost (no DNS rebinding), refuses
writes from other origins, and only touches plain *.lisp names in SOURCE-DIR.
"""
import functools
import http.server
import os
import re
import sys
import tempfile

NAME = re.compile(r"^[a-z0-9][a-z0-9_-]*\.lisp$")
MAX_BYTES = 1 << 20


class Handler(http.server.SimpleHTTPRequestHandler):
    source = "."
    port = 0

    def end_headers(self):
        # a development server: the browser must always check for a newer build
        if not self.path.startswith("/files/"):
            self.send_header("Cache-Control", "no-cache")
        super().end_headers()

    def local_request(self):
        host = self.headers.get("Host", "")
        allowed = {f"localhost:{self.port}", f"127.0.0.1:{self.port}"}
        if host not in allowed:
            return False
        origin = self.headers.get("Origin")
        return origin is None or origin in {f"http://{h}" for h in allowed}

    def reply(self, code, body=b"", kind="text/plain; charset=utf-8"):
        self.send_response(code)
        self.send_header("Content-Type", kind)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def file_name(self):
        name = self.path[len("/files/"):].split("?")[0]
        return name if NAME.match(name) else None

    def do_GET(self):
        if not self.path.startswith("/files/"):
            return super().do_GET()
        if not self.local_request():
            return self.reply(403, b"not a local request")
        if self.path.split("?")[0] == "/files/":
            names = sorted(n for n in os.listdir(self.source) if NAME.match(n))
            return self.reply(200, "".join(n + "\n" for n in names).encode())
        name = self.file_name()
        path = name and os.path.join(self.source, name)
        if not path or not os.path.isfile(path):
            return self.reply(404, b"no such file")
        with open(path, "rb") as f:
            self.reply(200, f.read(), "application/octet-stream")

    def do_PUT(self):
        if not self.path.startswith("/files/"):
            return self.reply(405, b"read only")
        if not self.local_request():
            return self.reply(403, b"not a local request")
        name = self.file_name()
        path = name and os.path.join(self.source, name)
        if not path or not os.path.isfile(path):
            return self.reply(404, b"only existing world files can be written")
        length = int(self.headers.get("Content-Length", "-1"))
        if length < 0 or length > MAX_BYTES:
            return self.reply(413, b"too large")
        data = self.rfile.read(length)
        fd, tmp = tempfile.mkstemp(dir=self.source, prefix=".save-")
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        os.chmod(tmp, os.stat(path).st_mode & 0o777)
        os.replace(tmp, path)                     # all or nothing
        self.log_message("wrote %s (%d bytes)", path, len(data))
        self.reply(200, b"ok")


def main():
    port, web, source = int(sys.argv[1]), sys.argv[2], sys.argv[3]
    Handler.source = os.path.abspath(source)
    Handler.port = port
    handler = functools.partial(Handler, directory=web)
    server = http.server.ThreadingHTTPServer(("127.0.0.1", port), handler)
    print(f"serving http://localhost:{port}/ (source files: {Handler.source})")
    server.serve_forever()


if __name__ == "__main__":
    main()
