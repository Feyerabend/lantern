# Lantern — build
#
#   make          build the browser version into build/web/
#   make lisp     build the native REPL: ./build/lisp
#   make test     run all native tests (Lisp suite, GC stress, errors, screen)
#   make stress   collect every 10 allocations and verify the heap (slow)
#   make serve    serve build/web/ on http://localhost:8742/, and world/*.lisp to the editor
#                 (build/web/lisp.html is the whole machine in one file: open it from disk)
#   make pdf      docs/A-Lisp-Machine-in-the-Browser.pdf from docs/*.md (needs Tectonic)
#   make docs/lantern.png   the README's screenshot
#   make clean

CLANG   ?= clang
WASM_LD ?= $(shell command -v wasm-ld 2>/dev/null || echo /opt/homebrew/opt/llvm/bin/wasm-ld)
PYTHON  ?= python3
PORT    ?= 8742

LISP_SRC   = kernel/mem.c kernel/obj.c kernel/bignum.c kernel/float.c kernel/package.c kernel/text.c kernel/array.c kernel/read.c kernel/print.c kernel/eval.c kernel/vm.c kernel/prims.c kernel/repl.c kernel/image.c \
             kernel/gprims.c kernel/fb.c
SCREEN_SRC = kernel/kernel.c kernel/klib.c
KERNEL_SRC = $(SCREEN_SRC) $(LISP_SRC)
HEADERS    = kernel/kernel.h kernel/lisp.h kernel/machine.h build/font5x9.h build/boot_lisp.h

# _DEFAULT_SOURCE: on Linux, -std=c11 hides POSIX functions the native build
# uses (clock_gettime, localtime_r, opendir); it changes nothing elsewhere
CFLAGS        = -std=c11 -Wall -Wextra -Werror -Ikernel -Ibuild -D_DEFAULT_SOURCE
WASM_CFLAGS   = $(CFLAGS) --target=wasm32 -O2 -ffreestanding -fno-builtin -nostdlib -mbulk-memory
NATIVE_CFLAGS = $(CFLAGS) -O1 -g -fsanitize=address,undefined
REPL_CFLAGS   = $(CFLAGS) -O2 -g
LDLIBS        = -lm          # the native build's sqrt, sin ...: macOS links them anyway, Linux needs this

# 64 MiB of linear memory: two 8 MiB semispaces, the 2 MiB Lisp stack,
# framebuffer + RGBA buffer for up to 2560x1600 (~20 MiB), a 1 MiB C stack.
WASM_LDFLAGS = --no-entry --initial-memory=67108864 --max-memory=268435456 -z stack-size=1048576

WASM_OBJ = $(patsubst kernel/%.c,build/wasm/%.o,$(KERNEL_SRC))
TESTS    = tests/lisp/check.lisp tests/lisp/core.lisp tests/lisp/control.lisp tests/lisp/objects.lisp tests/lisp/gc.lisp tests/lisp/debugger.lisp tests/lisp/cl.lisp tests/lisp/cl2.lisp tests/lisp/cl3.lisp tests/lisp/cl4.lisp tests/lisp/cl5.lisp tests/lisp/cl6.lisp tests/lisp/cl7.lisp tests/lisp/cl8.lisp
ERRORS_EXPECTED = 16

.PHONY: all web lisp test stress serve clean pdf

all: web

web: build/web/kernel.wasm build/web/index.html build/web/host.js build/web/lisp.html

# the whole machine as one file, for opening from disk or any plain web host
build/web/lisp.html: build/web/kernel.wasm host/index.html host/host.js tools/single.py
	$(PYTHON) -I tools/single.py host/index.html host/host.js build/web/kernel.wasm > $@

lisp: build/lisp

build/font5x9.h: fonts/5x9.txt tools/mkfont.py
	@mkdir -p build
	$(PYTHON) -I tools/mkfont.py $< $@ font5x9 5 9

# The world loaded at boot: the prelude, then the compiler (which compiles
# both and takes over evaluation), then streams, fonts, graphics and the
# Listener, the machine's main program. Each file is preceded by a line
# ";;;; FILE name", so that the editor can show the source as files again.
WORLD = world/boot.lisp world/compiler.lisp world/streams.lisp world/objects.lisp world/processes.lisp \
        world/conditions.lisp world/cl.lisp world/packages.lisp world/cl2.lisp world/signals.lisp world/printer.lisp world/pretty.lisp world/reader.lisp world/cl3.lisp world/cl4.lisp build/font_5x9.lisp world/graphics.lisp world/windows.lisp \
        world/listener.lisp world/presentations.lisp world/debugger.lisp world/editor.lisp world/peek.lisp world/demo.lisp

build/font_5x9.lisp: fonts/5x9.txt tools/mkfont.py
	@mkdir -p build
	$(PYTHON) -I tools/mkfont.py $< $@ '*glyphs-5x9*' 5 9

build/boot_lisp.h: $(WORLD) tools/embed.py tools/catworld.py
	@mkdir -p build
	$(PYTHON) -I tools/catworld.py $(WORLD) > build/world.lisp
	$(PYTHON) -I tools/embed.py build/world.lisp $@ boot_lisp

build/wasm/%.o: kernel/%.c $(HEADERS)
	@mkdir -p build/wasm
	$(CLANG) $(WASM_CFLAGS) -c $< -o $@

build/web/kernel.wasm: $(WASM_OBJ)
	@mkdir -p build/web
	$(WASM_LD) $(WASM_LDFLAGS) $^ -o $@

build/web/%: host/%
	@mkdir -p build/web
	cp $< $@

build/lisp: host/native_repl.c $(LISP_SRC) $(HEADERS)
	$(CLANG) $(REPL_CFLAGS) host/native_repl.c $(LISP_SRC) -o $@ $(LDLIBS)

build/lisp-asan: host/native_repl.c $(LISP_SRC) $(HEADERS)
	$(CLANG) $(NATIVE_CFLAGS) host/native_repl.c $(LISP_SRC) -o $@ $(LDLIBS)

build/shot: tests/shot.c $(KERNEL_SRC) $(HEADERS)
	$(CLANG) $(NATIVE_CFLAGS) tests/shot.c $(KERNEL_SRC) -o $@ $(LDLIBS)

test: build/lisp build/lisp-asan build/shot
	@echo "== Lisp test suite"
	./build/lisp --stats $(TESTS)
	@echo "== same, with address/UB sanitizers"
	./build/lisp-asan $(TESTS)
	@echo "== small heap (many collections)"
	./build/lisp --stats --heap 350000 tests/lisp/check.lisp tests/lisp/gc.lisp
	@echo "== GC stress: collect every 100 allocations, verifying the heap each time"
	./build/lisp --stats --gc-verify --gc-every 100 tests/lisp/check.lisp tests/lisp/core.lisp tests/lisp/control.lisp
	@echo "== errors are reported and the system stays usable"
	@./build/lisp tests/lisp/errors.lisp -e '(+ 1 2)' > build/errors.out; \
	  n=$$(grep -c '^>>Error' build/errors.out); \
	  if [ "$$n" = "$(ERRORS_EXPECTED)" ] && tail -1 build/errors.out | grep -qx 3; \
	  then echo "ok: $$n errors reported, then (+ 1 2) => 3"; \
	  else cat build/errors.out; echo "FAIL: expected $(ERRORS_EXPECTED) errors"; exit 1; fi
	@echo "== world images: save, restore, reject damaged ones"
	@rm -f build/test.world build/bad.world
	@./build/lisp --world build/test.world -e "(defun saved-fn (x) (list 'it-works x))" \
	  -e "(defvar *saved* (list 1 \"two\" #(3)))" -e "(disk-save)" > /dev/null 2>&1
	@./build/lisp --world build/test.world -e "(saved-fn 42)" -e "*saved*" > build/world.out 2>&1; \
	  if grep -q "world restored" build/world.out && grep -qx "(IT-WORKS 42)" build/world.out \
	     && grep -qx '(1 "two" #(3))' build/world.out; then echo "ok: definitions and data survive a save and restore"; \
	  else cat build/world.out; echo "FAIL: world round trip"; exit 1; fi
	./build/lisp --stats --gc-verify --gc-every 100 --world build/test.world $(TESTS)
	@cp build/test.world build/bad.world; printf 'X' | dd of=build/bad.world bs=1 seek=5000 conv=notrunc 2>/dev/null; \
	  ./build/lisp --world build/bad.world -e "(+ 1 2)" > build/world.out 2>&1; \
	  if grep -q "damaged" build/world.out && grep -qx 3 build/world.out; then echo "ok: a damaged image is rejected, the system boots from source"; \
	  else cat build/world.out; echo "FAIL: damaged image"; exit 1; fi
	@head -c 1000 build/test.world > build/bad.world; \
	  ./build/lisp --world build/bad.world -e "(+ 1 2)" > build/world.out 2>&1; \
	  if grep -q "truncated" build/world.out && grep -qx 3 build/world.out; then echo "ok: a truncated image is rejected"; \
	  else cat build/world.out; echo "FAIL: truncated image"; exit 1; fi
	@echo "== screen"
	rm -rf build/shot-src && mkdir -p build/shot-src && cp world/*.lisp build/shot-src/
	./build/shot build/shot.ppm build/shot-src
	@echo "== M9: a world built from the files the editor saved has the change"
	$(PYTHON) -I tools/catworld.py $(subst world/,build/shot-src/,$(WORLD)) > build/shot-src/world.lisp
	$(PYTHON) -I tools/embed.py build/shot-src/world.lisp build/shot-src/boot_lisp.h boot_lisp
	$(CLANG) -std=c11 -D_DEFAULT_SOURCE -O1 -Ikernel -Ibuild/shot-src -Ibuild host/native_repl.c $(LISP_SRC) -o build/shot-src/lisp $(LDLIBS)
	./build/shot-src/lisp tests/lisp/check.lisp tests/lisp/rebuilt.lisp
	$(PYTHON) -I tools/ppm2png.py build/shot.ppm build/shot.png

stress: build/lisp
	./build/lisp --stats --gc-verify --gc-every 10 $(TESTS)

# The PDF edition of docs/ (needs Tectonic: https://tectonic-typesetting.github.io)
TECTONIC ?= $(shell command -v tectonic 2>/dev/null || echo $(HOME)/.local/bin/tectonic)
SCREENSHOT_FORMS = "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))" "(fact 15)" \
  "(defun average (numbers) (/ (apply '+ numbers) (length numbers)))" \
  "(defun report (scores) (list :average (average scores)))" \
  "(report nil)" ":Frame 0"

build/screenshot: tools/screenshot.c $(KERNEL_SRC) $(HEADERS)
	$(CLANG) $(CFLAGS) -O2 tools/screenshot.c $(KERNEL_SRC) -o $@ $(LDLIBS)

build/docs/screen.png: build/screenshot tools/ppm2png.py
	@mkdir -p build/docs
	./build/screenshot build/docs/screen.ppm 900 640 2 $(SCREENSHOT_FORMS) > /dev/null
	$(PYTHON) -I tools/ppm2png.py build/docs/screen.ppm $@

# the README's picture, kept in the repository: make docs/lantern.png
README_FORMS = "(demo)" "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))" "(fact 30)" \
  "(/ (fact 10) (fact 12))" \
  "(let ((*print-right-margin* 40)) (pprint '(defun area (shape) (let ((w (width shape)) (h (height shape))) (* w h)))))" \
  '"Grüße, λόγος"'

docs/lantern.png: build/screenshot tools/ppm2png.py
	@mkdir -p build/docs
	./build/screenshot build/docs/lantern.ppm 900 600 2 $(README_FORMS) > /dev/null
	$(PYTHON) -I tools/ppm2png.py build/docs/lantern.ppm $@

build/docs/overview.tex: docs/OVERVIEW.md tools/md2tex.py
	@mkdir -p build/docs
	$(PYTHON) -I tools/md2tex.py $< $@

build/docs/preface.tex: docs/PREFACE.md tools/md2tex.py
	@mkdir -p build/docs
	$(PYTHON) -I tools/md2tex.py $< $@

build/docs/history.tex: docs/LISP-MACHINES.md tools/md2tex.py
	@mkdir -p build/docs
	$(PYTHON) -I tools/md2tex.py $< $@

pdf: docs/A-Lisp-Machine-in-the-Browser.pdf

docs/A-Lisp-Machine-in-the-Browser.pdf: build/docs/book.pdf
	cp $< $@

build/docs/book.pdf: docs/book.tex build/docs/preface.tex build/docs/overview.tex build/docs/history.tex build/docs/screen.png
	cd docs && $(TECTONIC) -o ../build/docs book.tex

serve: web
	$(PYTHON) -I tools/serve.py $(PORT) build/web world

clean:
	rm -rf build
