# PLAN — Lantern, a Lisp Machine–spirited environment in C + WebAssembly

See [docs/OVERVIEW.md](docs/OVERVIEW.md) for what the machine is today, and
[docs/LISP-MACHINES.md](docs/LISP-MACHINES.md) for the history of the original
machines and how this design compares to them. `make pdf` turns both, after
the preface in [docs/PREFACE.md](docs/PREFACE.md), into the manual,
[docs/A-Lisp-Machine-in-the-Browser.pdf](docs/A-Lisp-Machine-in-the-Browser.pdf)
(its frame and title page are [docs/book.tex](docs/book.tex)).

## Status

- **M0 — done (2026-11-19).** `make` builds `build/web/` (Apple clang
  `--target=wasm32` + `wasm-ld`, no Emscripten, no libc); `make serve` serves it on
  http://localhost:8742/; `make test` runs the native headless checks and writes
  `build/shot.png`.
- **M1 — done (2026-11-19).** Tiny Lisp in C (`kernel/lisp.h` and friends):
  - 64-bit tagged words with heap *indices*, not addresses.
  - A Cheney copying GC with precise roots.
  - A reader (with backquote) and a printer.
  - An **explicit-stack evaluator** instead of a C-recursive tree-walker: proper
    tail calls, no C-stack overflow, errors without `setjmp`, and a computation
    can run a budget of steps per frame (the browser stays live; F1 aborts).
  - Lisp-2, lexical scope, displacing macros, and Lisp Machine–style `IF` with
    an implicit-PROGN else.
  - The prelude is in `world/boot.lisp`, embedded into the kernel at build time.
  - `make lisp` gives a native REPL (`./build/lisp`).
  - `make test` runs 144 Lisp checks: plain, under ASan/UBSan, with a small heap,
    with GC every 100 allocations, plus the error suite. `make stress` collects
    on every allocation (passes, about 5 min).
  - In the browser the Listener is a real REPL. Speed is about 160M evaluator
    steps/s in wasm, about 110M native.
  - Known M1 limits, left for M2: no special variables, `&optional` defaults
    must be constants, no `block`/`catch`/`unwind-protect`, fixnums only, no
    double-nested backquote.
- **M2 — done (2026-11-19).** Bytecode VM + a compiler written in Lisp:
  - `kernel/vm.c` is a stack VM with about 50 opcodes, built into the same
    explicit-stack machine. Compiled and interpreted code call each other
    freely, and the per-frame step budget and Abort still work.
  - `world/compiler.lisp` works in three passes: macro expansion into a small
    core language, code generation, and assembly.
    - Captured variables go in heap environment vectors; all others stay in
      stack slots.
    - Tail calls compile to `TAILCALL`.
    - Simple primitives (`car`, `+`, `<`, …) compile inline.
    - `disassemble` shows the bytecode.
  - **Bootstrap:** the interpreter runs `boot.lisp` and `compiler.lisp`. The
    compiler then compiles everything defined so far, itself included (112
    functions), and becomes the evaluator, so every REPL form and `eval` is
    compiled. Boot takes about 50 ms, in wasm too.
  - New in the language:
    - special variables (`defvar`, shallow binding)
    - `catch`/`throw`, `block`/`return-from`/`return`
    - `unwind-protect`
    - `&optional` defaults that are any expression
    - an implicit `block` around `defun` bodies
    - `loop`
  - **Conditions:** errors are condition objects (`error` takes an optional type).
    `handler-case` and `ignore-errors` catch them, including errors raised in C
    and stack overflow. *Restarts are deferred to the debugger (M8).*
  - **Speed:** compiled code is about 3x faster than the interpreter on
    fib/tak/list work (`tests/lisp/bench.lisp`). There's room for more later:
    direct calls, fewer `GFUNC`s.
  - **Tests:** `make test` runs 205 checks. The GC stress run now also
    *verifies the heap* before every collection (`--gc-verify`). That found an
    M1 reader bug, a stale `QUOTE` symbol held across an allocation, now fixed.
  - **Limits:**
    - The interpreter, now used only for bootstrapping and `%interpret`,
      has no `block`/`catch`/specials.
    - No `&key`.
    - Fixnums only.

- **M3 — done (2026-11-19).** Saved worlds, the Lisp Machine "band":
  - `kernel/image.c`: an image is header + global roots + heap + host data,
    taken right after a compacting GC while the machine is idle. Heap pointers
    are indices, so loading is a plain copy, and images move freely between
    the native build and the browser.
  - A fingerprint (primitives, opcodes, roots, heap size, the embedded world
    source) and a checksum guard every image. A foreign, truncated or damaged
    image is rejected and the machine boots from source instead; nothing is
    half-loaded.
  - Lisp: `(disk-save)` saves now, `(forget-world)` makes the next start fresh.
  - Browser: the world is autosaved to IndexedDB about 1.5 s after each
    command (and when the tab is hidden), and restored on reload together with
    the Listener's text. Cmd+S downloads the world as a `.world` file, dropping
    a `.world` file on the page loads it, and `?fresh` in the URL skips it.
  - Native: `./build/lisp --world FILE`.
  - Starting from a world takes about 0.3 ms instead of about 55 ms from source.
    A typical world is about 75 KB.
  - Tests: a save/restore round trip, the whole Lisp suite run on a restored
    world under verified GC stress, and damaged and truncated images rejected.
  - *Note:* rebuilding the kernel or changing `world/*.lisp` changes the
    fingerprint, so an older saved world is not used. The machine boots
    fresh and says so.

- **M4 — done (2026-11-19).** Graphics and text drawn by Lisp:
  - **C keeps only the "microcode":** `bitblt` (CADR argument order) with four
    ALU ops (seta, ior, xor, andca), `%draw-rectangle`, `%draw-char` from a
    font raster, pixel reads, the event queue (`kernel/gprims.c`).
  - **Bitmaps and fonts are Lisp data** (`world/graphics.lisp`).
    - The 5×9 font is generated as Lisp source from `fonts/5x9.txt`.
    - Lisp builds a fixed, a variable-width and a bold font from it at boot.
    - Drawing: lines (with thickness), circles, filled circles, frames,
      `draw-string`, `string-width`. `(graphics-demo)` shows them off.
  - **All printing goes through Lisp streams** (`world/streams.lisp`): host,
    Listener and string streams, `with-output-to-string`.
  - **The Listener is a Lisp program** (`world/listener.lisp`), the machine's
    one process.
    - It draws its window label, text, blinker, mouse documentation line and
      who-line, and scrolls with `bitblt`.
    - It reads multi-line forms with Rubout and c-U editing, reports errors and
      goes on.
    - It waits for input by *yielding* to the browser, and the frame loop
      resumes it when an event arrives (or every 500 ms for the blinker and
      the clock).
    - Abort (F1) throws to its `(catch :abort ...)`.
    - Its text is part of the world, so it survives a save and restore.
  - `demo.c` is gone: the kernel only composites the mouse cursor.
  - Images now store the *global* values of special variables. The
    evaluator's stack is not saved: after a restore the Listener process is
    started afresh.
  - Tests: the screen test types into the Listener through the kernel.
    - It checks evaluation, multi-line forms, error recovery, Abort of
      `(loop)`, scrolling, resizing and the graphics demo.
    - All earlier tests still pass.

- **M5 — done (2026-11-19).** Objects and processes:
  - **Object system** (`world/objects.lisp`): CLOS syntax, dispatch on the first
    argument as in Flavors.
    - `defclass` with multiple inheritance (mixins), slots with
      `:initform`/`:initarg`/`:accessor`/`:reader`.
    - Generic functions with standard method combination (`:around`,
      `:before`, primary with `call-next-method`, `:after`).
    - Methods on built-in classes (integer, string, …), `make-instance` with
      `initialize-instance`, `slot-value`, `setf` on accessors, `describe`.
    - The class precedence list is depth-first with shared superclasses last,
      as in Flavors.
  - **`&key` arguments** in the compiler.
  - **Processes**, in the spirit of stack groups.
    - A process is switched out by copying its stack and special bindings into
      the heap.
    - The C scheduler (`kernel/kernel.c`) gives every runnable process a slice
      each frame and preempts at any instruction.
    - Lisp API (`world/processes.lisp`): `process-run-function`,
      `process-wait`, `process-sleep`, `process-yield`, `process-kill`,
      `(processes)`.
    - Locks use the atomic `store-conditional`.
    - The Listener is just the process that owns the keyboard (and Abort).
  - **Saved worlds keep their processes.** Between frames every process lives
    in the heap, so an image holds them all. After a restore they carry on,
    and their wake-up times are reset because the page clock starts again.
  - **Done-test:** `(demo)` starts three windows built from
    `label-mixin` + `border-mixin` + `window`, each run by its own process.
    - A clock sleeps between ticks.
    - A ball bounces at about 30 fps.
    - A prime finder never sleeps, so only preemption lets the others run.
    - Meanwhile the Listener keeps working in a narrower column.
  - Tests: 37 object-system checks, plus screen tests for concurrent
    processes, a lock under contention (no lost updates), stop-demo, and
    saving and restoring a world with processes running.

- **M6 — done (2026-11-19).** The window system (`world/windows.lisp`):
  - **Windows are objects:** `window` + `border-mixin` + `label-mixin`, with
    generic functions `window-refresh`, `draw-contents`, `window-inside`,
    `mouse-documentation`, `window-blinker` and `window-reshaped`.
  - **Every window has its own bitmap**, so a covered window keeps its
    contents, like a deexposed window with its saved bitmap.
  - **The Screen Manager** is the process that owns the keyboard and mouse.
    - It composites the windows back to front over a dotted desktop whenever
      something changed.
    - On top it draws the selected window's blinker, any pop-up menu, the
      mouse documentation line and the who-line.
  - **Mouse:**
    - left-click selects a window (black label)
    - dragging the label moves it, dragging the corner grip reshapes it
    - Mouse-R opens the system menu: Create Listener, Move, Reshape, Bury,
      Refresh, Kill, Select next, the demos
  - **Keys:** they go to the selected window's input queue (typeahead works).
    Select (F2) cycles windows, and the kernel delivers Abort (F1) to the
    selected window's process.
  - **Listener windows** (`world/listener.lisp`): each has its own process,
    text, cursor and output stream, and reflows its text when reshaped.
  - The demo windows and the graphics demo are ordinary windows that can be
    moved, reshaped, buried and killed.
  - **Saved worlds** keep all windows and their bitmaps.
  - **Done-test:** several Listeners in overlapping windows, moved and reshaped
    with the mouse. It is covered by the screen test and was checked in the
    browser with real mouse drags.
  - Tests: the screen test drives the window system through the event queue.
    It covers moving and reshaping by mouse, the system menu creating a
    second Listener, F2 between Listeners, one Listener working while the
    other loops, Abort reaching the selected one, the demo windows, a lock
    under contention, saving and restoring everything, and screen resizes.

- **M7 — done (2026-11-19).** Presentations (`world/presentations.lisp`):
  - **Presented values:** the Listener prints values with `present`, which
    records which text shows which object, nested, so every element of a
    printed list or vector is an object of its own.
  - **Finding them again:** each logical line has a serial number, and a row
    map ties visual rows back to lines and columns, so the records survive
    scrolling, wrapping, reshaping and saved worlds.
  - **Mouse:** moving over a presentation outlines the innermost object. The
    mouse documentation line names its commands. **Mouse-L inspects it**,
    and **Mouse-R** gives the commands for its type: Inspect, Describe, Show
    Definition, Disassemble, Use as Input.
  - **Commands** (`define-command`) have typed arguments: a class or a
    predicate. One table serves the mouse and the keyboard.
    - Commands that print run in the Listener's own process: the mouse queues
      them in its input like keystrokes, the Lisp Machine's "blips".
    - In the Listener, `:Name args` runs a command, Tab completes the name
      (or lists the candidates), and Help (F4) lists the commands.
  - **Use as Input** types the object into the next form, as
    `(presentation-object N)` when it cannot be read back.
  - **The Inspector** is a window showing an object, its class and its parts
    (slots, elements, a symbol's value, function and properties, a class's
    precedence list, a process's state…). Every part is presented, there is
    a Back link, and it never takes the keyboard.
  - `defun` and `defmacro` record their source; Show Definition pretty-prints it.
  - `*`, `**` and `***` hold the last values.
  - **Faster generic functions:** a generic whose applicable method is a
    single primary method without `call-next-method` now calls it directly.
    `defmethod` detects that at compile time. The Listener also caches its
    text geometry. Together these made redrawing text many times cheaper (the
    tests caught a slow resize).
  - **Done-test:** clicking a printed object in the Listener opens it in the
    Inspector. It is in the screen test and was checked in the browser.
  - Tests: highlight on hover, click to inspect, clicking parts in the
    Inspector and Back, the command menu with Describe, command lines, Tab
    completion, Show Definition, Help, and Use as Input.

- **M8 — done (2026-11-19).** Tools: the debugger and the editor.
  - **Restarts** (`world/conditions.lisp`): `restart-case`, `invoke-restart`,
    `compute-restarts`, `find-restart`, `with-simple-restart`, `cerror`,
    `abort`, `continue`, built on catch tags and a special `*restarts*`.
  - **The debugger runs on live frames** (`world/debugger.lisp`): an error no
    handler takes no longer unwinds. If the process has a debugger
    (`*debugger*`), the kernel calls `%invoke-debugger` on top of the stack,
    so the failed frames are still there.
    - It shows the error, the restarts (Abort/F1 takes the innermost ABORT,
      Resume/F3 the innermost CONTINUE) and a backtrace whose frames and
      arguments are presentations. Mouse-L on a frame shows its locals (named
      from the source), on a restart takes it.
    - `:Retry` calls a frame's function again with the same arguments; if it
      was redefined meanwhile, the new definition runs (methods too).
      `:Return form` makes a frame return a value. Leaving a frame runs the
      unwind-protect cleanups inside it and undoes its special bindings.
    - Forms typed at `Debug 1>` run with the failed frames' dynamic bindings
      in effect; an error there gives a deeper debugger (up to three).
    - **Where it runs:** in a Listener, in the Listener. In a window's process,
      in a Debugger window of its own. In the Screen Manager, the kernel first
      starts a new Screen Manager, so the screen keeps working while you debug
      the old one; after a restart the old one ends.
    - A stack overflow can be debugged too: the stack keeps a reserve for the
      debugger.
  - **Kernel support:** each special binding records the stack height it was
    made at. New primitives: `%frames`, `%frame-function`, `%frame-args`,
    `%frame-locals`, `%frame-return`, `%frame-reinvoke`, `%function-name`,
    `%set-function-name`, `%world-source`, `%split-lines`. Methods and generic
    functions are named, as `(method name qualifier class)`.
  - **Zmacs** (`world/editor.lisp`): an Emacs-like editor on the world's own
    source. The kernel keeps the text the world was built from, and the
    editor cuts it into files again at the `;;;; FILE` lines
    `tools/catworld.py` adds.
    - Keys: moving by character, word, line and page; kill, yank, mark and
      region; search; Lisp indentation on Tab; the matching paren is boxed.
      `c-X c-F` visits a file, `c-X B` a buffer, `m-X` runs Compile Buffer,
      List Buffers and so on. Help (F4) lists the keys.
    - `m-.` finds a definition in any file: functions, macros, variables,
      classes, methods (by class and qualifier); `c-.` goes to the next one.
      A function typed in a Listener comes back from its recorded source.
    - **`c-sh-C` compiles the definition around the cursor into the running
      world.** Buffers are saved with the world.
    - `(ed)`, `(ed 'name)`, `(ed "file.lisp")`, `:Edit-Definition`, and
      `:Edit-Frame` in the debugger; "Editor" in the system menu.
  - **Fixes along the way:**
    - `eval` itself had stayed interpreted, defined before the compiler took
      over; it is compiled now.
    - Keys could be lost when the Screen Manager added input while a window's
      process took it, because both did a read-modify-write. Both now use
      `store-conditional`.
    - In the browser, Option with punctuation (`m-.`, `m-<`) now reaches the
      machine. Plain c-S is search: the world downloads with Cmd-S (or
      Ctrl-Shift-S).
  - **Done-test:** fix a bug in the window system from inside the running
    system. In the screen test:
    1. Zmacs plants a bug in `graphics-window`'s `draw-contents` and compiles it.
    2. The system menu's Graphics demo makes the Screen Manager fail, and a
       Debugger window opens while a new Screen Manager takes over.
    3. `:Edit-Frame` takes Zmacs to the method, the bug is fixed and compiled,
       and `:Retry` in the debugger finishes the old Screen Manager's work:
       the window opens.

    It was also checked in the browser: `draw-label` was changed in Zmacs and
    the title bars changed at once.
  - Tests: 39 new Lisp checks (restarts, frames, return, retry, cleanups,
    bindings, nested debuggers, stack overflow, locals, the editor's search
    for definitions and its indentation), and the debugger and editor in the
    screen test.
  - **Peek** (`world/peek.lisp`), added after the done-test: a window whose
    process redraws it every second, with three views.
    - **Processes:** state, what each one waits for, and its share of the
      machine since the last look. The kernel now counts the evaluator steps
      each process runs, in a 13th process field.
    - **Windows:** front to back, with place, size and process.
    - **Memory:** a heap bar, collections and steps, and counts of symbols,
      classes, commands, processes, windows and editor buffers.
    - Click a view's name, or type P, W or M. Processes and windows are
      presentations: Mouse-R offers Kill Process, Select Window, Bury Window
      and Kill Window. Open it from the system menu, with `:Peek` or with
      `(peek)`. It never takes the keyboard.
  - Writing a changed buffer back to a file on the host came with M9.

- **M9 — done (2026-11-19).** Files: the system edits its own source.
  - **The server:** `make serve` now runs `tools/serve.py`. It serves the
    page as before and also lists, reads and writes `world/*.lisp` under
    `/files/`.
    - It listens on 127.0.0.1 only and answers only requests addressed to
      localhost, which rules out DNS rebinding.
    - It refuses writes from other origins, and only touches existing,
      plainly named `.lisp` files. Writes are atomic.
    - It tells the browser never to reuse an old build without checking.
  - **The request channel:** the machine has no file system, so it asks the
    host. A Lisp process puts one request in a kernel slot with
    `%file-request` (list, read or write). After a frame `host.js` takes it,
    fetches, and answers with `kernel_file_reply`, which sets `*file-reply*`.
    Bytes pass through unchanged, so UTF-8 survives a round trip. A lock
    keeps it to one request at a time.
  - **Zmacs on real files:**
    - Under a file server it loads every world file from disk; a file the
      server lacks (the generated font) comes from the built-in source and
      shows `[built-in]` in the mode line.
    - `c-X c-S` saves to `world/NAME`.
    - `m-X Revert Buffer` reads the file again.
    - `m-X Compare with File` lists the changed lines: the common start and
      end are skipped, the rest is matched by a longest common subsequence.
    - Without a server everything works as in M8, read-only.
  - **Done-test:** fix a bug in the window system in Zmacs, save it, run
    `make`, and the fresh build has the fix.
    - `make test` does it end to end. The screen test plays the file server
      on a copy of `world/`: Zmacs changes `draw-label`, compiles and saves
      it, and Compare and Revert are checked. Then the Makefile builds a
      native machine from the saved files, and `tests/lisp/rebuilt.lisp`
      checks that its compiled `draw-label` really draws the title further
      right.
    - It was also done in the browser against the real `world/windows.lisp`:
      edit, `c-sh-C`, `c-X c-S`, `git diff` (one line), `make`, a fresh boot
      with the title moved. Then the file was put back with git.
  - The project is now a git repository.

- **M10 — done (2026-11-19), apart from one check left to you.** Self-contained:
  the machine as one file.
  - **Where the files are kept** (`host/host.js`, chosen automatically, best
    first; the mode line and `m-X Where Are Files` say which):
    1. **The file server** (`make serve`): `world/` on disk, as in M9.
    2. **A folder you chose** (Chrome, Edge: `m-X Choose Folder`): saves go
       straight to disk without a server. The browser keeps the permission,
       and asks again when it has lapsed.
    3. **The browser's storage** (IndexedDB, next to the saved world): saves
       stay in the browser. A file never saved comes from the built-in
       source, marked `[built-in, not saved yet]`.
  - **In and out:**
    - `m-X Download File` saves a buffer to Downloads.
    - `m-X Upload Files`, or dropping `.lisp` files on the page, brings files
      into Zmacs as modified buffers (`[uploaded]`). Nothing is written until
      `c-X c-S`, so a drop never overwrites a file by itself.
    - Dropping a `.world` file still loads a world.
  - **No extra buttons:** a small dialog appears at the top of the page only
    when the browser needs a click (choosing files or a folder), with Cancel.
    Failures and cancellations show on Zmacs's echo line, not in the debugger.
  - **Kernel:** file operations 4–8 (download, upload, where, choose folder,
    take dropped files) and the `EV_FILES` event. `%file-server-p` now says
    which kind of storage is in use.
  - **`build/web/lisp.html`** (`tools/single.py`, built by `make`): the page,
    `host.js` and the kernel as base64, about 450 KB. It needs no server: it
    opens from disk or from any plain web host. `serve.py` also stops the
    browser from reusing an old build.
  - **Done-test:**
    - Checked in the browser, with `lisp.html` on a plain static web server:
      it boots; a buffer saved there survives a fresh reload; the saved world
      survives a reload; Download File hands the browser the file; a dropped
      `.lisp` file opens in Zmacs; Upload Files shows its dialog, and Cancel
      is reported quietly.
    - `make test` covers download and drop through the screen test's host.
    - **Not checked yet:** opening `lisp.html` straight from Finder, and the
      folder route, which needs Chrome or Edge.

- **M11 — done (2026-11-20).** Common Lisp, part 1: the data a program
  needs (`world/cl.lisp`, `kernel/bignum.c`).
  - **Bignums:** integers of any size, up to 262,144 bits.
    - A raw heap object of 32-bit limbs, which the collector copies without
      scanning. Results are normalized, so anything that fits is a fixnum
      again.
    - `+ - * / rem mod 1+ 1-` and the comparisons keep their fixnum fast path,
      in the primitives and in the VM's opcodes, and overflow moves on.
      Division is Knuth's algorithm D.
    - The reader reads them, the printer prints them, `eql` and `equal`
      compare them by value, and hash tables hash them by value. `eql` was
      compiled as `eq` before; it is not inlined any more.
  - **Multiple values:** `values` leaves its values in the machine, and a new
    MVLIST instruction takes them right after the form. They pass through
    `RET`, `JMP`, `UNBIND` and `UNCATCH`, every other instruction drops them,
    and a process keeps them.
    - Built on that: `multiple-value-bind`, `multiple-value-list`,
      `multiple-value-call`, `multiple-value-setq`, `nth-value` and
      `values-list`.
    - `floor`, `ceiling`, `truncate` and `round` return quotient and
      remainder; `gethash` and `parse-integer` return two values.
  - **Hash tables** (EQ, EQL, EQUAL): `%hash` hashes by contents where it can
    (numbers, characters, symbol names, strings and lists for EQUAL). A key
    hashed by its address makes the table remember the collection count, and
    it rehashes after a collection. Lookups and inserts retry if a collection
    moved keys in between. `setf`/`incf` on `gethash`, `remhash`, `maphash`,
    `clrhash`, `loop ... being the hash-keys`. The Inspector shows the
    entries.
  - **`defstruct`** on top of `defclass` (so `typep`, the Inspector and
    `describe` work), with `:conc-name`, `:constructor`, `:predicate`,
    `:copier` and `:include`, `setf`-able accessors, and slot positions looked
    up once.
  - **`format`:** `~A ~S ~D ~B ~O ~X ~R`, with mincol and padchar, `@` and `:`
    (`~:D` groups digits); `~C ~% ~& ~~ ~T ~* ~P ~?`; `~{~}` with `~^`, `~:{`
    and `~@{`; `~[~;~:;~]`, `~:[` and `~@[`; `~(~)` in four kinds; and
    `~newline`.
  - **The extended `loop`:**
    - Iteration: `for/as` with `from/to/below/downto/by`, `in`, `on`,
      `across`, `= ... then`, simple destructuring, and `being the
      hash-keys/hash-values`; plus `with` and `repeat`.
    - Tests: `while`, `until`, `always`, `never`, `thereis`.
    - Accumulation: `collect/append/nconc/sum/count/maximize/minimize`,
      with or without `into`.
    - Clauses: `when/if/unless ... and ... else ... end`, `do`, `return`,
      `initially`, `finally` and `named`.
  - **Sequences and strings:**
    - On sequences, with `:test`, `:key`, `:start`, `:end` and `:from-end`:
      `sort` (stable merge sort), `subseq`, `concatenate`, `map`, `find`,
      `position`, `count`, `remove`, `remove-duplicates`, `substitute`,
      `search`, `mismatch`, `reduce`, `fill`, `elt`.
    - On lists: `mapcar` over several lists, `nconc`, `union` and the other
      set operations, `getf`.
    - On characters and strings: case conversion, `string<` and friends,
      `string-trim`, `parse-integer`, `digit-char-p`.
    - Also `typecase`, `ecase`, `check-type`, `gcd`, `lcm`, `isqrt`, and
      `expt` by squaring.
  - **Kept from Lisp Machine Lisp:** `member`, `assoc` and `remove` compare
    with `equal` by default.
  - **Found on the way:** every `block`, and so every `dolist` and `dotimes`,
    allocated a tag and set up a catch, even when nothing returned from it.
    Now a block that no `return-from` uses compiles to a plain `progn`. The
    GC stress test found it, through hash tables that lost keys.
  - **Done-test:** `tests/lisp/cl.lisp` (187 checks) starts with a small
    program in ordinary Common Lisp style, run unchanged. It splits text with
    `loop`, counts words in an EQUAL hash table, sorts them and prints a
    table with `format`, computes `(fact 100)` exactly, and uses `defstruct`
    and `multiple-value-bind`. It also passes under the GC stress settings and
    in a saved world. Checked in the browser too: bignums, `format`, `loop`
    and a hash table in the Listener and the Inspector.
  - Floats are next, with `labels`/`flet` and packages.
- **M12 — done (2026-11-20).** Common Lisp, part 2: floats, local functions,
  packages (`kernel/float.c`, `kernel/package.c`, `world/packages.lisp`).
  - **Floats:** one kind, the IEEE double, a raw 8-byte heap object. Every
    float type name (`single-float`, `double-float` ...) means it.
    - The reader reads `1.5`, `.5`, `1e10`, `2.5d0`, `1.0f0` (all exponent
      markers give a double); `12.` is still the integer 12. The printer gives
      the shortest digits that read back the same: `0.1`, `100.0`, `1.0e21`.
    - Arithmetic has float contagion, in the primitives and on the VM's slow
      path: anything with a float goes to `float.c`. Comparisons between
      floats and bignums are exact. `/` on two integers still truncates
      (there are no fractions).
    - Results that are not finite are errors (overflow, division by zero):
      there are no infinities or NaNs in Lisp.
    - `sqrt exp log sin cos tan asin acos atan sinh cosh tanh expt`,
      `floor ceiling truncate round` and the `f` versions on floats, `float`,
      `decode-float`, `integer-decode-float`, `float-sign`, `pi`, `random`.
    - `format`: `~F`, `~E`, `~$`, rounded half up from the printed digits, so
      `(format nil "~$" 12.345)` gives `12.35`.
    - What a libm would do comes from the host: the browser's `Math`
      functions and its decimal conversions (`toExponential`, `Number`), three
      imports in all; natively the C library. The browser and native builds
      print the same digits.
  - **Local functions:** `flet`, `labels` and `macrolet`, in the compiler's
    macro-expansion pass. A local function is a variable holding a closure
    named `(flet f)` or `(labels f)`, and a call is a `funcall` of it. Local
    names shadow global functions and macros; `#'f` and `return-from f` work.
  - **Packages:** `defpackage` (`:use :export :nicknames :shadow
    :import-from :intern`), `in-package`, `export`, `import`, `shadow`,
    `use-package`, `find-symbol` with its status, `intern`, `do-symbols` and
    the rest.
    - COMMON-LISP (also CL, LISP, SYSTEM) is the world itself: its table is
      the old obarray, and every one of its symbols counts as external.
      KEYWORD has the keywords, which keep their colon in their names.
      COMMON-LISP-USER is where the Listener starts.
    - Every symbol records its home package. The reader interns in
      `*package*` and reads `pkg:name` (external only, else an error) and
      `pkg::name`; the printer adds the prefix when `*package*` does not see
      the symbol, and prints uninterned ones as `#:g12`.
    - Each Listener binds its own `*package*`. Zmacs reads a buffer in the
      package of the last `(in-package ...)` above the form, else COMMON-LISP
      for a file.
    - The kernel's REPL now reads one form, evaluates it, then reads the
      next, so an `in-package` at the top of a file applies to the rest.
  - **Also:** `#:name`, `#x #b #o`; `declare`, `declaim` and `the` accepted
    (and ignored); `read-from-string`; the Inspector shows packages, floats,
    and a symbol's package.
  - **Done-test:** `tests/lisp/cl2.lisp` (170 checks) starts with a small
    statistics library in a package of its own, written in ordinary Common
    Lisp style. It has floats, `flet` and `labels`, Newton's square root and
    `~,2F` output, and is used from CL-USER through `stats:mean`. It passes
    under the GC stress settings and in a saved world. Checked in the
    browser too: float digits, `defpackage`/`in-package`, and a package in
    the Inspector.
  - Next: `destructuring-bind` and `do`, arrays (`make-array`, `coerce`),
    condition classes (`define-condition`, `handler-bind`), string streams and
    `read`, `#+`/`#-`.
- **M13 — done (2026-11-20).** Common Lisp, part 3: the forms and data larger
  programs lean on (`world/cl2.lisp`, `world/signals.lisp`, `kernel/array.c`).
  - **Forms:** `do`, `do*`, `psetq`, `psetf`, `rotatef`, `shiftf`;
    `destructuring-bind` with nested patterns, `&optional`, `&rest`/`&body`,
    `&key`, `&aux`, `&whole` and dotted tails. `defmacro` takes the same
    lambda lists, `(defmacro with-x ((var val) &body body) ...)`.
  - **Lambda lists:** supplied-p variables, `(x 1 x-p)` in `&optional` and
    `&key`, `((:keyword var) default)`, and `&aux`, in the compiler.
  - **`setf`:** several pairs at once, `(defun (setf name) (new ...) ...)`,
    `#'(setf name)`, and `defsetf` in its short and long forms.
  - **Arrays:** simple vectors and strings stay as they were, the fast common
    case. Other arrays are a new kernel object (HT_ARRAY), a header around a
    vector or string of the elements in row-major order. It handles:
    - any rank, with `aref` and `(setf aref)` taking any number of subscripts
    - fill pointers, and `vector-push`, `vector-push-extend`, `vector-pop`
    - `adjust-array`, which keeps the elements whose subscripts are in both
      shapes
    - displaced arrays
    - the rest: `array-dimensions`, `row-major-aref`, `array-in-bounds-p`,
      `coerce`
    - printing and reading as `#2A((1 2) (3 4))`
    `length`, `vectorp` and the sequence functions take one-dimensional
    arrays.
  - **Conditions:**
    - `define-condition` makes real classes, with slots and a `:report`
      (a string or a function).
    - `make-condition`; `error`, `signal` and `warn` take a condition, a type
      and initargs, or a format control.
    - The standard classes: `simple-error`, `type-error`, `division-by-zero`,
      `unbound-variable`, `undefined-function`, `end-of-file` and the rest.
    - The kernel's own errors map onto those classes by their message, so a
      handler for `type-error` catches `(car 5)`.
    - `princ` and `~A` of a condition write its report.
  - **Handlers:** `handler-bind` and `handler-case` now share one handler
    stack, `*handler-clusters*`.
    - On an error, while any handler is established, the kernel calls
      `%signal-hook` on top of the stack, as it does the debugger. The
      handlers run before anything unwinds, innermost first, each with only
      the outer ones in effect.
    - `handler-case` is a `catch` whose handlers throw to it. So the two nest
      as Common Lisp says, and a `handler-bind` handler can take a restart.
    - If no handler takes the error, `%unhandled` passes it on to the
      debugger, which hides these frames.
    - Also new: `:no-error`, `muffle-warning`, `use-value`, `store-value`,
      `assert`.
  - **Input:** input streams (`make-string-input-stream`,
    `with-input-from-string`), `read`, `read-line`, `read-char`, `peek-char`,
    `unread-char`, `listen`.
    - A Listener's `*standard-input*` reads lines from the keyboard, so
      `(read)` typed there waits for the user. `y-or-n-p`; `*error-output*`
      and `*query-io*` follow the Listener.
    - `open`, `close` and `with-open-file` work on the editor's files, read
      and written whole. `load` reads and evaluates a file or stream form by
      form, so `in-package` inside it applies to the rest of it.
  - **Reader:** `#+` and `#-` with `:and`, `:or` and `:not` over
    `*features*`. A form left out is read without interning anything, so it
    may name packages that do not exist here.
  - **Done-test:** `tests/lisp/cl3.lisp` (129 checks) starts with a small
    program in ordinary Common Lisp style:
    - it reads records with `read` from a string stream
    - takes them apart with `destructuring-bind`
    - keeps a table in a two-dimensional array
    - signals its own condition for a bad record, and skips it through a
      restart that a `handler-bind` chooses and logs
    - prints the averages with `~,1F` under `#+lisp-m`
    It passes under the GC stress settings and in a saved world. Checked in
    the browser too: `(read-line)` and a multi-line `(read)` from the
    keyboard, `#2A` output, and a user's condition in the debugger with its
    report.
  - Next: `tagbody`/`go` and `prog`, `defstruct` printing and `print-object`,
    `loop` destructuring and `return` details, `format ~<~>`, bit operations
    on bignums, characters beyond ASCII.
- **M14 — done (2026-11-20).** Common Lisp, part 4: control, printing, bits
  (`world/printer.lisp`; `tagbody` in `world/compiler.lisp`; bit operations in
  `kernel/bignum.c`).
  - **`tagbody` and `go`, `prog` and `prog*`:** the compiler cuts the body at
    its tags into numbered segments and runs them in a loop around a `catch`.
    - `go` throws the number of the segment to start at, so it unwinds
      `unwind-protect`, special bindings and handlers on the way, and works
      from inside a closure while the `tagbody` is active.
    - Running from segment k falls through the later ones. Tags may be
      symbols or integers.
    - `do` bodies with tags are `tagbody` bodies.
  - **The printer in Lisp:**
    - `print-object` is a generic function, and `print-unreadable-object`
      is there. Structures are `structure-object`s and print as
      `#S(POINT :X 1 :Y 2)`; `defstruct` takes `:print-function` and
      `:print-object`. Conditions print their report under `princ`.
    - `*print-escape* *print-base* *print-radix* *print-length*
      *print-level* *print-case*`, and `write` and `write-to-string` with the
      same keys. `prin1`, `princ`, `print` and `pprint` go through it.
    - The kernel's printer still prints anything that is plain data, so
      printing stays fast. A kernel primitive, `%holds-instance-p`, says when
      an object holds an instance somewhere; then the Lisp printer walks it,
      and `print-object` applies at any depth.
  - **`format`:**
    - `~mincol,colinc,minpad,padchar<...~;...~>` justification: one
      segment is right-justified, `:` and `@` pad at the ends, and the odd
      spaces go to the leftmost gaps.
    - `~<...~:>` logical blocks (they take a list argument).
    - `~W`, and `~/name/`, which calls a function. `~_` and `~I` are
      accepted; there is no pretty printer.
  - **`loop`:**
    - `for i below n` and `for i to n` (with `from 0` understood)
    - `with a = 1 and b = 2`
    - `of-type` and the simple type words (`for x fixnum in ...`)
    - `being the elements of` any sequence, and `loop-finish`
    - loop keywords written as keywords (`:for x :in ...`)
  - **Bits on integers of any size:**
    - `logand`, `logior`, `logxor` and `ash` work on two's complement of any
      length, in the kernel, with the fixnum fast path kept. `ash` right
      rounds down, as Common Lisp says.
    - `integer-length` and `logcount` are kernel primitives.
    - `lognot`, `logbitp`, `logtest`, `logandc1`/`logandc2`, `lognand`,
      `lognor`, `logeqv`, and bytes: `byte`, `ldb`, `dpb`, `mask-field` and
      `(setf ldb)`.
  - **Done-test:** `tests/lisp/cl4.lisp` (96 checks) starts with a small
    program in ordinary Common Lisp style:
    - it sieves primes with one big integer as its bit set (`ash`,
      `logbitp`, `logandc2`, `logcount`)
    - collects runs of primes in structures with `prog` and `go`
    - prints a report object through its own `print-object` and
      `print-unreadable-object`
    - lays out the lines with `~30<...~;...~>`
    It passes under the GC stress settings and in a saved world. Checked in
    the browser too: `#S` structures in the Listener and the Inspector, a
    `print-object` method, bignum bit operations in wasm, and `prog`/`go`.
  - Next: characters beyond ASCII (Unicode strings and a font for them), a
    pretty printer, `#S` reading, `*print-circle*`.
- **M15 — done (2026-11-20).** Characters beyond ASCII (`kernel/text.c`;
  the glyphs in `fonts/5x9.txt`).
  - **Characters and strings:**
    - A character is a Unicode code point, up to `#x10FFFF`.
    - A string is narrow (one byte per character) while every character in
      it is below 256, and wide (four bytes) otherwise. Functions that make
      strings from characters (`string-append`, `subseq`, `concatenate`,
      the reader) choose narrow when they can. Both kinds are `stringp`, and
      they compare `equal` and hash alike.
    - `make-string` and `make-array` of characters make wide strings, so any
      character can be stored in them. `:element-type 'base-char` makes a
      narrow one, and storing a wider character there is an error.
      `base-string` and `base-char` are types.
    - `copy-seq` of a wide string stays wide. `setf` of `char` and `schar`
      is new.
  - **Case and letters:** `char-upcase`, `char-downcase`, `upper-case-p`,
    `lower-case-p`, `both-case-p`, `alpha-char-p` and `graphic-char-p` are
    kernel primitives. They know ASCII, Latin-1, Latin Extended-A, Greek and
    Cyrillic as one-to-one pairs. `ß` and final `ς` have no case of their
    own, as in SBCL. Other scripts (CJK, kana, Hangul, Hebrew, Arabic) are
    letters without case. `string-upcase` and friends follow, and so do
    `char-equal`, `string-equal` and the character comparisons, which now
    take any number of arguments.
  - **UTF-8 everywhere outside the heap:**
    - the reader's buffers, what the printer writes, the native REPL's
      terminal, and the world's source built into the kernel
    - the files the editor reads and writes through the host (bytes that
      are not UTF-8 read as Latin-1, so nothing is lost)
    - `%utf8-encode` and `%utf8-decode` convert by hand.
    - Symbol names hash by their characters, so `straße` reads as the
      symbol `STRAßE` and `λ` as `Λ`, in any package.
  - **Reader and printer:**
    - `#\é`, `#\λ`, and any character by code as `#\U+00E9`.
    - More names: `Linefeed`, `Escape`, `Altmode`, `Nul`, `Delete`, `Bell`.
    - Control characters print as `#\U+0085`. `char-name` and `name-char`
      are new.
    - `read-from-string` positions count characters, not bytes.
  - **Keyboard:**
    - The host passes any character the keyboard types (`å`, `ö`, `ß`,
      dead keys composed by the browser, `λ` from a Greek layout) as its
      code.
    - The special keys (Return, Rubout, the arrows, F1–F4) moved from
      256–267, which are letters such as `Ā`, to just above `#x10FFFF`.
    - The Listener and Zmacs insert any graphic character.
    - Option is still Meta, so Option-key compositions on a US layout give
      Meta commands, as before.
  - **The font:** 5x9 gains 488 glyphs:
    - Latin-1 and Latin Extended-A, all of Western, Central and Northern
      Europe. `tools/mkfont.py` composes most of them from a base letter and
      a mark.
    - Capitals under a mark are squeezed into the small letters' five rows,
      or six under a diaeresis or dot, so `Ö` stays taller than `ö`.
    - Greek and Cyrillic, typographic punctuation (– — ‘ ’ “ ” … € •),
      arrows, mathematical symbols (∀ ∃ ∈ ≤ ≠ ⊂ ∧ ∞ ...), box drawing and
      blocks. Box-drawing glyphs fill the whole eleven-row line and join
      their neighbours.
    - A font's tables are now pages of 256 characters, and a missing
      character draws as `�`.
    - The kernel's own C font stays ASCII.
  - **Done-test:** `tests/lisp/cl5.lisp` (92 checks) starts with a program
    in a package named `wörter`. It counts the words of a text in Swedish,
    German, Greek, Russian and Polish in an `equal` hash table, ignoring
    case, prints a table with `format`, and reads its own output back.
    - The checks cover case in each script, wide and narrow strings,
      `make-string` with `setf char`, hash tables, the reader and printer,
      and UTF-8.
    - The screen tests type `å`, `ö` and Greek into the Listener, define and
      call a function named `σ`, and check the glyph widths. They also save
      `é λ Ж` from Zmacs and check the bytes on disk.
    - Checked in the browser: typed characters beyond ASCII, their glyphs,
      box drawing, and `σ`.
  - The small-heap test now runs in 300,000 words: the world grew by about
    45,000 words, mostly the three fonts' glyph tables.
  - Next: a pretty printer, `#S` reading, `*print-circle*`, `|...|` symbol
    syntax.
- **M16 — done (2026-11-20).** Common Lisp, part 5: the reader and the
  printer complete (`world/pretty.lisp`, `world/reader.lisp`; escapes and
  the new `#` syntax in `kernel/read.c`).
  - **Symbols with escapes:**
    - `|Hello World|`, `a\ b`, `||`: escaped characters keep their case and
      are never a package marker, and a token with escapes is never a
      number.
    - `prin1` writes a symbol with bars when it would not read back as
      itself: lower case, blanks, special characters, a colon, a name that
      looks like a number. `princ` writes the plain name.
  - **What the reader leaves to Lisp:**
    - `#S(name slot value ...)` makes a structure through its constructor,
      so the slot defaults apply. `defstruct` records the constructor.
    - `#n=` and `#n#` give shared and circular structure, in lists,
      vectors and structures.
    - `#.form` is evaluated at read time, unless `*read-eval*` is NIL.
    - The kernel's reader cannot call Lisp, so it leaves marker vectors and
      says so (`%read-fixups-p`). `%read-fixup` then builds the structures
      and `#.` values bottom up, and puts each labelled object where a `#n#`
      stood for it.
    - `%read-from-string`, which `read`, `read-from-string`, the Listener
      and the editor use, does this. So does the kernel's REPL, which
      evaluates such a form through `%read-fixup-eval`.
  - **`*print-circle*`:** a walk before printing finds the conses, vectors,
    arrays and instances met more than once. They print as `#1=` the first
    time and `#1#` after, shared list tails included (`(1 2 . #1#)`).
    `write` takes `:circle`, `:right-margin` and `:readably`.
  - **The pretty printer:**
    - `pprint-logical-block` (`:prefix`, `:suffix`, `:per-line-prefix`),
      `pprint-pop`, `pprint-exit-if-list-exhausted`, `pprint-newline`
      (`:linear :fill :miser :mandatory`), `pprint-indent`, `pprint-fill`,
      `pprint-linear`, `pprint-tabular`, and the dispatch table
      (`set-pprint-dispatch`, `pprint-dispatch`, `copy-pprint-dispatch`,
      with `(cons (eql op))` types).
    - Printing to a pretty stream builds a tree of logical blocks. The
      outermost one is laid out against the right margin when it ends:
      - a block that fits is printed flat
      - otherwise its linear newlines are taken
      - a fill newline is taken when the next section does not fit or the
        one before it broke
      - miser newlines are never taken
    - The margin is `*print-right-margin*`, else the Listener's width in
      characters, else 80.
    - Code is laid out the way it is written: `defun`, `defmacro`,
      `defmethod`, `let`, `flet`, `do`, `cond`, `case`, `if`, `when` and the
      other body forms, and function calls with their arguments under the
      first. `pprint` uses it, as does `write` with `:pretty t` or
      `*print-pretty*`.
    - `format`, under `*print-pretty*`:
      - `~<...~:>` is a logical block, with `~:<` for parentheses, `~@<`
        over the remaining arguments, and `~:@>` breaking at blanks in its
        text
      - `~_ ~:_ ~@_ ~:@_` are the conditional newlines, `~I` and `~:I` the
        indentation
    - `*print-pretty*` stays NIL by default, so the Listener prints as
      before. `(setq *print-pretty* t)` turns pretty printing on.
  - **Fixed on the way:** `list-length` (and the Listener's presentation of
    lists) looped forever on a circular list.
  - **Done-test:** `tests/lisp/cl6.lisp` (118 checks) starts with a program
    in a package of its own:
    - it keeps a graph with cycles in structures, writes it with
      `*print-circle*`, and reads it back with every edge the same object
    - it prints its own objects through `pprint-logical-block` at two
      margins
    - it pretty-prints a function definition, gets the layout it would be
      written in, and reads it back `equal`
    - The screen tests check that a Listener's margin is its width and
      that `pprint` there lays a definition out over several lines.
    - Checked in the browser: `pprint`, circular lists, `|Mixed Case|`
      and `#S` reading in the Listener.
  - The small-heap test now runs in 350,000 words; the world grew by about
    15,000 words.
  - Known limits:
    - `symbol-name` of a keyword includes its colon (`":FOO"`), as it
      always has here.
    - The Listener prints only a form's first value, so `(values)` shows
      `NIL`.
    - `*print-readably*` is accepted but adds nothing.
  - Next: the manual opens with a short history of the Lisp Machines, and
    the system gets a name.
- **M17 — done (2026-11-20).** Common Lisp, part 6: what ordinary programs
  still missed (`world/objects.lisp` rewritten; `world/cl3.lisp`;
  `symbol-macrolet` in the compiler).
  - **CLOS:**
    - **Multiple dispatch:** a method has a specializer for each required
      argument, a class or `(eql value)`. The applicable methods are ordered
      by their specializers, the first argument's first. Effective methods
      are cached by the classes of the arguments, except for generic
      functions with `eql` specializers. `defgeneric` takes `:method` and
      `:documentation`. New: `find-method`, `remove-method`,
      `method-specializers`, `ensure-generic-function` and
      `no-applicable-method`.
    - **Instances** are now `#(:instance class slots layout)`, with the slot
      values in a vector of their own, so an instance can change shape and
      keep its identity:
      - Defining a class again changes the class object in place; its
        subclasses are worked out again. Each instance takes on the new slots
        the next time it is used (`update-instance-for-redefined-class`), and
        added slots get their initforms.
      - `change-class` keeps the slots both classes have
        (`update-instance-for-different-class`).
    - **Unbound slots:** a slot without an initform or initarg is unbound.
      Reading one calls `slot-unbound`, which signals `unbound-slot`. New:
      `slot-makunbound`, `slot-exists-p` and `slot-missing`.
    - **Making instances follows Common Lisp:** `allocate-instance`, then
      `initialize-instance` with the initargs, then `shared-initialize`;
      also `reinitialize-instance`.
    - **`defclass` options:** `:default-initargs`, `:documentation`,
      `:allocation :class` (one value for the class and its subclasses),
      `:writer`, and repeated `:reader`/`:accessor`.
    - `with-slots` and `with-accessors`; `describe` goes through
      `describe-object`.
    - Structures are instances too, and their accessors index the slot
      vector.
  - **Forms:**
    - `symbol-macrolet` and `define-symbol-macro`, in the compiler; `setq`
      or `setf` of a symbol macro sets what it stands for.
    - `eval-when`, `progv` (on the VM's own special-binding stack),
      `load-time-value`, `define-modify-macro`, `ccase`, `ctypecase`, `remf`,
      `get-properties`, `sublis`, `map-into`, and `define-compiler-macro`
      (kept, not used).
    - **Lists with `:test` and `:key`:** `member`, `assoc`, `rassoc`,
      `adjoin`, `pushnew`, `union`, `intersection`, `set-difference` and
      `subsetp`, plus `member-if`. Without a test they still compare with
      `equal`.
    - `every`, `some`, `notany` and `notevery` take any sequences, one or
      several.
  - **Documentation:**
    - Doc strings are kept for `defun`, `defmacro`, `defgeneric`,
      `defclass`, `defvar`, `defparameter` and `defconstant`.
    - `(documentation 'name 'function)` and `(setf documentation)`.
  - **Tools:**
    - `trace` and `untrace`: the calls and values, indented by depth.
    - `time`: the milliseconds and collections.
    - `apropos` and `apropos-list`.
  - **The clock:** `get-universal-time`, `get-decoded-time`,
    `decode-universal-time` and `encode-universal-time`. The host gives the
    wall clock and the local time zone (two new imports in wasm, libc
    natively).
  - **Old warts fixed:**
    - `(symbol-name :foo)` and `(string :foo)` are `"FOO"`, and `princ`
      writes a keyword without its colon. The kernel still keeps the colon in
      the name; `%symbol-name` gives it.
    - The Listener prints every value of a form, each on its own line, and
      nothing for `(values)`; `/`, `//` and `///` hold the value lists.
    - Instances print with their class's package prefix, as in
      `#<LIBRARY:BOOK>`.
  - **Fixed on the way:**
    - A package could not export a symbol whose home is another package. An
      inherited CL name such as `item` came out internal, since COMMON-LISP
      holds every symbol of the world. A package now lists such symbols
      (`PK_EXTERNALS`), and external-ness is per package.
    - Two places where the collector could move an object a C function was
      still using; the GC stress run found both. One was in the reader's
      `#2A(...)`, from M13; the other was the new keyword `symbol-name`.
    - `every` returns T, not the predicate's last value.
  - **Done-test:** `tests/lisp/cl7.lisp` (101 checks) starts with a library
    program in its own package:
    - books, films and members as classes
    - loan periods by a generic function on two arguments
    - `with-slots` in its catalogue
    - its book class redefined with a new slot while books exist
    - a book changed into a film with `change-class`
    - `trace` and `time` on its own functions, and the documentation of
      what it defines
    - The screen tests check that the Listener prints both values of
      `(values a b)` and nothing for `(values)`.
    - Checked in the browser: multiple dispatch with an `eql` specializer,
      `trace`, both values of `floor`, the clock and doc strings.
  - Known limits:
    - `define-modify-macro`, `remf` and `pushnew` evaluate the place's
      subforms twice.
    - `slot-boundp` gives NIL for a missing slot instead of calling
      `slot-missing`.
    - There is no `step`.
- **M18 — done (2026-11-20).** Numbers and data, the rest (ratios in
  `kernel/bignum.c`; bit arrays in `kernel/array.c`; `world/cl4.lisp`).
  - **Ratios:**
    - `/` is true division: `(/ 7 2)` is `7/2`, `(/ 6 3)` is `2`,
      `(/ 4)` is `1/4`. A ratio is a heap object (`HT_RATIO`) holding the
      numerator and a denominator above 1, in lowest terms.
    - `+ - * /`, the comparisons, `rem`, `mod`, `floor`, `truncate`,
      `ceiling` and `round` work on ratios, and a float anywhere gives a
      float. A ratio compares with a float exactly, the float taken as the
      rational it is.
    - The reader reads `3/4`, and the printer writes ratios in any
      `*print-base*`.
    - `numerator`, `denominator`, `rational`, `rationalize` and the types
      `ratio`, `rational` and `real`. `expt` takes negative powers; time
      zones off the hour are ratios.
    - This changes a Lisp Machine Lisp way the system had kept: `/` on
      integers truncated. The world's own code uses `%quotient`, which keeps
      that behaviour, wherever it meant an integer quotient. That is 79 places,
      changed mechanically, so the screen and every window draw as before.
  - **Bit vectors:**
    - `#*1011` reads and prints. `(make-array n :element-type 'bit)` makes
      an array header whose data are bytes 0 and 1. It may be adjustable,
      with a fill pointer, or of any rank.
    - `bit` and `sbit`, `bit-and`, `bit-ior`, `bit-xor`, `bit-not` and the
      rest (into a new array, the first, or a given one), and
      `bit-vector-p`.
    - `equal` compares bit vectors bit by bit, and `subseq`, `copy-seq`,
      `reverse` and `coerce` keep them bit vectors.
  - **Pathnames:**
    - A structure that prints and reads as `#P"dir/name.type"`.
      `parse-namestring`, `make-pathname`, `merge-pathnames`, `namestring`
      and its parts, and `pathname-match-p` and `wild-pathname-p`, with `*`
      anywhere in a name.
    - `probe-file`, `truename` and `directory` look at the files the host
      has. `open`, `load` and `with-open-file` take pathnames.
  - **Byte streams:**
    - `open` with `:element-type '(unsigned-byte 8)`, `read-byte`,
      `write-byte`, `read-sequence` and `write-sequence`. In memory:
      `make-byte-input-stream` and `make-byte-output-stream`.
    - A file request can carry bytes that are not UTF-8, so a byte file
      keeps every byte, 128 and up too.
    - Under `make serve` a written file must already exist (the server only
      replaces world files); the browser's storage and a chosen folder
      take new files.
  - **Fixed on the way:**
    - `row-major-aref` on a bit array.
    - The Listener showed a bit vector as a vector of numbers.
  - **Done-test:** `tests/lisp/cl8.lisp` (145 checks) starts with a dice
    program in its own package:
    - it works out the odds of two and three dice exactly (1/36 up to 1/6,
      mean 7, variance 35/6)
    - finds primes with a bit vector
    - names its report files with `merge-pathnames`
    - writes the odds as bytes and reads them back
    - The screen tests write and read a byte file with bytes 0 to 255 on the
      file host, checking the bytes on disk, and see the host's files through
      `probe-file` and `directory`.
    - Checked in the browser: ratios, bit vectors, `#P` and `directory`.
  - Known limits:
    - A bit vector takes a byte per bit.
    - Pathnames know no hosts, devices or versions beyond keeping them.
    - There are no complex numbers.
- **M19 — done (2026-11-20).** Release: Lantern 1.0, ready for a public
  repository.
  - **Checked for private details:**
    - every tracked file, for local paths, user names, e-mail addresses and
      traces of the development sessions
    - the PDF's metadata, which holds only its title and date
    - One development note in this plan was reworded. The public repository
      starts from one clean commit, so the private history stays private.
  - **New files:**
    - `README.md`, with a screenshot (`docs/lantern.png`;
      `make docs/lantern.png` makes it again)
    - `LICENSE` (MIT)
    - `.github/workflows/ci.yml`, which builds and runs `make test` on Linux
      and macOS
  - **Portable:** the C flags add `-D_DEFAULT_SOURCE`, which Linux needs for
    `clock_gettime`, `localtime_r` and `opendir` under `-std=c11`.
  - **The Listener:** a form that returns no values (`pprint`, say) leaves
    the next one on a line of its own. The banner says Lantern 1.0.
  - **Done when:** a fresh clone of the public repository builds and passes
    `make test`. That was checked on macOS. The Linux build is CI's first run,
    after the push.
- **Name — 2026-11-20.** The system is called **Lantern**:
  - It shows on the manual's title page, in the Listener's banner, in the
    native REPL and in the browser tab.
  - The manual (`docs/OVERVIEW.md`, the book's first chapter) now opens with
    a short history: what the Lisp Machines were, who made them, and when
    they were in use, from the late 1970s to the early 1990s.

## Vision

A small, self-contained Lisp *environment* (not just a language) that runs in a
browser tab, inspired by the MIT CADR / Symbolics Lisp Machines:

- **Everything is Lisp, all the way down** — above a tiny C kernel, the window
  system, editor, inspector, debugger and REPL are written in the Lisp itself and
  can be inspected and redefined while running.
- **One live world** — no "program vs. OS" split. You save and resume the whole
  world (an image), like a Lisp Machine band.
- **The screen is made of live objects** — output stays connected to the objects
  that produced it ("presentations"): click a printed object to inspect, edit or
  act on it.
- **Mouse documentation line** — the bottom line always says what each mouse
  button will do right now.

This is *not* a historical reconstruction: no compatibility goal with Zetalisp
or Genera. We take the ideas and keep the system small enough for one person to
understand.

## Non-goals (for now)

- Running original MIT/Symbolics code.
- Full Common Lisp conformance (we can borrow CL names and semantics where it's
  convenient).
- Multi-user, networking, a native (non-browser) frontend. A native SDL frontend
  is cheap later, because the kernel only sees a framebuffer and an event queue.

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│  Browser                                                     │
│  ┌──────────────┐   events (key/mouse)   ┌────────────────┐  │
│  │  host.js     │ ─────────────────────▶ │  kernel.wasm   │  │
│  │  - <canvas>  │ ◀───────────────────── │  (C)           │  │
│  │  - IndexedDB │   framebuffer (shared  │                │  │
│  │  - rAF loop  │   linear memory)       │                │  │
│  └──────────────┘                        └────────────────┘  │
└──────────────────────────────────────────────────────────────┘

kernel.wasm (C, keep it small: target < 10k lines)
  ├─ object memory: tagged words, heap, symbols, conses, vectors, strings
  ├─ garbage collector (copying first, generational later)
  ├─ bytecode VM (stack machine) + primitives
  ├─ reader/printer (minimal; replaced by Lisp versions later)
  ├─ image save/load (heap snapshot)
  └─ device layer: framebuffer, BitBLT, event queue, clock, storage

world (written in the Lisp, loaded from source, then saved as an image)
  ├─ core: macros, backquote, setf, loops, conditions/restarts
  ├─ compiler: Lisp → bytecode (written in Lisp, bootstrapped)
  ├─ objects: a small Flavors/CLOS-like system (classes, generic fns, mixins)
  ├─ processes: green threads scheduled by the VM (cooperative → preemptive)
  ├─ graphics: fonts, BitBLT ops, lines, text
  ├─ window system: sheets, exposure, mouse tracking, who-line
  ├─ presentations: typed output records, mouse-sensitive regions, commands
  └─ tools: Listener, Editor (Emacs-like), Inspector, Debugger, Peek
```

### Key design decisions

| Topic | Decision | Why |
|---|---|---|
| Word size | 64-bit tagged words (low 3–4 bits tag) | Wasm has native i64; fixnums cheap |
| Execution | Bytecode VM in C | Simple, portable, debuggable; JIT to wasm is a far-later idea |
| GC | Cheney copying GC first | ~200 lines, compacting, easy image save |
| Display | 1-bit (or 8-bit) framebuffer in wasm memory, blitted to canvas each frame | The Lisp owns every pixel, like the real machines |
| Fonts | Bitmap fonts compiled into the image | Authentic look, no browser text dependency |
| Persistence | Image snapshots in IndexedDB/OPFS + export/import as a file | "Save world" / "Load world" |
| Concurrency | Wasm stays single-threaded; Lisp processes are green threads in the VM | Avoids SharedArrayBuffer/COOP headaches |
| Build | C → Emscripten (or clang `--target=wasm32` + tiny libc) | Also builds natively for fast testing |

### Keyboard

Map a "Space-cadet-ish" set onto a modern keyboard:
Control, Meta (Alt/Option), Super (Cmd), Hyper (configurable),
plus Abort (Esc-Esc or F1), Resume, Help, Rubout (Backspace), Select (F2) for
switching between activities.

## Milestones

Each milestone ends in something you can run and demo.

### M0 — Skeleton (week 1)
- Repo layout, Makefile, native build + wasm build.
- `host.js` + `index.html`: canvas, rAF loop, keyboard/mouse into an event ring buffer.
- C draws a test pattern into the framebuffer; the mouse moves a cursor.
- **Done when:** a browser tab shows a bitmap screen with a working mouse cursor.

### M1 — Tiny Lisp in C (weeks 2–4)
- Tagged objects, symbols/packages (one package to start), conses, fixnums, strings, vectors.
- Reader, printer, eval as a tree-walking interpreter (temporary).
- Cheney GC with precise roots.
- Native REPL in a terminal for fast iteration; test suite in Lisp + C.
- **Done when:** `(defun fact (n) ...)` works and the GC survives a stress test.

### M2 — Bytecode VM + compiler in Lisp (weeks 5–8)
- Design the bytecode (stack machine, closures, `unwind-protect`, non-local exits).
- Write the compiler in the Lisp itself; bootstrap it with the interpreter, then compile itself.
- Conditions & restarts (needed for a real debugger later).
- **Done when:** the compiler compiles itself and runs noticeably faster than the interpreter.

### M3 — Images (week 9)
- Save/load the heap as an image (native file + IndexedDB in the browser).
- Boot sequence: kernel.wasm + world image → instant start.
- **Done when:** reloading the browser tab resumes the saved world.

### M4 — Graphics & text (weeks 10–12)
- BitBLT primitives in C, exposed to Lisp. Everything else (lines, rectangles, text) in Lisp.
- Bitmap font format + one or two fonts (e.g. a CPTFONT-like fixed font, a proportional one).
- A full-screen Listener drawn by Lisp: echo, scrolling, input editing.
- **Done when:** the REPL runs in the browser, rendered entirely by Lisp code.

### M5 — Objects & processes (weeks 13–16)
- Object system: classes, multiple inheritance/mixins, generic functions, method combination (before/after at least).
- Green-thread processes, locks, `process-wait`, a scheduler driven by the host frame loop.
- **Done when:** two processes run concurrently and drawing windows can be expressed as mixins.

### M6 — Window system (weeks 17–22)
- Sheets (windows) with hierarchy, exposure/deexposure, saved bitmaps, borders, labels.
- Mouse handling: which window owns the mouse, blinkers, menus (pop-up, momentary).
- The **who-line / mouse documentation line** at the bottom of the screen.
- System menu, Select key to switch between activities.
- **Done when:** several Listeners in overlapping windows, moved and reshaped with the mouse.

### M7 — Presentations (weeks 23–26)
- Output records: printing an object keeps a link from the screen region to the object.
- Presentation types + mouse gestures → commands ("Inspect", "Describe", "Edit definition").
- Command processor with completion.
- **Done when:** clicking a printed object in the Listener opens it in the Inspector.

### M8 — Tools (ongoing)
- **Inspector** — browse any object, live.
- **Debugger** — backtrace, frames, locals, restarts, return-from-frame.
- **Editor** — Emacs-like buffers, Lisp mode, `meta-.` to find definitions, compile region/buffer.
- **Peek** — processes, memory, windows.
- **Done when:** you can fix a bug in the window system from inside the running system.

### M9 — Files: the system edits its own source
- `make serve` also serves the source: a small server (`tools/serve.py`, on
  localhost only) lists, reads and writes `world/*.lisp`.
- The machine asks the host through a request slot in the kernel; the host
  fetches and hands the reply back. Bytes pass through unchanged.
- Zmacs edits the files on disk: `c-X c-S` saves, Revert Buffer reads the file
  again, Compare with File shows what changed. Without a file server it falls
  back to the source the world was built from, as before.
- **Done when:** you fix a bug in the window system in Zmacs, save it, run
  `make`, and the fresh build has the fix.

### M10 — Self-contained: the machine as one file
- **Three ways to the files, chosen by the host:**
  - **Server** (`make serve`): `world/` on disk, as in M9.
  - **Browser storage:** without a server, saved files are kept in the
    browser (IndexedDB) next to the saved world. Files not saved yet come
    from the built-in source.
  - **A folder** (Chrome and Edge): choose the `world` folder once, and saves
    go straight to disk.
- **Upload and download:** `m-X Download File` saves a buffer to the
  Downloads folder. `m-X Upload Files`, or dropping `.lisp` files on the
  page, brings files in. Dialogs appear only when needed, and the mode line
  says where a buffer lives.
- **`lisp.html`:** `make` also builds one file with the kernel inside. It
  works opened straight from Finder, or from any plain web host.
- **Done when:**
  - `lisp.html` opened from disk boots, and its saved world survives a reload.
  - A buffer saved there survives a reload.
  - Download File produces the file, and dropping a `.lisp` file onto the
    page loads it into Zmacs.

### M11 — Common Lisp, part 1: the data a program needs
- **Bignums** in the kernel: integers of any size, read, printed and
  computed (`+ - * / rem mod expt gcd`). The VM's fixnum fast path stays;
  overflow moves to bignums.
- **Multiple values:** `values`, `multiple-value-bind`, `multiple-value-list`,
  `nth-value`.
- **Hash tables** (EQ, EQL, EQUAL), correct across the moving collector.
- **`defstruct`, `format` and `loop`**, the subsets programs use most.
- **Sequences and strings:** `sort`, `subseq`, `concatenate`, `position`,
  `find`, `count`, `remove-duplicates`, `reduce` with `:key`, `parse-integer`,
  case conversion, `string<`, ... with `:test` and `:key`.
- **Done when:** a program written in ordinary Common Lisp style runs
  unchanged. `tests/lisp/cl.lisp` counts words with a hash table, sorts them
  and prints a table with `format`, computes `(fact 100)` exactly, and uses
  `loop`, `defstruct` and multiple values.
- Floats come after this.

### M12 — Common Lisp, part 2: floats, local functions, packages
- **Floats** (doubles) in the kernel: read, printed so they read back the
  same, and computed with contagion; `sqrt`, `exp`, `log`, the trigonometric
  functions, and `~F`/`~E`/`~$` in `format`.
- **`flet`, `labels` and `macrolet`** in the compiler.
- **Packages:** `defpackage`, `in-package`, `export`, `import`,
  `use-package`, `find-symbol`, `pkg:name` and `pkg::name` in the reader and
  the printer. The system is the COMMON-LISP package; the Listener starts in
  COMMON-LISP-USER.
- **Done when:** a small library in a package of its own, with floats and
  local functions, loads unchanged and is used from CL-USER.
  `tests/lisp/cl2.lisp` does that.

### M13 — Common Lisp, part 3: forms, arrays, conditions, input
- `do`, `destructuring-bind` (also in `defmacro` lambda lists), supplied-p
  parameters, `setf` with several places and user-defined places.
- Arrays of any rank, fill pointers, `vector-push-extend`, `adjust-array`,
  `coerce`, `#2A(...)`.
- Condition classes: `define-condition`, `make-condition`, `handler-bind`,
  `signal`, `warn`, with `handler-case` and the kernel's own errors in the
  same scheme.
- Input streams and `read`, `read-line` from strings and from the Listener;
  `load`; `#+` and `#-`.
- **Done when:** a program that reads its data with `read`, keeps it in a
  two-dimensional array and recovers from bad records through its own
  condition and a restart runs unchanged. `tests/lisp/cl3.lisp` does that.

### M14 — Common Lisp, part 4: control, printing, bits
- `tagbody` and `go`, `prog`.
- A printer in Lisp: `print-object`, `#S(...)` for structures, the
  `*print-...*` variables, `write`.
- `format`: `~<...~>`, `~W`, `~/fn/`; the `loop` clauses still missing.
- `logand`, `logior`, `logxor`, `ash`, `logcount`, `integer-length`, `ldb`
  and `dpb` on integers of any size.
- **Done when:** a program that keeps a bit set in one big integer, prints
  its structures and its own objects, and loops with `prog` and `go` runs
  unchanged. `tests/lisp/cl4.lisp` does that.

### M15 — Characters beyond ASCII
- Characters are Unicode code points. Strings hold any of them, narrow
  (Latin-1) when they can and wide otherwise; the two kinds are the same to
  programs.
- Case and the letter tests for Latin, Greek and Cyrillic.
- UTF-8 at every edge: the reader, the printer, files, the terminal and the
  built-in source.
- The keyboard types any character; the special keys move out of the way.
- The 5x9 font covers Latin-1, Latin Extended-A, Greek, Cyrillic, common
  symbols and box drawing.
- **Done when:** a program that counts the words of a text in several
  languages, ignoring case, in a package with a non-ASCII name, runs
  unchanged; and text beyond ASCII can be typed in the Listener and in
  Zmacs, shown, and saved to disk as UTF-8. `tests/lisp/cl5.lisp` and the
  screen tests do that.

### M16 — Common Lisp, part 5: the reader and the printer complete
- `|...|` and `\` in symbols, read and printed.
- `#S(...)` reading, `#n=` and `#n#`, `#.`; `*print-circle*`.
- A pretty printer: logical blocks, conditional newlines and indentation,
  the dispatch table, code laid out as it is written, and `format`'s
  `~<...~:>`, `~_` and `~I`.
- **Done when:** a program that keeps a graph with cycles in structures,
  writes it and reads it back with its sharing, prints its own objects
  through logical blocks, and pretty-prints its source runs unchanged.
  `tests/lisp/cl6.lisp` does that.

## The road to 1.0

M0–M10 were the original plan. M11–M16 brought the language close to
Common Lisp, one milestone at a time. Three more finish it, and then
Lantern is 1.0:

### M17 — Common Lisp, part 6: what ordinary programs still miss
- CLOS: `with-slots`, `with-accessors`, unbound slots (`slot-unbound`,
  `slot-makunbound`), `slot-exists-p`, `change-class`, `find-method`,
  `remove-method`, `describe-object`, `initialize-instance` methods.
- Forms: `eval-when`, `symbol-macrolet`, `define-symbol-macro`,
  `define-modify-macro`, `progv`, `load-time-value`, `ccase`, `remf`,
  `get-properties`, `sublis`, `nsubst`, `map-into`, `define-compiler-macro`
  (accepted).
- Tools: `trace` and `untrace`, `time`, `apropos`, `documentation` (doc
  strings kept for functions, variables and classes), `describe` through
  `describe-object`.
- The clock: `get-universal-time`, `get-decoded-time`,
  `decode-universal-time`, `encode-universal-time`.
- Old warts: `(symbol-name :foo)` is `"FOO"`; the Listener prints every
  value of a form, and none for `(values)`.
- **Done when:** a program that keeps its records in classes it redefines
  and changes the class of, traces and times its own functions, and reads
  the documentation of what it defines, runs unchanged.
  `tests/lisp/cl7.lisp` does that.

### M18 — Numbers and data, the rest
- Ratios: `(/ 1 2)` is `1/2`. This changes a Lisp Machine Lisp way kept so
  far (`/` on integers truncated); the system's own code keeps it under the
  name `%quotient`.
- Bit vectors; pathnames mapped onto the host's files; byte streams.
- **Done when:** a program that computes exact odds with ratios, keeps a
  set in a bit vector, names its files with pathnames and writes bytes runs
  unchanged. `tests/lisp/cl8.lisp` does that.

### M19 — Release: a clean public repository (done)
- README, a license, CI running `make test`, `lantern.html` as a download.
- The tree and the history checked for personal details (local paths,
  e-mail addresses); a decision on keeping the history or starting the
  public repository from one clean commit.
- **Done when:** a stranger can clone it, run `make` and `make test`, and
  open Lantern in a browser, and nothing in it is private.

## Repository layout (proposed)

```
lisp/
  PLAN.md
  Makefile
  kernel/          C sources: mem.c gc.c vm.c read.c print.c image.c dev.c
  host/            index.html host.js (canvas, events, storage)
  world/           Lisp sources, loaded in order by world/boot.lisp
    core/          macros, setf, conditions, ...
    compiler/
    objects/
    graphics/      bitblt wrappers, fonts
    windows/
    present/
    tools/         listener, inspector, debugger, editor
  fonts/           bitmap fonts (source + converter)
  tests/           C unit tests + Lisp test files
  tools/           build helpers (font converter, image dumper)
```

## Risks & open questions

- **Scope creep** — the window system and editor can eat infinite time. Keep each
  milestone demoable; resist polishing before M7.
- **Debuggability of wasm** — always keep the native build working, and do the hard
  debugging there (ASan, lldb).
- **GC + bytecode correctness** — precise roots and stack maps are the classic
  bug sources; write a GC stress mode (collect on every allocation) early.
- **Performance** — the bytecode VM should be fine for a 1990-style UI; profile
  before optimising.
- **Name** — settled: **Lantern** (2026-11-20). It avoids the historical
  trademarks (Genera, Symbolics, Lambda, Explorer).
- **Open:** 1-bit monochrome (authentic) or 8-bit greyscale (antialiasing, nicer fonts)?
- **Open:** how much Common Lisp to follow — names only, or real CL semantics?

## First concrete steps

1. `kernel/` + `host/` skeleton with framebuffer test pattern (M0).
2. Decide the tag scheme and write `mem.h`.
3. Native REPL with reader/printer/eval and the GC stress mode.
