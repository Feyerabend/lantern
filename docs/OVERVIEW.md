# Lantern

**Lantern** is a small Lisp environment in the spirit of the MIT and
Symbolics Lisp Machines, running in a browser tab.

## The Lisp Machines, briefly

The Lisp Machines were personal workstations built to run Lisp all the way
down. The language, the operating system, the window system and the tools
were all one Lisp world that could be changed while it ran.

- **Where they came from:** the MIT AI Lab built the first ones. CONS was
  built from 1974 and was working by 1977. The CADR followed in the late
  1970s; about 25–30 were made.
- **Who sold them:** Lisp Machines Inc. (LMI, founded 1979) and Symbolics
  (1980) sold their first machines in 1981, and Texas Instruments followed in
  the mid-1980s. Perhaps 7,000 had been sold by 1988. The buyers were mostly AI laboratories, universities,
  and companies doing research and defence work.
- **When they were in use:** from the late 1970s to the early 1990s.
  - They peaked in the mid-1980s.
  - The market collapsed between about 1987 and 1990, once ordinary Unix
    workstations running Common Lisp caught up on price and speed.
  - Symbolics machines, and from about 1993 Open Genera running in software
    on the DEC Alpha, stayed in use for some years after that.

Lantern follows their ideas, not their hardware. The last chapter tells the
story in detail and compares each part of Lantern with its original.

**The name.** Lantern is a small light carried from those machines into the
browser. It avoids the names that still belong to others: Genera, Symbolics,
Lambda, Explorer.

## The machine

Lantern runs in a browser tab. A C kernel compiled to WebAssembly provides memory,
a garbage collector, a bytecode VM and a framebuffer. Everything above it is
meant to be written in Lisp and changeable while it runs.

![The debugger in a Listener, after a division by zero: the restarts, then the backtrace, whose frames and arguments are presentations. `:Frame 0` shows the failing frame's locals by name. Everything on the screen is drawn by Lisp.](screen.png)

## What you see

A window system, drawn entirely by Lisp (`world/windows.lisp`):

- **Windows** that overlap, each with its own bitmap and usually its own
  process. Click to select one (its label turns black and it gets the
  keyboard). Drag the label to move it and the corner grip to reshape it.
- **Mouse-R** opens the system menu: Create Listener, Editor, Peek, Move,
  Reshape, Bury, Refresh, Kill, Select next window, and the demos.
- **Listener windows**: read-eval-print loops, as many as you like. A form can
  span several lines; Rubout deletes, c-U clears the input. **Abort** (F1) stops the selected window's computation at any
  instruction; **Select** (F2) goes to the next window.
- **Mouse documentation line**: what the buttons do where the pointer is.
- **Who-line**: the selected process's state (`Tyi`, `Run`, …), the selected
  window, heap use, collections, uptime, processes, screen size, last key.
- **Presentations**: values the Listener prints stay connected to their
  objects, down to each element of a list. Move the mouse over one and it is
  outlined; **Mouse-L** opens it in the **Inspector**, **Mouse-R** offers its
  commands (Describe, Show Definition, Disassemble, Use as Input). In the
  Inspector every part is presented too, so you can click your way through
  the world.
- **Commands**: `:Inspect *windows*`, `:Describe 'car`, `:Show-Definition 'fact`;
  Tab completes a command name, Help (F4) lists them.
- **The debugger**: an error opens it right where it happened, with the
  failed frames still on the stack. It lists the restarts and a backtrace.
  Click a frame to see its locals, or a restart to take it. `:Retry` calls a
  frame again (with its newest definition), and `:Return form` makes it
  return a value. Abort (F1) leaves. If the error is in a window's process,
  it gets a Debugger window. If it is in the Screen Manager, a new Screen
  Manager takes over meanwhile.
- **Zmacs**, the editor (system menu "Editor", or `(ed 'name)`): the world's
  own source files, with Emacs keys. `m-.` finds any definition, and
  **`c-sh-C` compiles the one at the cursor into the running system**:
  change a window method and the windows change. In the debugger,
  `:Edit-Frame` takes you to the failing function. Under `make serve` the
  files are the real ones on disk: **`c-X c-S` saves to `world/`**, so the
  next `make` builds your change in. `m-X Compare with File` and `Revert
  Buffer` are there too. Without a server, files are saved in the browser
  (or in a folder you choose, in Chrome and Edge). `m-X Download File` and
  `m-X Upload Files`, or dropping `.lisp` files on the page, move them in
  and out.
- **One file:** `build/web/lisp.html` is the whole machine in a single file,
  about 970 KB. Open it from disk or put it on any web host: no server is
  needed. Both editions are also on the web, on GitHub Pages:
  [lisp.html](https://feyerabend.github.io/lantern/lisp.html) and
  [plexi.html](https://feyerabend.github.io/lantern/plexi.html).
- **Peek** (system menu): the processes with what they wait for and how much
  of the machine each uses, the windows, and memory, refreshed every second.
  Mouse-R on a process or window offers Kill Process, Select Window and so on.
- `(demo)` opens a clock, a bouncing ball and a prime finder, each driven by
  its own process; `(graphics-demo)` a window of lines, circles, grey levels
  and fonts.
- **Colour:** "Colour screen" in the system menu, or `(set-color-scheme
  :color)`, shows the screen through a colour map. **Plexi** (system menu,
  or `(plexi)`) is a neural network editor; see below.

**The world is saved.** About 1.5 s after each command, the whole Lisp
world (every function, variable and object, the Listener's text, and every
running process) is written to the browser's storage as an image. Reloading the page brings it back in a
fraction of a millisecond. `(disk-save)` saves at once, `(forget-world)` makes
the next start fresh, Cmd+S downloads the world as a file, and dropping a
`.world` file on the page loads it. On the Lisp Machines this was a *band*.

The screen fills the browser window. Each pixel of the machine is drawn as an
exact block of 2×2 or 3×3 screen pixels, never smoothed. Cmd + and Cmd − change
the zoom, and Cmd 0 restores it.

## Plexi, in colour

In 1989 Symbolics sold Plexi for the 3600 and the XL400, "an extensible,
graphics-based development environment for constructing neural networks".
Lantern has one of its own, written in Lisp in `world/plexi.lisp`. The
page `plexi.html` is the same machine as `lisp.html` with a colour screen:
it starts in the Plexi editor and keeps a saved world of its own.

![Plexi: the brochure's own network, INPUT-IMAGE, COMPETE-LAYER and OUTPUT-LAYER, learning the letters of PLEXI. A noisy I comes in and a clean one goes out; the Hinton diagram shows what each competing cell has learned, and the strip chart the error falling.](plexi.png)

A network is layers of cells joined by bundles of links. Each layer has a
transfer function, an update type (all at once, one cell at a time in no
order, or competitive) and an end condition; each bundle has a
connectivity pattern and a learning law. The editor draws the layers as
grids of cells, with the input at the bottom, and shows the network
working: the activations as coloured cells, as on the brochure's cover, the
links into the selected layer as a Hinton diagram (a square for each link,
its area the size of the weight), and the error, or a memory's energy, as a
strip chart. A click on an input cell flips it; a click on an attribute
moves it to the next choice.

| Network | What it does |
|---|---|
| Hopfield Memory | 100 cells, each linked to all the others, store the letters P, L, E, X and I and recall them from noisy copies, one cell at a time (Hopfield, 1982). The letters overlap too much for Hebb's rule, so the links come from the projection rule (Personnaz, Guyon and Dreyfus, 1986); switch the learning law to Hebbian and E is lost. |
| Competition | The brochure's example: INPUT-IMAGE, COMPETE-LAYER, OUTPUT-LAYER. The competing cells learn the letters from noisy images, untaught (Kohonen, with DeSieno's conscience, 1988), and the output layer is taught what each should produce (counter-propagation, Hecht-Nielsen, 1987). |
| Back Propagation | The 8-3-8 encoder (Rumelhart, Hinton and Williams, 1986): three hidden cells must find a code for eight patterns. |

Networks are built in Lisp with `make-network`, `add-layer` and `connect`,
and `*plexi-network*` is the one on show. As in the original, the
paradigms can be extended: `(deftransfer :name (x) ...)` adds a transfer
function and `(deflearning-law :name (net bundle) ...)` a learning law, and
both are in the attribute menus at once.

**The colour screen.** The framebuffer is still 8 bits of ink per pixel.
`*color-map*`, 768 bytes, gives each of the 256 inks a red, green and blue,
and the kernel reads it whenever it shows the screen; without it, ink is
darkness. A program asks for `(color-ink r g b)` or `(ink :orange)`, and
each colour takes the free ink nearest its own darkness, so a colour picture
still reads on the black-and-white screen. The multiples of 17 stay the
sixteen greys.

## Keyboard

| Lisp Machine key | Here |
|---|---|
| Control | Ctrl |
| Meta | Alt / Option |
| Super | Cmd |
| Hyper | not mapped yet |
| Abort / Select / Resume / Help | F1 / F2 / F3 / F4 (Abort: the selected window; Select: next window) |
| Altmode | Esc |
| Rubout | Backspace |

## Architecture

| Layer | What it is | Where |
|---|---|---|
| Hardware | the browser's WebAssembly engine, or native for tests | — |
| "Microcode" | C kernel, about 8,300 lines, no libc | `kernel/` |
| Memory | 64-bit tagged words, heap *indices*, Cheney copying GC | `mem.c`, `obj.c` |
| Text | Unicode characters, narrow and wide strings, UTF-8, case | `text.c` |
| Interpreter | explicit-stack machine: never recurses in C, can be suspended | `eval.c` |
| VM | stack bytecode, about 50 opcodes, catch/unwind records, special bindings | `vm.c` |
| "Macrocode" | bytecode produced by the Lisp compiler | — |
| Compiler | written in Lisp: macro expansion, code generation, assembly | `world/compiler.lisp` |
| Prelude | macros and list functions | `world/boot.lisp` |
| Worlds | images of the heap: save, check, load | `image.c` |
| Graphics microcode | `bitblt`, rectangles, characters from a font raster | `gprims.c` |
| Screen, input | framebuffer and its colour map, event queue, mouse cursor | `kernel.c`, `fb.c` |
| Streams | host, Listener and string streams; all printing | `world/streams.lisp` |
| Printer | `print-object`, `*print-circle*`, the pretty printer | `world/printer.lisp`, `world/pretty.lisp` |
| Graphics | bitmaps, lines, circles, fonts built from glyph data | `world/graphics.lisp` |
| Objects | classes, mixins, generic functions, method combination | `world/objects.lisp` |
| Processes | lightweight processes, locks; scheduled by the kernel | `world/processes.lisp`, `kernel.c` |
| Window system | windows, mixins, compositing, mouse, menus, who-line | `world/windows.lisp` |
| Listener | Listener windows, each with its own process | `world/listener.lisp` |
| Presentations | presented output, commands, the Inspector | `world/presentations.lisp` |
| Demo | clock, ball, primes and graphics windows | `world/demo.lisp` |
| Colour | colour inks, ramps, the colour map, colour schemes | `world/color.lisp` |
| Plexi | networks, learning laws, the network editor | `world/plexi.lisp` |
| Host | canvas, keyboard and mouse, zoom, the edition | `host/host.js` |

**Boot.** The interpreter runs `boot.lisp` and `compiler.lisp`. The compiler
then compiles everything defined so far, itself included (112 functions), and
becomes the evaluator: from then on every form typed, and every `eval`, is
compiled first. With building the fonts, all of this takes about 60 ms.

**Suspendable by design.** Machine state is a handful of registers plus one
Lisp stack. The browser runs a budget of 800,000 evaluator steps per frame,
about 5 ms, so the page stays live during long computations, and F1 aborts
anywhere.

## The language

A Lisp-2 in the MacLisp / Lisp Machine Lisp tradition, with Common Lisp names:

- `defun`, `defmacro`, `lambda` with `&optional` (any default expression) and `&rest`
- lexical closures; special variables with `defvar` (shallow binding)
- `if` with an implicit-`progn` else, as in Lisp Machine Lisp; `cond`,
  `and`, `or`, `case`, `when`, `unless`
- `let`, `let*`, `setq`, `setf` on common places, `push`, `pop`, `incf`
- `while`, `dolist`, `dotimes`, the extended `loop` (for/in/from/across/being,
  collect/sum/count/maximize, when/unless, finally ...), `block`/`return-from`/`return`
- `catch`/`throw`, `unwind-protect`
- conditions: `error`, `handler-case`, `handler-bind`, `ignore-errors`;
  restarts: `restart-case`, `invoke-restart`, `cerror`
- multiple values: `values`, `multiple-value-bind`, `multiple-value-list`, `nth-value`;
  `floor`, `ceiling`, `truncate`, `round` return quotient and remainder
- `&key` arguments
- objects: `defclass` with mixins, `make-instance`, `defgeneric`, `defmethod`
  with `:before`, `:after`, `:around` and `call-next-method`; methods on
  several arguments and on `(eql value)`, unbound slots, class slots,
  `change-class`, redefining a class while instances of it exist,
  `with-slots`, `initialize-instance` methods
- processes: `process-run-function`, `process-wait`, `process-sleep`,
  `with-lock`
- presentations and commands: `present`, `inspect`, `define-command`
- backquote, `disassemble`, `compile`, `eval`, property lists, `gensym`
- integers of any size (fixnums up to 61 bits, bignums beyond, up to
  262,144 bits), strings, characters, vectors
- Unicode characters and strings: `#\é`, `#\U+03BB`, `"Grüße, κόσμε"`,
  case in Latin, Greek and Cyrillic, symbols such as `straße` and `λ`;
  UTF-8 in files and at the terminal; the font has Latin-1, Latin
  Extended-A, Greek, Cyrillic, symbols and box drawing
- hash tables (`eq`, `eql`, `equal`), `defstruct`, `format` (`~A ~S ~D ~:D ~X
  ~% ~& ~T ~{~} ~[~] ~(~)` ...), sequence functions with `:test` and `:key`
  (`sort`, `find`, `position`, `remove-duplicates`, `reduce`, `search` ...),
  string functions (`string-upcase`, `string<`, `parse-integer` ...)
- floats (IEEE doubles): `1.5`, `1e10`, `sqrt`, `exp`, `log`, `sin` ...,
  `~F`/`~E`/`~$` in `format`; mixed arithmetic gives a float
- local functions: `flet`, `labels`, `macrolet`
- packages: `defpackage`, `in-package`, `export`, `import`, `use-package`,
  `find-symbol`, `pkg:name` and `pkg::name`; the Listener starts in CL-USER
- `declare` and `the` are accepted (and ignored)
- `do`, `destructuring-bind` (also as `defmacro` lambda lists), supplied-p
  parameters, `setf` on several places and on places of one's own
  (`defsetf`, `(defun (setf f) ...)`)
- arrays of any rank, fill pointers, `vector-push-extend`, `adjust-array`,
  `coerce`, `#2A((1 2) (3 4))`
- condition classes: `define-condition`, `make-condition`, `handler-bind`,
  `signal`, `warn`, the standard types (the kernel's own errors are
  `type-error`, `division-by-zero` ...)
- input: `read`, `read-line`, `read-char` from string streams and from the
  Listener's keyboard; `load`; `#+` and `#-`
- `tagbody`/`go`, `prog`; `print-object`, structures printed as `#S(...)`,
  `*print-base*`, `*print-length*` and the rest, `write`; `format`'s
  `~<...~>`, `~W`, `~/fn/`; `logand`, `ash`, `ldb`, `dpb` ... on integers of
  any size
- symbols with escapes (`|Hello World|`); `#S(...)`, `#1=`/`#1#` and `#.`
  in the reader; `*print-circle*`; a pretty printer with logical blocks
  (`pprint`, `pprint-logical-block`, `format`'s `~<...~:>` and `~_`) that
  lays code out the way it is written
- `symbol-macrolet`, `eval-when`, `progv`, `load-time-value`,
  `define-modify-macro`; `member`, `assoc` and the set functions with
  `:test` and `:key`; doc strings and `documentation`; `trace`, `time`,
  `apropos`; `get-universal-time` and `decode-universal-time`
- ratios (`(/ 7 2)` is `7/2`), bit vectors (`#*1011`, `bit-and` ...),
  pathnames (`#P"name.type"`, `merge-pathnames`, `directory`) and byte
  streams (`read-byte`, `write-byte`)
- Lisp Machine Lisp ways kept: `member`, `assoc` and `remove` compare with
  `equal` unless given a test

```
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(fact 15)                       => 1307674368000
(fact 30)                       => 265252859812191058636308480000000
(disassemble 'fact)
  0   LREF 0
  2   FIX 2
  4   LT
  5   JMPNIL -> 11
  8   FIX 1
  10  RET
  11  GFUNC 0  ; *
  14  LREF 0
  16  GFUNC 1  ; FACT
  19  LREF 0
  21  SUB1
  22  CALL 1
  24  TAILCALL 2
```

## Numbers

| | |
|---|---|
| Boot from source, including the self-compile and fonts | about 60 ms |
| Boot from a saved world | about 1 ms; a world is about 200 KB |
| Evaluator speed | about 160 million steps/s in wasm |
| Compiled vs. interpreted | about 3× faster (fib 3.6×, tak 5×, list work 2.7×) |
| Tests | 1,364 Lisp checks, plus error, world-image, screen and rebuilt-world tests |
| Stress | 133,000+ verified garbage collections without a fault |
| Kernel size | about 710 KB of WebAssembly with the world's source inside, about 8,300 lines of C |

## Status and road map

| Milestone | State |
|---|---|
| M0 Skeleton: screen, mouse, keyboard in the browser | done |
| M1 Tiny Lisp: tagged memory, GC, reader, printer, evaluator | done |
| M2 Bytecode VM and a compiler written in Lisp | done |
| M3 Saving and loading world images | done |
| M4 Graphics and text drawn by Lisp | done |
| M5 Object system and processes | done |
| M6 Window system | done |
| M7 Presentations | done |
| M8 Debugger with restarts, the Zmacs editor, Peek (the Inspector came with M7) | done |
| M9 Files: Zmacs reads and saves `world/*.lisp` under `make serve` | done |
| M10 Self-contained: `lisp.html`, files in the browser, upload and download | done |
| M11 Common Lisp, part 1: bignums, multiple values, hash tables, `defstruct`, `format`, `loop`, sequences | done |
| M12 Common Lisp, part 2: floats, `flet`/`labels`/`macrolet`, packages | done |
| M13 Common Lisp, part 3: `do`, `destructuring-bind`, arrays, condition classes, input streams and `read`, `#+`/`#-` | done |
| M14 Common Lisp, part 4: `tagbody`/`go`, `print-object` and `#S(...)`, `format ~<~>`, bit operations on bignums | done |
| M15 Characters beyond ASCII: Unicode strings, UTF-8, case, the keyboard, a font with 583 glyphs | done |
| M16 Common Lisp, part 5: symbols with escapes, `#S` reading, `#n=`/`#n#`, `*print-circle*`, a pretty printer | done |
| M17 Common Lisp, part 6: multiple dispatch, unbound slots, `change-class`, `symbol-macrolet`, `trace` and `time`, the clock | done |
| M18 Numbers and data: ratios, bit vectors, pathnames, byte streams | done |
| M19 Release: Lantern 1.0, a clean public repository, README, MIT license, CI | done |
| M20 Colour, and Plexi: a colour map, a neural network editor, `plexi.html`, GitHub Pages | done |

## Building and running

```
make            # build the browser version into build/web/
make serve      # http://localhost:8742/, and world/*.lisp to the editor
open build/web/lisp.html   # the whole machine in one file, no server
open build/web/plexi.html  # the same, in colour, with Plexi open
make lisp       # native REPL: ./build/lisp  (--world FILE to keep a world)
make test       # all tests
make pdf        # this document
```
