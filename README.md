# Lantern

A Lisp Machine of your own, in a browser tab.

Lantern is a small Lisp environment in the spirit of the MIT CADR and the
Symbolics Lisp Machines. A C kernel, compiled to WebAssembly, gives it
tagged memory, a copying garbage collector, a bytecode VM and a screen.
Everything above that is written in Lisp and can be read and changed while
it runs: the compiler (which compiles itself at boot), the window system,
the Listeners, the Zmacs editor, the Inspector, the debugger with restarts,
and presentations that keep printed output connected to its objects.

The original LM-2 brochure highlights its remarkably high capacity for
the early 1980s: https://www.bitsavers.org/pdf/symbolics/brochures/LM-2.pdf
Our own interpretation in Latern:

![Lantern: a Listener, the clock, a bouncing ball and a prime finder, each a window with its own process](docs/lantern.png)

The language is most of Common Lisp: packages, CLOS with multiple dispatch,
conditions and restarts, `loop`, `format` and the pretty printer, bignums,
ratios and floats, arrays and bit vectors, Unicode strings, pathnames and
byte streams.

## Try it

```
make            # build the browser version into build/web/
make serve      # http://localhost:8742/, with world/*.lisp served to the editor
```

Or open `build/web/lisp.html`: the whole machine in one file, which works
straight from disk or from any plain web host. A saved world comes back
when you reload the page.

Some keys to start with:

|---|---|
| Mouse-R | the system menu: new Listeners, the editor, Peek, the demos |
| F1 | Abort: stop what the selected window is doing |
| F2 | Select: the next window |
| F4 | Help: the Listener's commands |
| `(demo)` | the clock, the bouncing ball and the prime finder |
| `(ed 'name)` | Zmacs, at the definition of `name` |

## Building

You need clang with the `wasm32` target, `wasm-ld` (from LLVM's lld), and
Python 3. No Emscripten, no libc in the kernel.

- **macOS:** `brew install llvm lld` (`wasm-ld` comes with lld). If your
  clang has no WebAssembly target, use LLVM's:
  `make CLANG=$(brew --prefix llvm)/bin/clang`.
- **Linux:** `apt install clang lld python3`.

```
make            # the browser version
make lisp       # a native REPL: ./build/lisp
make test       # the Lisp test suites (also under ASan/UBSan, a small heap and
                # GC stress), error reports, world images, and the screen tests
```

`make pdf` builds the manual from `docs/*.md`. It needs
[Tectonic](https://tectonic-typesetting.github.io) and the fonts the book
uses (Charter, Helvetica Neue, Menlo, as on macOS); the built PDF is in the
repository.

## Reading

- [The manual](docs/A-Lisp-Machine-in-the-Browser.pdf) (PDF): the preface,
  the machine, and the history of the Lisp Machines.
- [docs/OVERVIEW.md](docs/OVERVIEW.md): what Lantern is, how it is built,
  and the language.
- [docs/LISP-MACHINES.md](docs/LISP-MACHINES.md): the original machines,
  and how each part of Lantern compares with them.
- [PLAN.md](PLAN.md): how it was built, milestone by milestone.

## The source

|---|---|
| `kernel/` | the C kernel: memory, garbage collector, reader, printer, interpreter, VM, graphics, files |
| `world/` | the Lisp world, loaded in order at boot: compiler, objects, Common Lisp, windows, tools |
| `host/` | the browser side (`host.js`, `index.html`) and the native REPL |
| `fonts/` | the bitmap font, as text |
| `tests/` | the Lisp test suites and the screen tests |
| `tools/` | build helpers, the development file server, the screenshot tool |
| `docs/` | the manual and its sources |

## License

MIT; see [LICENSE](LICENSE).
