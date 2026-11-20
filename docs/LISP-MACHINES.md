# Lantern and the Lisp Machines

A historical and technical reference: what the original Lisp Machines were,
how they worked, and how each part of this project lines up with them, where
it copies them, where it differs, and why.

> **On accuracy.** This chapter was first written from memory. In November
> 2026 its dates, names, counts and word formats were checked against the
> sources listed under [Sources checked](#sources-checked), mostly the
> Wikipedia articles on the machines and companies, MIT AI Lab documents,
> Kent Pitman's history of condition handling, and the 2005 CADR source
> release. Several details were corrected then. What the sources do not
> settle is marked: *approx.* for a figure, *unconfirmed* for a date or
> name. For anything you quote, go to the [references](#references).

---

## 1. History in brief

| When | What |
|---|---|
| 1958– | Lisp at MIT (McCarthy). By the late 1960s **MacLisp** on the PDP-10 is the AI Lab's main language. Large Lisp programs outgrow the PDP-10's 18-bit address space and time-sharing. |
| 1973 | Richard **Greenblatt** and Tom **Knight** start the MIT Lisp Machine project at the AI Lab: a single-user computer built for Lisp. |
| 1974–77 | The first machine, **CONS**, is built from 1974 (Greenblatt, Jack Holloway and Knight; Knight's design paper is dated 1974–75). The *LISP Machine Progress Report* (AI Memo 444, August 1977) describes it working. Only one was built. |
| late 1970s | The **CADR**, an improved CONS, follows (its AI Lab memo, AIM-528, is from 1979). About 25–30 are built: "about 25" sold according to one account, "over 30" by 1979 according to another. The software is **Lisp Machine Lisp**, with the window system, the **Zmacs** editor and **Chaosnet**. |
| 1979–80 | **Flavors**: Howard **Cannon**'s object system with mixins and method combination, in use on the Lisp Machine from late 1979. Weinreb and Moon describe it in AI Memo 602 (1980). The window system is written in it. |
| 1979–80 | Two companies are spun off from the AI Lab. **Lisp Machines Inc. (LMI)** is founded by Greenblatt in 1979. **Symbolics** is founded on 9 April 1980, with Russell Noftsker as a founder and most of the lab's hackers. Both license the MIT software. The split, and Richard Stallman's response to it, is one of the stories behind the start of GNU. |
| 1981 | The first products are repackaged CADRs. Symbolics' **LM-2** comes out in 1981; about 100 are built, at $70,000 each. LMI's first machines are sold as the **LMI-CADR**. |
| 1983 | The **Symbolics 3600** ships, a year late. It is a new architecture with a 36-bit word: 4 or 8 bits of tag, and 32 bits of data or a 28-bit address. Its software is named **Genera** around 1984 and becomes the best-known Lisp Machine environment. The **LMI Lambda** follows in 1983: processor cards for the NuBus-based NuMachine, about 200 sold. TI licenses the Lambda design from LMI in 1983. |
| mid-1980s | The **TI Explorer**, TI's variant of the LMI Lambda (its first-shipment year is *unconfirmed* here). *Common Lisp: the Language* (Steele, 1984) is shaped largely by Lisp Machine Lisp and MacLisp people. |
| 1987–88 | Symbolics introduces the **Ivory** single-chip Lisp processor (1987; the first Ivory machines, such as the MacIvory, come in 1988). TI's **Explorer II** (1987) is built on its own Lisp chip. Later the **MicroExplorer** puts an Explorer II on a NuBus card for the Macintosh II. LMI goes bankrupt in 1987. |
| late 1980s–early 1990s | The "AI winter", and general-purpose workstations running Common Lisp catch up on price and performance. Perhaps 7,000 Lisp Machines have been sold in all by 1988. Most makers are gone by the early 1990s. Symbolics files for Chapter 11 in January 1993 and lasts until 1996. |
| about 1993 | **Open Genera**: Ivory emulated in software on the DEC Alpha (the "VLM", virtual Lisp machine). This is the closest historical precedent to what Lantern does. |
| 2005– | In October 2005 MIT allows the CADR software (from tapes of about 1980) to be redistributed. Emulators follow: **usim** (CADR), **LambdaDelta** (LMI Lambda), **Meroko** (TI Explorer). The **LM-3** project keeps the MIT system alive and developing. **Portable Genera** (2021) runs Genera on x86-64 and Arm64. |

Also relevant, as a separate lineage: the **Xerox** Interlisp-D machines,
the Dolphin (1979), the Dorado and the Dandelion (1981), and the BBN
**Jericho**, which ran Interlisp but was never sold. They share the idea of
a personal Lisp workstation, but not the MIT/Symbolics design.

---

## 2. What made a Lisp Machine a Lisp Machine

1. **Tagged memory, checked in hardware.** Every word carried a data-type tag.
   Type checks, fixnum overflow and GC barriers ran *in parallel* with the
   operation, so safe, generic Lisp arithmetic cost almost nothing.
2. **Microcode.** The CADR was a microprogrammed machine with a *writable*
   control store. The Lisp compiler produced **macrocode**, a compact stack
   instruction set, and **microcode** interpreted it. Hot primitives such as
   `car`, `memq`, type dispatch, function call and the GC were written in
   microcode.
3. **One address space, one language.** No kernel/user split, no processes
   in the Unix sense. The OS, device drivers, network stack, window system,
   editor, compiler and user programs were all Lisp objects in one large
   virtual memory, all inspectable and redefinable while running.
4. **Persistent worlds.** The whole virtual memory could be saved to a disk
   partition (a **band**, or later a *world load*) and booted again, so the
   environment was a long-lived, evolving image rather than a program started
   from source.
5. **An integrated, mouse-driven environment.** A high-resolution bitmap
   display, a three-button mouse with a **mouse documentation line** (and
   **who-line**) always saying what the buttons do. Later came Symbolics
   **Dynamic Windows** with **presentations**: output that stays connected to
   the objects it shows.
6. **The keyboard.** The **Space-cadet** keyboard (Knight) and its successors
   had Control, Meta, Super, Hyper (plus Top, Greek, Front on the Space-cadet),
   and dedicated keys such as Abort, Resume, Help, Select, Rubout, Clear Input.

---

## 3. Architecture, side by side

The main analogy runs through the whole project:

| Lisp Machine | Lantern |
|---|---|
| hardware (CADR boards, 3600, Ivory) | the browser's **WebAssembly** engine |
| **microcode** (writable control store) | the **C kernel**: `kernel/vm.c`, `eval.c`, `mem.c` |
| **macrocode** produced by the Lisp compiler | **bytecode** produced by `world/compiler.lisp` |
| everything above: OS, windows, Zmacs… in Lisp | the same, in Lisp (M4 onward) |

Open Genera did exactly this on the Alpha: the "hardware" became a C program
(the VLM) interpreting Ivory instructions. We do the same with a smaller,
self-designed instruction set, and wasm as the host.

### 3.1 Words and tags

| | CADR | Symbolics 3600 | Lantern |
|---|---|---|---|
| Word | 32 bits | 36 bits | 64 bits |
| Tag | 5-bit data type + 2-bit cdr-code | 4 bits (fixnums) or 8 bits (pointers) | 3-bit low tag (`kernel/lisp.h`) |
| Address | 24-bit datum (an early memo gives a 23-bit virtual address space) | 28-bit word address | 61-bit word *index* into a semispace |
| Fixnum | 24-bit (*approx.*) | 32-bit | 61-bit |
| Checked by | hardware, in parallel | hardware, in parallel | software (`FIXNUMP` etc. in C) |
| Headers | object headers for arrays etc. | same | header word with type + length |

*Same:* every word is self-describing, so the GC is **precise** and type
errors are caught, never "undefined behaviour".
*Different:* our tag checks cost instructions. Pointers are **indices**, not
addresses, which makes saving an image (M3) a copy rather than a relocation.

### 3.2 Lists and cdr-coding

The Lisp Machines used **cdr-coding**: a list could be stored as a vector of
cars, with 2 bits per word saying "next", "nil follows" or "normal pointer
follows". This roughly halved list memory, at the price of "invisible
pointers" when such a list was destructively modified.

*Lantern:* plain two-word conses, no cdr-coding. Memory is cheap now,
and cdr-coding complicates `rplacd` and the collector. It could come back if
lists ever dominate the heap.

### 3.3 Garbage collection

| | CADR (later MIT/LMI) | Symbolics 3600 | Lantern |
|---|---|---|---|
| Algorithm | **Baker-style incremental copying** (Baker 1978), real-time-ish | incremental copying + **ephemeral (generational) GC** (Moon 1984) | **Cheney** stop-and-copy (1970), two semispaces |
| Hardware help | read barrier in microcode | tag/page hardware for barriers | none |
| Pauses | short, incremental | short | whole-heap pauses (fine at today's heap sizes, ~ms) |
| Debug aid | — | — | `--gc-verify`: checks every pointer before each collection |

*Plan:* a generational collector could come later. An incremental one
would matter once heaps reach hundreds of MB; Lantern's are 8 MiB per
semispace.

### 3.4 Instruction set and compiler

| | CADR | 3600 / Ivory | Lantern |
|---|---|---|---|
| Model | stack machine (the PDL) | stack machine | stack machine (`lisp_stack`) |
| Encoding | 16-bit macroinstructions | 17-bit instructions, two per 36-bit word (one source says 18) | byte-coded, 1-byte opcode + operands |
| Executed by | microcode | hardware (3600 microcode, Ivory direct) | C `switch` loop (`vm_run`) |
| Compiler | QCMP, in Lisp | Symbolics compiler, in Lisp | `world/compiler.lisp`, in Lisp |
| Self-hosting | yes | yes | yes: compiles itself at every boot |
| Inspect | `disassemble` | `disassemble` | `(disassemble 'f)` |

The VMs share the classic features: arguments and locals in a stack frame, a
constants vector per function, `car`/`cdr`/`+`/`eq` as single instructions, and
calls dispatched on the type of the callee.

Our difference: **closures**. The compiler moves only variables captured by an
inner `lambda` into heap environment vectors and keeps the rest in stack slots.
Lisp Machine Lisp's closures over specials, and its later lexical closures,
were implemented differently.

### 3.5 The interpreter

The Lisp Machines had both an interpreter (`eval`) and the compiler. Most
system code was compiled, but the interpreter was used constantly at the
listener. Lantern **compiles every listener form** before running it, as many
modern Lisps do. The interpreter is kept for bootstrapping (`%interpret`).

The interpreter is an **explicit-stack machine**, not a recursive C function.
That gives us what microcode gave the CADR "for free": deep recursion that
can't crash the host stack, and execution that can be suspended at any
instruction (the browser per-frame budget, F1 Abort).

### 3.6 Dynamic binding and non-local exits

The closest match of all:

| Lisp Machine | Lantern |
|---|---|
| **regular PDL** (push-down list): frames, arguments, temporaries | `lisp_stack` |
| **special PDL**: saved values for special-variable bindings (shallow binding) | `bind_stack` (`BIND`/`UNBIND`) |
| catch frames on the PDL; `*catch`/`*throw`, later `catch`/`throw` | `CATCH`/`THROW` records linked by `catch_top` |
| `unwind-protect` cleanups run during throws | `UWP`/`UNPROTECT`/`END-UWP` |

Shallow binding means a special variable's value is always in the symbol's
value cell. Binding saves the old value and restores it on exit or unwind.
That is the CADR/Genera approach, and ours.

### 3.7 Errors and conditions

On the Symbolics machines, Weinreb, Greenberg and Moon redesigned the error
system after the Multics PL/I condition system. The result, the **New Error
System**, had conditions as Flavors instances, handlers, *restarts* and
proceed types. Kent Pitman, then at Symbolics, wrote the proposals (1986)
that made it the basis of the Common Lisp condition system.

*Lantern:* errors are conditions, with classes as in Common Lisp
(`define-condition`, `handler-bind`, `handler-case`), and there are
**restarts and an interactive debugger** (M8, M13). They are what made the
Lisp Machine debugger feel magical: fix the bug, then *resume* the
computation.

### 3.8 Processes

Lisp Machines had lightweight **processes** built on **stack groups**
(coroutine stacks) and a scheduler, all inside the one address space.
Lantern's whole machine state is "registers + `lisp_stack`". Its processes
(M5) each keep their own stack and bindings, and the kernel switches between
them: the same idea as stack groups.

### 3.9 Memory, areas and paging

The real machines had **virtual memory** paged to disk, with **areas** for
grouping objects (and controlling GC). The browser gives Lantern linear
memory only: 64 MiB to start, of which the heap is two semispaces of 8 MiB.
Persistence comes from images (M3), not paging.

### 3.10 Display, input, windows

| | Lisp Machines | Lantern |
|---|---|---|
| Screen | monochrome bitmap, portrait (CADR *approx.* 768×900), later larger and colour options | a framebuffer as large as the browser window (up to 2560×1600), 8-bit ink shown in grey, drawn into a `<canvas>` |
| Who drew it | Lisp: the TV/window system, Dynamic Windows | Lisp (M4 on); the C kernel only before the world is up |
| Fonts | bitmap fonts (CPTFONT, Times, …), loaded from files | one hand-drawn 5×9 bitmap font (`fonts/5x9.txt`), fixed, proportional and bold, with Latin, Greek, Cyrillic and symbols (M15) |
| Mouse | 3 buttons, doc line, who-line | 3 buttons (L/M/R), doc line, who-line ✓ |
| Keyboard | Space-cadet etc. | Ctrl=Control, Alt/Option=Meta, Cmd=Super, F1 Abort, F2 Select, F3 Resume, F4 Help |
| Windows | sheets, exposure, blinkers, menus, mixins (Flavors) | windows with their own bitmaps and processes, mixins, menus (M6) |
| Presentations | Dynamic Windows / CLIM (Symbolics) | presentations, commands and the Inspector (M7) |

### 3.11 Persistence

A **band** (MIT/LMI) or **world load** (Symbolics) was a snapshot of virtual
memory on a disk partition. Bands were built incrementally (incremental
worlds on Genera) and could be booted at will. Microcode had its own bands.

*Lantern:* the heap is saved as an image (M3), in IndexedDB in the browser
or in a file natively. Heap pointers are indices, so loading is a memory
copy.

### 3.12 Networking and files

The MIT machines spoke **Chaosnet**, and later TCP/IP. They used remote file
servers (often a PDP-10 or another LM) with a generic pathname system.
Lantern has no network of its own. Its files come through the host (M9,
M10): a small file server under `make serve`, the browser's storage, or a
folder the user chooses; `load` and `open` read them.

---

## 4. The language: Lisp Machine Lisp vs. this Lisp

| Feature | Lisp Machine Lisp / ZetaLisp | Lantern (M18) |
|---|---|---|
| Ancestor | MacLisp | MacLisp-flavoured, CL names |
| Namespaces | Lisp-2 (value and function cells) | Lisp-2 ✓ |
| Scoping | dynamic by default early on; lexical closures later | lexical; specials via `defvar` |
| `if` | `(if test then else...)`: else is an implicit `progn` | same ✓ |
| Macros | `defmacro`, MacLisp-style **displacing** macros | `defmacro`; interpreter displaces, compiler expands once |
| Objects | **Flavors**: mixins, method combination, `defmethod` | CLOS: mixins, multiple dispatch, `:before`/`:after`/`:around`, `change-class` (M5, M17) |
| Numbers | fixnums, bignums, flonums, ratios, complex | fixnums, bignums, ratios, floats (doubles); no complex numbers |
| Strings/arrays | rich array system (art-q, art-string, displaced arrays…) | Unicode strings, arrays of any rank, fill pointers, displaced and adjustable arrays (M13, M15) |
| `&key`, `&aux` | yes | yes |
| Packages | yes | yes (M12) |
| Errors | condition system with restarts | condition classes, `handler-bind`, restarts, the debugger |
| Loop | `loop` macro (Burke/Moon) | the extended `loop` |
| Multiple values | yes | yes |

---

## 5. Why do it differently?

* **No custom hardware.** The machines' advantage was tag checking and
  microcode in parallel with execution. Today a general CPU running wasm is
  orders of magnitude faster than a CADR (*approx.* 1 MIPS) even when
  it does those checks in software. ~160 million bytecode steps per second in
  the browser is plenty for a 1980s-style environment.
* **One person, not a lab.** The C kernel is kept small; everything that
  *can* be Lisp *will* be Lisp, as on the original. That is also what made
  those systems so inspectable.
* **Simplicity over memory density.** No cdr-coding, no paging, a simple
  stop-and-copy GC. These are the cheapest things to add later if needed.
* **Suspendable by construction.** A browser tab must give control back every
  frame. Making the interpreter and VM explicit-stack machines turns that
  constraint into Lisp Machine–like behaviour: Abort works anywhere, and
  processes come almost for free.
* **Not a reconstruction.** For the real thing, run the original software in
  usim, LambdaDelta or Meroko (see §1). This project reuses the *ideas*,
  not the code.

---

## 6. Glossary

- **Band / world load**: saved image of virtual memory that the machine boots from.
- **Bucky bits**: the Control/Meta/Super/Hyper modifier bits.
- **cdr-coding**: compact list representation (see §3.2).
- **CADR / CONS**: the two MIT prototype machines (CADR: "the second CONS").
- **Chaosnet**: MIT local network protocol used by the machines.
- **Flavors**: Lisp Machine object system; ancestor of CLOS.
- **Invisible pointer / forwarding**: a word that redirects references, used
  by the GC and by cdr-coded lists. Our GC uses forwarding during collection only.
- **Macrocode / microcode**: compiled Lisp instructions / the firmware that runs them.
- **PDL**: push-down list, the stack (regular PDL and special PDL).
- **Stack group**: a coroutine with its own stacks; the basis of processes.
- **Who-line**: status line at the bottom of the screen (process, state, package, run bar).
- **Zmacs**: the Lisp Machine Emacs, written in Lisp (via EINE, "EINE Is Not Emacs",
  and ZWEI, "ZWEI Was EINE Initially").

---

## References

Primary and secondary sources worth reading. Check details here, not in this document.

- T. F. Knight, *CONS*, MIT AI Lab Working Paper 80, 1974 (*unconfirmed*:
  the series number was not checked).
- R. Greenblatt, *The LISP Machine*, MIT AI Lab Working Paper 79, 1974.
- A. Bawden, R. Greenblatt, J. Holloway, T. Knight, D. Moon, D. Weinreb,
  *LISP Machine Progress Report*, MIT AI Memo 444, 1977.
- D. Weinreb, D. Moon, *Lisp Machine Manual* (the "Chine Nual"), MIT, several
  editions 1979–1984.
- H. G. Baker, "List Processing in Real Time on a Serial Computer",
  *CACM* 21(4), 1978.
- C. J. Cheney, "A Nonrecursive List Compacting Algorithm", *CACM* 13(11), 1970.
- D. A. Moon, "Garbage Collection in a Large Lisp System", *ACM Conference on
  LISP and Functional Programming*, 1984.
- D. A. Moon, "Architecture of the Symbolics 3600", *ISCA* 1985, in *ACM
  SIGARCH Computer Architecture News* 13(3), pp. 76–83.
- H. I. Cannon, *Flavors: A Non-Hierarchical Approach to Object-Oriented
  Programming*, 1979 (unpublished; revised 1992–2007).
- D. Weinreb, D. Moon, *Flavors: Message Passing in the Lisp Machine*, MIT AI
  Memo 602, 1980.
- K. M. Pitman, *Error System Proposal #5*, X3J13, 1986.
- H. P. Newquist, *The Brain Makers*, 1994 (the market, and the 7,000
  machines).
- G. L. Steele Jr., R. P. Gabriel, "The Evolution of Lisp", *HOPL-II*, 1993.
- G. L. Steele Jr., *Common Lisp: The Language*, 1984 (2nd ed. 1990).
- K. M. Pitman, "Condition Handling in the Lisp Language Family", 2001.
- S. Levy, *Hackers: Heroes of the Computer Revolution*, 1984 (the AI Lab and
  the Symbolics/LMI split).
- Bitsavers.org: scanned Lisp Machine manuals, memos and Symbolics documentation.
- The LM-3 project and the usim CADR emulator; LambdaDelta (LMI Lambda);
  Meroko (TI Explorer). Running these is the best way to *see* the originals.

### Sources checked

The November 2026 check used these pages:

- Wikipedia: "Lisp machine", "Lisp Machines" (LMI), "Symbolics", "Texas
  Instruments Explorer", "Genera (operating system)", "Flavors (programming
  language)".
- Computer History Wiki: "CONS" (one machine, built from 1974); the Computer
  History Museum's catalogue (Knight's CONS document, dated 1975).
- The 1977 progress report (MIT AI Memo 444); an early MIT working paper
  and an LMI storage specification (the CADR's word: 2-bit cdr code, 5-bit
  type, 24-bit datum); the CADR microprocessor documentation (the 16-bit
  order code).
- Software Preservation Group: Cannon's Flavors paper (in use since late
  1979).
- Kent Pitman, "Condition Handling in the Lisp Language Family" (2001), and
  his 1986 error-system proposals (the New Error System).
- A court record of Symbolics' Chapter 11 filing (27 January 1993).
- LWN.net and unlambda.com (the CADR source release, October 2005; usim).
- WinWorld (Open Genera 1.0, 1993: one source only).

Still *unconfirmed*: the TI Explorer's first-shipment year, the
MicroExplorer's year, the CADR's display size and macro speed, and the
exact LambdaDelta and Meroko histories.
