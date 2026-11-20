#!/usr/bin/env python3
"""Convert a text bitmap font (fonts/5x9.txt) into a C header, or into Lisp
source (when OUT ends in .lisp) that world/graphics.lisp turns into fonts.

The file is UTF-8. One glyph per line: the character (or U+XXXX), a space,
then the rows separated by '|', using '#' for ink and '.' for paper. Row 0
is the top of a capital; HEIGHT rows make the glyph's body, and a glyph
may go on below it to fill the whole line (box drawing). Missing rows at
the bottom are blank. Rows are WIDTH wide, or one more for glyphs that
fill the cell to its edge and join their neighbours (box drawing).

Beyond ASCII (M15) a glyph may also be made of others:

  é = e acute        a base glyph with marks over (or under) it
  Α = A              the same picture as another glyph

and these lines define the parts:

  @acute 0 ...#.|..#..   a mark: its name, the row it starts at, its rows
  ^A .###.|#...#|...     a capital squeezed into the five rows of a small
                         letter, to make room for a mark above it; or into
                         six rows, for marks one row high (Ö is taller than ö)

A mark that starts above row 2 sits on the squeezed form of a capital (any
base with ink in rows 0-1 must have one): the six-row form when every such
mark is dots in row 0 only (diaeresis, dot) and there is one, else the
five-row form. Lines whose second character is
not a space and that are not one of these are comments.

The C header has the ASCII glyphs only (the kernel's own text before the
world is up); the Lisp source has all of them.

usage: mkfont.py <font.txt> <out> <name> <width> <height>
"""
import re
import sys


def parse_char(field):
    if re.fullmatch(r"U\+[0-9A-Fa-f]{4,6}", field):
        return chr(int(field[2:], 16))
    if len(field) != 1:
        raise ValueError(f"bad character {field!r}")
    return field


def main():
    src, out, name, width, height = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4]), int(sys.argv[5])
    max_rows = height + 2              # a whole line: the body and the gap below it
    glyphs, order, marks, squeezed, recipes = {}, [], {}, {}, []

    def rows_of(text, where, wide_ok=True):
        rows = text.split("|")
        for row in rows:
            if len(row) not in ((width, width + 1) if wide_ok else (width,)) or set(row) - {"#", "."}:
                sys.exit(f"{where}: bad row {row!r}")
        return rows

    with open(src, encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            line = line.rstrip("\n")
            where = f"{src}:{lineno}"
            if not line.strip():
                continue
            m = re.match(r"(U\+[0-9A-Fa-f]{4,6}) (.*)$", line)
            if m:
                ch, rest = chr(int(m.group(1)[2:], 16)), m.group(2)
            elif len(line) >= 2 and line[1] == " ":
                ch, rest = line[0], line[2:]
            elif line.startswith("@"):
                parts = line[1:].split()
                marks[parts[0]] = (int(parts[1]), rows_of(parts[2], where, False))
                continue
            elif line.startswith("^") and len(line) > 2 and line[2] == " ":
                rows = rows_of(line[3:], where, False)
                if len(rows) not in (5, 6):
                    sys.exit(f"{where}: a squeezed capital has 5 or 6 rows")
                squeezed.setdefault(line[1], {})[len(rows)] = rows
                continue
            else:
                continue                                   # a comment
            if ch in glyphs or any(r[0] == ch for r in recipes):
                sys.exit(f"{where}: duplicate glyph {ch!r}")
            order.append(ch)
            if rest.startswith("= "):
                parts = rest[2:].split()
                try:
                    base = parse_char(parts[0])
                except ValueError as e:
                    sys.exit(f"{where}: {e}")
                recipes.append((ch, base, parts[1:], where))
                continue
            rows = rows_of(rest, where)
            if len(rows) > max_rows:
                sys.exit(f"{where}: {ch!r} has {len(rows)} rows, max {max_rows}")
            glyphs[ch] = rows

    blank = "." * width
    for ch, base, mark_names, where in recipes:        # in order: a recipe may use an earlier one
        if base not in glyphs:
            sys.exit(f"{where}: no glyph {base!r} to build {ch!r} from")
        rows = list(glyphs[base]) + [blank] * (height - len(glyphs[base]))
        above = [marks[mk] for mk in mark_names if mk in marks and marks[mk][0] < 2]
        if above and "#" in rows[0] + rows[1]:
            forms = squeezed.get(base, {})
            # dots only (¨ ˙): a bar right on top of the letter would merge with it
            one_row = all(start == 0 and "#" not in "".join(mrows[1:]) and "##" not in mrows[0]
                          for start, mrows in above)
            if one_row and 6 in forms:
                rows = [blank] + forms[6] + rows[7:]
            elif 5 in forms:
                rows = [blank, blank] + forms[5] + rows[7:]
            else:
                sys.exit(f"{where}: {base!r} needs a squeezed form (^{base}) for a mark above it")
        for mk in mark_names:
            if mk not in marks:
                sys.exit(f"{where}: no mark named {mk!r}")
            start, mrows = marks[mk]
            rows += [blank] * (start + len(mrows) - len(rows))
            for k, mrow in enumerate(mrows):
                old = rows[start + k]
                rows[start + k] = "".join("#" if a == "#" or b == "#" else "." for a, b in zip(old.ljust(len(mrow), "."), mrow))
        glyphs[ch] = rows

    missing = [chr(c) for c in range(32, 127) if chr(c) not in glyphs]
    if missing:
        sys.exit(f"{src}: missing glyphs: {''.join(missing)!r}")

    if out.endswith(".lisp"):
        with open(out, "w", encoding="utf-8") as f:
            f.write(f";;; Generated by tools/mkfont.py from {src}. Do not edit.\n")
            f.write(f";;; One string per glyph: the character, a space, then its rows of\n")
            f.write(f";;; pixels ('#' ink, '.' paper) separated by '|'.\n\n")
            f.write(f"(defparameter {name}\n  '(")
            chars = [chr(c) for c in range(32, 127)] + [c for c in order if ord(c) >= 127]
            for i, c in enumerate(chars):
                rows = glyphs[c]
                while len(rows) > 1 and "#" not in rows[-1]:
                    rows = rows[:-1]
                text = (c + " " + "|".join(rows)).replace("\\", "\\\\").replace('"', '\\"')
                f.write(("" if i == 0 else "\n    ") + '"' + text + '"')
            f.write("))\n")
        return

    with open(out, "w", encoding="ascii") as f:
        f.write(f"/* Generated by tools/mkfont.py from {src}. Do not edit. */\n")
        f.write(f"static const unsigned char {name}[95][{height}] = {{\n")
        for c in range(32, 127):
            bits = [int(r.replace("#", "1").replace(".", "0"), 2) for r in glyphs[chr(c)][:height]]
            bits += [0] * (height - len(bits))
            row = ", ".join(f"0x{b:02x}" for b in bits)
            label = chr(c).replace("\\", "backslash")
            f.write(f"    {{ {row} }}, /* {label} */\n")
        f.write("};\n")


if __name__ == "__main__":
    main()
