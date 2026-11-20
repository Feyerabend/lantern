#!/usr/bin/env python3
"""Convert the subset of Markdown used in docs/ to LaTeX for docs/book.tex.

Handles: # ## ### headings, paragraphs, - and 1. lists (nested by indentation), tables,
> quotes, ``` code blocks, --- rules, ![caption](image), and inline `code`,
**bold**, *italic*, [text](url).

usage: md2tex.py <in.md> <out.tex>
"""
import re
import sys

SPECIALS = {
    "\\": r"\textbackslash{}", "&": r"\&", "%": r"\%", "$": r"\$", "#": r"\#",
    "_": r"\_", "{": r"\{", "}": r"\}", "~": r"\textasciitilde{}", "^": r"\textasciicircum{}",
}


def escape(s):
    return "".join(SPECIALS.get(c, c) for c in s)


def inline(s):
    out = []
    for i, part in enumerate(re.split(r"`([^`]*)`", s)):
        if i % 2:   # code span
            out.append(r"\texttt{" + escape(part).replace("-", "-{}") + "}")
            continue
        t = escape(part)
        # links; anchors inside the document keep only their text
        t = re.sub(r"\[([^\]]+)\]\(([^)]+)\)",
                   lambda m: m.group(1) if m.group(2).startswith(r"\#")
                   else r"\href{" + m.group(2).replace("\\_", "_") + "}{" + m.group(1) + "}", t)
        t = re.sub(r"\*\*(.+?)\*\*", r"\\textbf{\1}", t)
        t = re.sub(r"(?<![\w*])\*(?!\s)(.+?)(?<!\s)\*(?![\w*])", r"\\emph{\1}", t)
        t = re.sub(r'"([^"]*)"', "“\\1”", t)          # "x" -> curly quotes
        t = re.sub(r"\\textasciitilde\{\}(?=\d|ms)", "≈", t)   # ~24-bit -> approx. sign
        out.append(t)
    return "".join(out)


def heading(level, text):
    cmd = {1: "chapter", 2: "section", 3: "subsection"}[level]
    t = inline(text)
    return "\\%s*{%s}\n\\addcontentsline{toc}{%s}{%s}\n" % (cmd, t, cmd, t)


def table(rows):
    cells = [[c.strip() for c in r.strip().strip("|").split("|")] for r in rows]
    header, body = cells[0], cells[2:]
    n = len(header)
    has_header = any(h for h in header)
    # first column narrower when there are 3+ columns
    # a short first column (years, row labels) stays narrow; the rest share the width
    first = "l" if max(len(r[0]) for r in cells if r) < 22 else "Y"
    spec = "@{}" + first + "Y" * (n - 1) + "@{}"
    lines = [r"{\small", r"\begin{xltabular}{\linewidth}{%s}" % spec, r"\toprule"]
    if has_header:
        lines.append(" & ".join(r"\textbf{%s}" % inline(h) for h in header) + r" \\")
        lines.append(r"\midrule")
        lines.append(r"\endhead")
    for r in body:
        r = (r + [""] * n)[:n]
        lines.append(" & ".join(inline(c) for c in r) + r" \\")
    lines += [r"\bottomrule", r"\end{xltabular}}", ""]
    return "\n".join(lines)


def convert(md):
    out = []
    lines = md.split("\n")
    i = 0
    para = []
    lists = []          # open list environments, innermost last

    def flush_para():
        if para:
            out.append(inline(" ".join(para)) + "\n")
            para.clear()

    def close_lists(to=0):
        while len(lists) > to:
            out.append(r"\end{%s}" % lists.pop())

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()

        if stripped.startswith("```"):
            flush_para(); close_lists()
            code = []
            i += 1
            while i < len(lines) and not lines[i].strip().startswith("```"):
                code.append(lines[i])
                i += 1
            out.append(r"\begin{Verbatim}[fontsize=\small,frame=leftline,framesep=4mm,xleftmargin=2mm]")
            out.extend(code)
            out.append(r"\end{Verbatim}")
            i += 1
            continue

        if not stripped:
            flush_para(); close_lists()
            i += 1
            continue

        m = re.match(r"^(#{1,3}) (.*)$", line)
        if m:
            flush_para(); close_lists()
            out.append(heading(len(m.group(1)), m.group(2)))
            i += 1
            continue

        if stripped == "---":
            flush_para(); close_lists()
            out.append(r"\medskip\noindent\rule{\linewidth}{0.4pt}\medskip" + "\n")
            i += 1
            continue

        m = re.match(r"^!\[([^\]]*)\]\(([^)]+)\)$", stripped)
        if m:
            flush_para(); close_lists()
            out.append(r"\begin{figure}[htbp]\centering")
            out.append(r"\includegraphics[width=\linewidth]{%s}" % m.group(2))
            out.append(r"\caption*{\small %s}" % inline(m.group(1)))
            out.append(r"\end{figure}" + "\n")
            i += 1
            continue

        if stripped.startswith("|"):
            flush_para(); close_lists()
            rows = []
            while i < len(lines) and lines[i].strip().startswith("|"):
                rows.append(lines[i])
                i += 1
            out.append(table(rows))
            continue

        if stripped.startswith(">"):
            flush_para(); close_lists()
            quote = []
            while i < len(lines) and lines[i].strip().startswith(">"):
                quote.append(lines[i].strip()[1:].strip())
                i += 1
            out.append(r"\begin{quote}\small " + inline(" ".join(quote)) + r"\end{quote}" + "\n")
            continue

        m = re.match(r"^(\s*)([-*]|\d+\.) (.*)$", line)
        if m:
            flush_para()
            kind = "itemize" if m.group(2) in "-*" else "enumerate"
            depth = 1 + len(m.group(1)) // 2
            close_lists(depth)
            if len(lists) == depth and lists[-1] != kind:
                close_lists(depth - 1)
            while len(lists) < depth:
                out.append(r"\begin{%s}" % kind)
                lists.append(kind)
            item = [m.group(3)]
            i += 1
            # continuation lines: indented, not a new item
            while i < len(lines) and lines[i].strip() and not re.match(r"^\s*([-*]|\d+\.) ", lines[i]) \
                    and lines[i].startswith("  "):
                item.append(lines[i].strip())
                i += 1
            out.append(r"\item " + inline(" ".join(item)))
            continue

        close_lists()
        para.append(stripped)
        i += 1

    flush_para(); close_lists()
    return "\n".join(out) + "\n"


def main():
    with open(sys.argv[1], encoding="utf-8") as f:
        md = f.read()
    with open(sys.argv[2], "w", encoding="utf-8") as f:
        f.write(convert(md))


if __name__ == "__main__":
    main()
