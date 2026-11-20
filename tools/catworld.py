#!/usr/bin/env python3
"""Concatenate the world's Lisp files, each preceded by ";;;; FILE name".

The editor (world/editor.lisp) splits the embedded source at those lines to
show it as files again.

usage: catworld.py file.lisp ... > world.lisp
"""
import os
import sys


def main():
    for path in sys.argv[1:]:
        sys.stdout.write(f";;;; FILE {os.path.basename(path)}\n")
        with open(path, encoding="utf-8") as f:
            text = f.read()
        sys.stdout.write(text if text.endswith("\n") else text + "\n")


if __name__ == "__main__":
    main()
