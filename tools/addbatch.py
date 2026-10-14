#!/usr/bin/env python3
"""Add batch program names to RUNPGM's allowed list (both the 88-level
VALUES and the ALLOWED message), keeping lines inside column 72.
Usage: python3 tools/addbatch.py PGM [PGM...]"""
import re
import sys

path = "src/RUNPGM.cbl"
src = open(path).read()
m = re.search(r"(88  PGM-ALLOWED\s+VALUES)(.*?)(\.\n)", src, re.S)
names = re.findall(r"'([\w-]+)'", m.group(2))
for n in sys.argv[1:]:
    if len(n) > 8:
        sys.exit(f"{n}: program names are at most 8 characters")
    if n not in names:
        names.append(n)
lines, cur = [], ""
for n in names:
    piece = f"'{n}'"
    if len(cur) + len(piece) + 1 > 22:
        lines.append(cur)
        cur = piece
    else:
        cur = (cur + " " + piece).strip()
lines.append(cur)
values = "".join("\n" + " " * 49 + l for l in lines)
src = src[:m.start(2)] + values + src[m.end(2):]
# ALLOWED message: rebuild the DISPLAY literal list
m2 = re.search(r"(DISPLAY 'ALLOWED: )(.*?)(\n\s+MOVE 16 TO RETURN-CODE)", src, re.S)
words = " ".join(names).split()
chunks, cur = [], "ALLOWED:"
for w in words:
    if len(cur) + len(w) + 1 > 44:
        chunks.append(cur + " ")
        cur = w
    else:
        cur += " " + w
chunks.append(cur)
disp = "DISPLAY " + ("\n" + " " * 23).join(f"'{c}'" for c in chunks)
src = src[:m2.start()] + disp + src[m2.end(2):]
open(path, "w").write(src)
print("allowed:", " ".join(names))
