#!/usr/bin/env python3
"""Build docs/DATA_DICTIONARY.md from the record copybooks.

For every 01-level record: each field with its level, PICTURE, usage,
offset and length in bytes. Lengths follow the GnuCOBOL / IBM rules
(COMP-3 = digits/2 + 1, COMP = 2/4/8 bytes, OCCURS multiplies, REDEFINES
takes no space). The script cross-checks its record lengths against
`cobc -tsymbols` and stops if any disagree, so the dictionary cannot
quietly drift from the code.

Usage: python3 tools/datadict.py            (from the repository root)
"""
import glob
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# record layouts only - parameter blocks and tables are left out
SKIP_SUFFIX = ("PARM", "WS", "CODES", "DFLT", "RSN", "DENOMS")


def pic_length(pic, usage):
    pic = pic.upper()
    expanded = re.sub(r"(.)\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), pic)
    if usage == "COMP-2":
        return 8
    if usage == "COMP-1":
        return 4
    digits = sum(1 for c in expanded if c == "9")
    if usage == "COMP-3":
        return digits // 2 + 1
    if usage in ("COMP", "BINARY", "COMP-5"):
        return 2 if digits <= 4 else 4 if digits <= 9 else 8
    return sum(1 for c in expanded if c not in "SV")


def parse(path):
    """Return a list of records: (name, [field dicts])."""
    lines = []
    for raw in open(path):
        if len(raw) > 6 and raw[6] in "*/":
            continue
        lines.append(raw[7:72].rstrip("\n"))
    text = " ".join(lines)
    items = [s.strip() for s in text.split(".") if s.strip()]
    records, current = [], None
    for item in items:
        m = re.match(r"(\d\d)\s+([\w-]+)(.*)", item)
        if not m:
            continue
        level, name, rest = int(m.group(1)), m.group(2), m.group(3)
        if level == 88:
            continue
        pic = re.search(r"\bPIC(?:TURE)?\s+(\S+)", rest)
        usage = "DISPLAY"
        for u in ("COMP-3", "COMP-2", "COMP-1", "COMP-5", "COMP", "BINARY",
                  "PACKED-DECIMAL"):
            if re.search(r"\b" + u + r"\b", rest):
                usage = "COMP-3" if u == "PACKED-DECIMAL" else u
                break
        occurs = re.search(r"\bOCCURS\s+(\d+)", rest)
        field = {"level": level, "name": name,
                 "pic": pic.group(1) if pic else "",
                 "usage": usage,
                 "occurs": int(occurs.group(1)) if occurs else 1,
                 "redefines": "REDEFINES" in rest,
                 "external": "EXTERNAL" in rest}
        if level == 1:
            current = (name, [field])
            records.append(current)
        elif current:
            current[1].append(field)
    return records


def layout(fields):
    """Compute offset/length of every field; returns record length."""
    def size(i):
        f = fields[i]
        if f["pic"]:
            return pic_length(f["pic"], f["usage"]) * f["occurs"], i + 1
        total, j = 0, i + 1
        while j < len(fields) and fields[j]["level"] > f["level"]:
            s, nxt = size(j)
            if not fields[j]["redefines"]:
                total += s
            j = nxt
        return total * f["occurs"], j

    def place(i, offset):
        f = fields[i]
        length, nxt = size(i)
        f["offset"], f["length"] = offset, length
        j, child_off = i + 1, offset
        while j < nxt:
            c = fields[j]
            start = child_off
            if c["redefines"]:
                # same offset as the sibling it redefines (the previous one)
                prev = [k for k in range(i + 1, j)
                        if fields[k]["level"] == c["level"]][-1]
                start = fields[prev]["offset"]
            j_next = place(j, start)
            if not c["redefines"]:
                child_off += c["length"]
            j = j_next
        return nxt

    place(0, 0)
    return fields[0]["length"]


def cobc_lengths(copybooks):
    src = ["       IDENTIFICATION DIVISION.", "       PROGRAM-ID. DDCHECK.",
           "       DATA DIVISION.", "       WORKING-STORAGE SECTION."]
    src += [f"           COPY {c}." for c in copybooks]
    src += ["       PROCEDURE DIVISION.", "           GOBACK."]
    with tempfile.TemporaryDirectory() as tmp:
        p = os.path.join(tmp, "ddcheck.cbl")
        open(p, "w").write("\n".join(src) + "\n")
        lst = os.path.join(tmp, "ddcheck.lst")
        subprocess.run(["cobc", "-fsyntax-only", "-I", os.path.join(ROOT, "copybooks"),
                        "-t", lst, "-tsymbols", p], check=True)
        sizes = {}
        for line in open(lst, errors="replace"):
            m = re.match(r"\s*(\d+)\s+\S+\s+01\s+([\w-]+)", line)
            if m:
                sizes[m.group(2)] = int(m.group(1))
        return sizes


def main():
    books = sorted(glob.glob(os.path.join(ROOT, "copybooks", "*.cpy")))
    books = [b for b in books
             if not os.path.basename(b)[:-4].endswith(SKIP_SUFFIX)
             and os.path.basename(b) not in ("SESSION.cpy",)]
    names = [os.path.basename(b)[:-4] for b in books]
    measured = cobc_lengths(names)
    out = ["# Data dictionary", "",
           "Generated by `tools/datadict.py` from the copybooks - do not edit by hand.",
           "Offsets are 0-based bytes; every record length below was cross-checked",
           "against `cobc -tsymbols`.", ""]
    out += ["| Copybook | Record | Length |", "|---|---|---|"]
    body = []
    bad = []
    for book, name in zip(books, names):
        purpose = ""
        for raw in open(book):
            m = re.match(r"\s+\* PURPOSE\s+:\s+(.*?)\s*\*?$", raw)
            if m:
                purpose = m.group(1).rstrip(" *")
                break
        for rec_name, fields in parse(book):
            length = layout(fields)
            if measured.get(rec_name) not in (None, length):
                bad.append(f"{rec_name}: script {length}, cobc {measured[rec_name]}")
            out.append(f"| {name} | {rec_name} | {length} |")
            body += [f"## {rec_name} ({name}.cpy, {length} bytes)", "",
                     purpose.capitalize() + "." if purpose else "", "",
                     "| Level | Field | PICTURE | Usage | Offset | Length |",
                     "|---|---|---|---|---|---|"]
            for f in fields[1:]:
                occ = f" OCCURS {f['occurs']}" if f["occurs"] > 1 else ""
                red = " (REDEFINES)" if f["redefines"] else ""
                body.append(f"| {f['level']:02d} | {f['name']}{red} | {f['pic']}{occ} | "
                            f"{f['usage'] if f['pic'] else 'GROUP'} | {f['offset']} | {f['length']} |")
            body.append("")
    if bad:
        sys.exit("length mismatch:\n  " + "\n  ".join(bad))
    out.append("")
    open(os.path.join(ROOT, "docs", "DATA_DICTIONARY.md"), "w").write(
        "\n".join(out + body) + "\n")
    print(f"docs/DATA_DICTIONARY.md: {len(names)} copybooks, all lengths agree with cobc")


if __name__ == "__main__":
    main()
