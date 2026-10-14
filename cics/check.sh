#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Offline syntax check for the CICS programs (there is no CICS here).
#
# What a CICS translator would do is imitated just enough for GnuCOBOL to
# compile the rest: every EXEC CICS ... END-EXEC block becomes CONTINUE,
# DFHRESP(x) becomes its numeric value, and the EIB stub is put at the top
# of the LINKAGE SECTION. This proves the COBOL around the CICS commands
# (data names, copybooks, paragraph logic) is valid. It does NOT prove the
# CICS commands themselves are right - only a real translator can.
# ---------------------------------------------------------------------------
set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
rc=0
for src in "$HERE"/src/*.cbl; do
    name="$(basename "$src")"
    python3 - "$src" "$WORK/$name" <<'EOF'
import re, sys
text = open(sys.argv[1]).read()
resp = {"NORMAL": 0, "NOTFND": 13, "DUPKEY": 15, "USERIDERR": 69,
        "NOTAUTH": 70}
# comment lines (col 7 = '*') may mention EXEC CICS - keep them out
text = "\n".join(l.replace("EXEC CICS", "EXEC-CICS")
                  if len(l) > 6 and l[6] == "*" else l
                  for l in text.split("\n"))
text = re.sub(r"DFHRESP\((\w+)\)", lambda m: str(resp[m.group(1)]), text)
text = re.sub(r"EXEC\s+CICS\b.*?END-EXEC", "CONTINUE", text, flags=re.S)
text = text.replace("       LINKAGE SECTION.\n",
                    "       LINKAGE SECTION.\n           COPY DFHEIBLK.\n", 1)
open(sys.argv[2], "w").write(text)
EOF
    if cobc -fsyntax-only -I "$HERE/copy" -I "$HERE/stub" \
            -I "$HERE/../copybooks" "$WORK/$name"; then
        echo "  OK    $name"
    else
        echo "  FAIL  $name"; rc=1
    fi
done
exit $rc
