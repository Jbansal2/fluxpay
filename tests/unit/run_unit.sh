#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# COBOL unit tests: each tests/unit/T*.cbl is a small driver that CALLs one
# subprogram with known inputs and checks the outputs with the UTPROC
# assertions. Every driver runs in its own empty work directory (with an
# empty data/), so tests never touch real data and never depend on each
# other. Needs 'make' first (the modules come from bin/).
# ---------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
pass=0; fail=0; broken=0
for src in "$ROOT"/tests/unit/T*.cbl; do
    name="$(basename "$src" .cbl)"
    work="$(mktemp -d)"
    mkdir -p "$work/data"
    if ! cobc -x -I "$ROOT/copybooks" -I "$ROOT/tests/unit" \
             -o "$work/$name" "$src" 2>"$work/compile.err"; then
        echo "BROKEN $name (does not compile)"; cat "$work/compile.err"
        broken=$((broken + 1)); rm -rf "$work"; continue
    fi
    out="$(cd "$work" && COB_LIBRARY_PATH="$ROOT/bin" "./$name" 2>&1)"
    echo "$out" | grep -E '^UT (PASS|FAIL|DONE)|^        '
    p=$(grep -c '^UT PASS' <<<"$out"); f=$(grep -c '^UT FAIL' <<<"$out")
    pass=$((pass + p)); fail=$((fail + f))
    grep -q '^UT DONE' <<<"$out" || { echo "BROKEN $name (no summary)"; \
                                      broken=$((broken + 1)); }
    rm -rf "$work"
done
echo "UNIT RESULT: $pass passed, $fail failed, $broken broken"
(( fail == 0 && broken == 0 ))
