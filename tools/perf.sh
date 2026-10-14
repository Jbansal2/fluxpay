#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# perf.sh - time the main batch programs on a generated data set
#
#   tools/perf.sh [accounts] [transactions]      (default 5000 / 50000)
#
# Runs in a throwaway workspace (bin/ linked, empty data/), loads the data
# with TESTGEN, then times each batch step with the real clock. Prints a
# Markdown table that can be pasted into docs/PERFORMANCE.md.
# ---------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
N="${1:-5000}"; T="${2:-50000}"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/data"; ln -s "$ROOT/bin" "$WORK/bin"; cd "$WORK" || exit 1
export COB_LIBRARY_PATH="$WORK/bin"

timed() {      # timed LABEL PROGRAM [input lines...]
    local label="$1" prog="$2"; shift 2
    local s e rc
    s=$(date +%s.%N)
    printf '%s\n' "$@" | ./bin/runpgm "$prog" >/dev/null 2>&1; rc=$?
    e=$(date +%s.%N)
    printf '| %-28s | %8.2f | %3s |\n' "$label" "$(echo "$e - $s" | bc)" "$rc"
}

TODAY=$(date +%Y%m%d); TMRW=$(date -d tomorrow +%Y%m%d)
echo "Data: $N accounts, $T transactions, $((N / 5)) loans"
echo
echo "| Step                         | Seconds  | RC  |"
echo "|------------------------------|----------|-----|"
timed "TESTGEN (load)"               TESTGEN "$N" "$T" 777
timed "EODBATCH"                     EODBATCH "$TMRW"
timed "INTCALC"                      INTCALC ""
timed "GLRPT"                        GLRPT
timed "FRAUDBAT"                     FRAUDBAT "$TODAY"
timed "PROVBAT"                      PROVBAT "$TMRW"
timed "REGRPT"                       REGRPT "$TODAY"
timed "CIBILOUT"                     CIBILOUT "$TODAY"
timed "STMTBAT (SORT, all accounts)" STMTBAT "${TODAY:0:6}"
timed "DATAUTIL reconcile"           DATAUTIL 4
timed "BRGLBAT (positions, IB SORT)" BRGLBAT "$TODAY"
timed "YREND (year-end closing)"      YREND "$(( ${TODAY:0:4} + 1 ))0331"
