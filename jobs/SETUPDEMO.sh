#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# SETUPDEMO - wipe the data directory and load generated test data
#
# Usage: jobs/SETUPDEMO.sh [ACCOUNTS] [TRANSACTIONS] [SEED]
#        Set FORCE=1 to skip the confirmation prompt (used by the tests).
# ---------------------------------------------------------------------------
JOB_NAME=SETUPDEMO
source "$(dirname "$0")/common.sh"

if [[ "${FORCE:-0}" != "1" ]]; then
    read -r -p "This DELETES everything in data/. Type YES to continue: " ok
    [[ "$ok" == "YES" ]] || { echo "Cancelled."; exit 8; }
fi

rm -f data/*.DAT data/*.TXT data/*.CSV data/*.TMP data/*.idx
log "DATA DIRECTORY CLEARED"

run_step STEP010 TESTGEN 0 "${1:-}" "${2:-}" "${3:-}"
run_step STEP020 DATAUTIL 0 3

job_end
