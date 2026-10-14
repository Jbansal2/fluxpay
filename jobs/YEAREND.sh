#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# YEAREND - financial year-end closing (run once, after the 31 March EOD
# and MONTHEND have completed)
#
#   STEP010  general ledger must balance and reconcile before closing
#   STEP020  close income / expense to P&L, appropriate to reserves
#   STEP030  general ledger again - must still balance after closing
#
# Usage: jobs/YEAREND.sh YYYY0331
# ---------------------------------------------------------------------------
JOB_NAME=YEAREND
source "$(dirname "$0")/common.sh"

YEAR_END="${1:-}"
if [[ ! "$YEAR_END" =~ ^[0-9]{4}0331$ ]]; then
    echo "usage: jobs/YEAREND.sh YYYY0331" >&2
    exit 16
fi
log "JOB $JOB_NAME START, YEAR ENDING $YEAR_END"

# RC 4 = suspense not clear (investigate first), 8 = books do not balance
run_step STEP010 GLRPT 4
run_step STEP020 YREND 0 "$YEAR_END"
run_step STEP030 GLRPT 4

job_end
