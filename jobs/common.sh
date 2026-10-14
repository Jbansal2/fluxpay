#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# common.sh - shared step runner for batch jobs (sourced, not run directly)
#
# run_step NAME PROGRAM MAX_RC [INPUT...]
#   Runs PROGRAM through bin/runpgm, feeding each INPUT argument as one line
#   of stdin. Output is appended to the job log. If the step return code is
#   above MAX_RC the job abends, like COND= in JCL.
#
# Restart: every step that ends OK is recorded in data/<JOB>.state. If the
# job abends, run it again with RESTART=Y and the recorded steps are
# skipped (like RESTART=STEPxxx on a JOB card). Inside a step, programs
# that use CHKPOINT resume from their own last commit point. A job that
# completes removes its state file.
# ---------------------------------------------------------------------------
set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT" || exit 16
export COB_LIBRARY_PATH="$ROOT/bin"

if [[ ! -x bin/runpgm ]]; then
    echo "bin/runpgm not found - run 'make' first" >&2
    exit 16
fi

mkdir -p logs data
JOB_NAME="${JOB_NAME:-JOB}"
JOB_LOG="logs/${JOB_NAME}_$(date +%Y%m%d_%H%M%S).log"
JOB_MAX_RC=0
JOB_STATE="data/${JOB_NAME}.state"
if [[ "${RESTART:-N}" != "Y" ]]; then
    rm -f "$JOB_STATE"
elif [[ -f "$JOB_STATE" ]]; then
    echo "$(date '+%H:%M:%S') RESTART - SKIPPING: $(tr '\n' ' ' <"$JOB_STATE")"
fi

log() {
    echo "$(date '+%H:%M:%S') $*" | tee -a "$JOB_LOG"
}

run_step() {
    local name="$1" program="$2" max_rc="$3"
    shift 3
    if [[ -f "$JOB_STATE" ]] && grep -qx "$name" "$JOB_STATE"; then
        log "STEP $name SKIPPED - COMPLETED BEFORE RESTART"
        return 0
    fi
    log "STEP $name ($program) START"
    local rc
    printf '%s\n' "$@" | ./bin/runpgm "$program" >>"$JOB_LOG" 2>&1
    rc=$?
    (( rc > JOB_MAX_RC )) && JOB_MAX_RC=$rc
    if (( rc > max_rc )); then
        log "STEP $name ENDED RC=$rc (MAX ALLOWED $max_rc) - JOB ABENDED"
        log "JOB $JOB_NAME FAILED. LOG: $JOB_LOG"
        exit "$rc"
    fi
    log "STEP $name ENDED RC=$rc"
    echo "$name" >>"$JOB_STATE"
}

job_end() {
    rm -f "$JOB_STATE"
    log "JOB $JOB_NAME COMPLETE, HIGHEST RC=$JOB_MAX_RC. LOG: $JOB_LOG"
    exit "$JOB_MAX_RC"
}
