#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# EODJOB - end-of-day job stream
#
#   STEP010  control totals before the run (audit evidence)
#   STEP020  inward cheque clearing   (only if data/CLRIN.DAT is present)
#   STEP030  payment settlement       (returns, settle NEFT/RTGS, inward)
#   STEP035  inward NACH debits       (only if data/NACHIN.DAT is present)
#   STEP036  outward cheque returns / deemed-cleared credits
#   STEP037  outward cheque presentation file
#   STEP040  standing instructions due today
#   STEP045  auto-sweep: surplus into FDs, shortfall back from FDs
#   STEP050  end-of-day batch         (products, dormancy, loans, deposits,
#                                      cards, daily summary)
#   STEP052  reset repo-linked loans to the current repo rate
#   STEP055  credit cards: autopay, late fees, statements
#   STEP056  mutual fund SIPs, insurance premiums, remittance to AMCs
#   STEP058  gold loan revaluation / margin calls
#   STEP058A trade finance expiry / contingent register
#   STEP059  branch cash position (open / short drawers = RC 4)
#   STEP060  reconcile master to transaction log - mismatches abend the job
#   STEP070  general ledger: trial balance + sub-ledger reconciliation;
#            an unbalanced ledger (RC 8) abends the job
#   STEP075  fraud rules on the day's journal (RC 4 = new alerts)
#   STEP076  collections reminders / broken promises
#   STEP077  branch stock reorder (cheque books, card blanks)
#   STEP078  complaint SLA escalation + monthly complaints return
#   STEP079  branch positions and inter-branch reconciliation
#   STEP080  MIS reports
#
# Usage: jobs/EODJOB.sh [YYYYMMDD]     (default: today)
# ---------------------------------------------------------------------------
JOB_NAME=EODJOB
source "$(dirname "$0")/common.sh"

RUN_DATE="${1:-}"
STAMP="$(date +%Y%m%d_%H%M%S)"
log "JOB $JOB_NAME START, RUN DATE ${RUN_DATE:-TODAY}"

run_step STEP010 DATAUTIL 0 3

if [[ -f data/CLRIN.DAT ]]; then
    # RC 4 = some cheques returned, which is normal
    run_step STEP020 CLRBATCH 4 "$RUN_DATE"
    mv data/CLRIN.DAT "data/CLRIN_done_${STAMP}.DAT"
else
    log "STEP020 SKIPPED - NO INWARD CLEARING FILE"
fi

# RC 4 = returns or rejected inward credits, which is normal
run_step STEP030 PAYBATCH 4 "$RUN_DATE"
for f in PAYIN PAYRET; do
    [[ -f "data/$f.DAT" ]] && mv "data/$f.DAT" "data/${f}_done_${STAMP}.DAT"
done

# STEP035: inward NACH debits, only when the clearing file has arrived
if [[ -f data/NACHIN.DAT ]]; then
    # RC 4 = some items returned, which is normal
    run_step STEP035 NACHBAT 4 "$RUN_DATE"
    mv data/NACHIN.DAT "data/NACHIN_done_${STAMP}.DAT"
else
    log "STEP035 SKIPPED - NO INWARD NACH FILE"
fi

# STEP036/037: outward cheques - apply returns and credit what cleared,
# then present today's lodgements (RC 4 = returns / held credits)
run_step STEP036 CTSRET 4 "$RUN_DATE"
run_step STEP037 CTSOUT 0 "$RUN_DATE"

# RC 4 = retries, misses or suspensions - reported, not fatal
run_step STEP040 SIBATCH 4 "$RUN_DATE"

# RC 4 = a sweep posting was refused - reported
run_step STEP045 SWEEPBAT 4 "$RUN_DATE"

# RC 4 = warnings (e.g. a payout that could not be made)
run_step STEP050 EODBATCH 4 "$RUN_DATE"

# RC 4 = some EMIs had to be raised (tenure cap) - reported
run_step STEP052 LNRESET 4 "$RUN_DATE"

# RC 4 = autopay failures / margin calls - reported, not fatal
run_step STEP055 CCBATCH 4 "$RUN_DATE"
run_step STEP058 GOLDREVL 4 ""

# RC 4 = missed SIPs / unpaid premiums / lapsed policies - reported
run_step STEP056 MFBATCH 4 "$RUN_DATE"

# RC 4 = a margin could not be returned
run_step STEP058A TFBATCH 4 "$RUN_DATE"

# RC 4 = drawers not closed or not tallied - reported
run_step STEP059 CASHRPT 4 "$RUN_DATE"

# RC 4 = accounts with no history (allowed), 8 = real mismatch (abend)
run_step STEP060 DATAUTIL 4 4

# RC 4 = suspense not clear (investigate), 8 = books do not balance (abend)
run_step STEP070 GLRPT 4

# RC 4 = new fraud alerts / broken promises - for review, not fatal
run_step STEP075 FRAUDBAT 4 "$RUN_DATE"
run_step STEP076 COLLBAT 4 "$RUN_DATE"

# RC 4 = reorders raised / escalations / unmatched inter-branch legs
run_step STEP077 INVBATCH 4 "$RUN_DATE"
run_step STEP078 CMPBATCH 4 "$RUN_DATE"
run_step STEP079 BRGLBAT 4 "$RUN_DATE"

run_step STEP080 MISRPT 0 A

job_end
