#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# MONTHEND - month-end job stream
#
#   STEP010  savings interest (daily product) and overdraft interest
#   STEP020  service charges (min balance, SMS, card fee, locker rent)
#   STEP030  AML report and CTR extract for the month
#   STEP040  TDS certificates for the financial year to date
#   STEP050  reconcile master to transaction log
#   STEP060  general ledger reports
#   STEP070  export account master to CSV (for the data warehouse)
#   STEP080  credit bureau (CIBIL) submission file
#   STEP090  regulatory returns: CRR/SLR, liquidity buckets, BSR
#   STEP100  IRAC classification, provisioning, interest accrual
#   STEP110  monthly customer statements (+ queued requests)
#   STEP120  audit exception report for the month
#
# Usage: jobs/MONTHEND.sh [YYYYMMDD]   (run date; default: today)
# ---------------------------------------------------------------------------
JOB_NAME=MONTHEND
source "$(dirname "$0")/common.sh"

RUN_DATE="${1:-}"
log "JOB $JOB_NAME START, RUN DATE ${RUN_DATE:-TODAY}"

# RC 4 = nothing due this month, not an error
run_step STEP010 INTCALC 4 "$RUN_DATE"
# RC 4 = some charges could not be recovered; they are retried next run
run_step STEP020 CHGBATCH 4 "$RUN_DATE"
# RC 4 = STR alerts waiting for the compliance officer
run_step STEP030 AMLRPT 4 "${RUN_DATE:0:6}"
run_step STEP040 TDSRPT 0 "" ""
run_step STEP050 DATAUTIL 4 4
run_step STEP060 GLRPT 4
run_step STEP070 DATAUTIL 0 1
# RC 4 = loans whose borrower KYC is incomplete - listed for branches
run_step STEP080 CIBILOUT 4 "$RUN_DATE"
# RC 4 = a liquidity gap outside tolerance - reported to ALCO
run_step STEP090 REGRPT 4 "$RUN_DATE"
run_step STEP100 PROVBAT 0 "$RUN_DATE"
run_step STEP110 STMTBAT 0 "${RUN_DATE:0:6}"
# RC 4 = exceptions listed for audit
run_step STEP120 AUDEXRPT 4 "" "$RUN_DATE"

job_end
