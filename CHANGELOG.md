# Changelog

All of this was written on 2026-10-14, in one long working session with an
AI assistant, in rounds. Each round is listed as a version; the git history
starts at 0.5 (everything before it is one import commit).

## 0.6 - 2026-10-14

Added
- Mutual fund and insurance distribution: product master with NAV
  (MFMNT), units at NAV with exit load, SIPs, insurance policies with
  commission, KYC and senior-citizen suitability checks; MFBATCH for SIP
  instalments, premium renewals and lapses, remittance to AMCs / insurers and
  month-end trail commission; MFUTIL shared money module.
- Branch stock of cheque books, card blanks and PIN mailers (INVMNT,
  INVUTIL, INVBATCH); cheque book and debit card issue now take stock.
- Versioned deposit rate cards with a senior-citizen column (FDRTMNT,
  FDRATE file). DEPCALC uses the card in force on the booking date; old
  deposits keep their rate.
- Complaints and service requests (CMPMNT, CMPBATCH): SLA by category,
  escalation, ATM failed-debit refund with RBI Rs 100/day compensation,
  goodwill compensation, monthly complaints return with tickets over 30 days.
- Auto-sweep mandates and overdraft against FD (SWEEPMNT, SWEEPBAT).
- Branch master (BRMNT) and branch / inter-branch accounting (BRGLBAT):
  positions per branch, transfer legs matched by SORT, IB net must be zero.
- Financial year-end closing (YREND, jobs/YEAREND.sh, jcl/YEAREND.jcl):
  income and expense to P&L, 25% statutory reserve, rest to general reserve.
- GL codes 1310, 2906, 3002, 3003, 3004, 4106, 5102; transaction types MP,
  MR, IP, CM.
- Unit tests for ACCTPOST, GLPOST, TXNEXEC, LNDISB, DEPCALC, BATCHCTL,
  INVUTIL and MFUTIL (41 -> 133 unit checks).
- GitHub Actions CI, LICENSE, CONTRIBUTING, this changelog, .gitignore.

Changed
- Batch "keep files open" mode for the posting chain (BATCHCTL / BATMODE):
  TESTGEN load of 5,000 accounts / 50,000 transactions 48.7 s -> ~3.4 s.
- Premature FD closure looks up the rate card of the booking date.
- GLRPT's P&L heading now says "since last year-end".

## 0.5 - 2026-10-14

- Loan origination with scoring and two-level sanction (LOANAPP, LNDISB),
  IRAC provisioning and accrual (PROVBAT), collections with OTS, write-off
  and recovery (COLLMNT, COLLBAT).
- Fraud rules on the daily journal with hash tables (FRAUDBAT, FRAUDMNT).
- Trade finance BG / LC (TFMNT, TFBATCH), forex and NRI remittances with
  LRS and TCS (FXMNT), statement requests and monthly statements by SORT
  (STMTREQ, STMTBAT), audit exception report (AUDEXRPT), SQL enquiry with
  cursors (SQLINQ).
- Tools: data dictionary, program catalogue, JCL generator, perf script.

## 0.4 - 2026-10-14

- Credit cards, gold loans and loans against deposits, repo-linked home
  loans with a rate master, maker-checker approvals, branch cash (vault and
  teller drawers), NACH mandates and inward debits, outward cheques (CTS),
  credit bureau and Form 26AS files, regulatory returns, checkpoint /
  restart, JSON API, PostgreSQL reporting feed, JCL and CICS versions, COBOL
  unit test harness.

## 0.3 - 2026-10-14

- Double-entry general ledger with sub-ledger reconciliation, daily-product
  interest, overdrafts, dormancy, AML monitoring (CTR / STR), TDS on deposit
  interest, payments (NEFT / RTGS / IMPS / UPI), standing instructions,
  joint holders and nominees, lockers, customer 360 screen.

## 0.2 - 2026-10-14

- Customers and KYC, cheque books and inward clearing, service charges with
  GST, MIS reports, data migration utilities, batch job scripts.

## 0.1 - 2026-10-14

- Sign-on with roles and audit trail, savings / current accounts,
  deposits / withdrawals / transfers, loans with EMI schedules, debit cards
  with an ATM simulator, fixed and recurring deposits, end-of-day batch,
  end-to-end regression tests.
