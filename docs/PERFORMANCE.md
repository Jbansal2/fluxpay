# Performance

Measured with `tools/perf.sh 5000 50000` (GnuCOBOL 3.1.2, Berkeley DB indexed
files, one CPU of the build container). Re-run it after any change to a
batch program; the numbers below are from 2026-10-14.

Data: 5000 accounts, 50000 transactions, 1000 loans

| Step                         | Seconds  | RC  |
|------------------------------|----------|-----|
| TESTGEN (load)               |    48.72 |   0 |
| EODBATCH                     |     0.20 |   0 |
| INTCALC                      |     0.01 |   4 |
| GLRPT                        |     0.01 |   0 |
| FRAUDBAT                     |     0.07 |   4 |
| PROVBAT                      |     0.01 |   0 |
| REGRPT                       |     0.10 |   4 |
| CIBILOUT                     |     0.01 |   4 |
| STMTBAT (SORT, all accounts) |     0.24 |   0 |
| DATAUTIL reconcile           |     0.06 |   0 |

RC 4 here is a warning, not a failure (interest with nothing new to credit,
fraud alerts raised, liquidity gaps, loans with incomplete KYC).

## What was changed because of a measurement

**FRAUDBAT: 10.8 s -> 0.07 s.** The first version kept per-account state in
a table searched from the top for every journal entry - 55,000 entries x up
to 5,000 accounts. The account and payee tables are now open-addressing hash
tables (8,191 and 16,381 slots, both prime; key mod size, step to the next
free slot). Same alerts, about 150 times faster, and it no longer slows down
as the number of accounts grows.

**STMTBAT: sorted by posting order, not transaction id.** Not a speed issue
but found while checking the output: transaction ids come from several
programs and are not in posting order across them, so statements sorted by
id showed the opening deposit last and opening + credits - debits did not
equal the closing balance. The SORT key is now account + position in the
journal; every statement balances (checked by script over 50 statements).

## Where the time goes

* **TESTGEN (49 s)** is the only slow step, and it is the postings, not the
  generator: ~55,000 calls to ACCTPOST at roughly 0.9 ms each. ACCTPOST opens
  and closes the account master, journal and GL on every call so that the
  interactive programs, which open the same files themselves, never meet a
  file held open by someone else. A batch-only "keep files open" mode would
  cut this sharply; it has not been added because no nightly job posts at
  that volume.
* Everything else is a single sequential pass (or a SORT) and finishes in
  well under a second at this size.
