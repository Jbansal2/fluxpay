# Performance

Measured with `tools/perf.sh 5000 50000` (GnuCOBOL 3.1.2, Berkeley DB indexed
files, one CPU of the build container). Re-run it after any change to a
batch program; the numbers below are from 2026-10-14.

Data: 5000 accounts, 50000 transactions, 1000 loans

| Step                         | Seconds  | RC  |
|------------------------------|----------|-----|
| TESTGEN (load)               |     3.35 |   0 |
| EODBATCH                     |     0.21 |   0 |
| INTCALC                      |     0.01 |   4 |
| GLRPT                        |     0.01 |   0 |
| FRAUDBAT                     |     0.08 |   4 |
| PROVBAT                      |     0.02 |   0 |
| REGRPT                       |     0.13 |   4 |
| CIBILOUT                     |     0.01 |   4 |
| STMTBAT (SORT, all accounts) |     1.58 |   0 |
| DATAUTIL reconcile           |     0.08 |   0 |
| BRGLBAT (positions, IB SORT) |     0.05 |   0 |
| YREND (year-end closing)     |     0.01 |   0 |

RC 4 here is a warning, not a failure (interest with nothing new to credit,
fraud alerts raised, liquidity gaps, loans with incomplete KYC).

## What was changed because of a measurement

**TESTGEN: 48.7 s -> about 3.4 s (14x).** Every ACCTPOST call used to open
and close the account master and transaction log, and inside it GLPOST
(ledger + journal), AUDITLOG and AMLCHK did the same - eight opens and
closes per posting, ~55,000 postings. There is now a run-unit wide switch
(copybook BATMODE, an EXTERNAL byte) set through `CALL 'BATCHCTL' USING
'S'` before a posting loop and `'E'` after it. While it is on, the four
modules open their files on first use and leave them open; `'E'` makes each
of them close and flush. Online programs never set it, so they keep the old
open-per-call behaviour and never meet a file someone else is holding.
TESTGEN and SWEEPBAT use it. The unit test TBATCHCT checks that 300
postings in batch mode give the same balance, log and ledger as online
mode, and that online mode works again afterwards. The rule for a batch
that switches it on: it must not open ACCTMAST, TRANLOG, GLMAST, GLJRNL or
AUDITLOG itself between `'S'` and `'E'`.

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

* **TESTGEN (~3.4 s)** is still the biggest step: ~55,000 postings at about
  0.06 ms each now that the files stay open. The rest is record I/O.
* Timings in this container vary by up to 3x between runs (STMTBAT has
  measured 0.24 s to 1.6 s on the same data), so compare orders of
  magnitude, not the second decimal.
* Everything else is a single sequential pass (or a SORT) and finishes in
  well under a second at this size.
