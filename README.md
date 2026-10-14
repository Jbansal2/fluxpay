# Core Banking System (COBOL)

A core banking system in GnuCOBOL: sign-on with roles and an audit trail, customers and KYC,
joint holders and nominees, savings/current accounts with overdrafts and dormancy, transactions,
NEFT/RTGS/IMPS/UPI payments, standing instructions, loans, debit cards with an ATM simulator, fixed
and recurring deposits with TDS, cheques and inward clearing, safe deposit lockers, service charges,
daily-product interest, AML monitoring (CTR/STR), a double-entry general ledger, end-of-day
processing, MIS reports, a full-screen customer 360 view, data migration utilities, credit
cards, gold loans and loans against deposits, repo-linked home loans with a rate master,
maker-checker approvals, branch cash (vault and teller drawers), NACH mandates, outward cheque
clearing (CTS), credit bureau and Form 26AS interface files, regulatory returns, checkpoint /
restart for batch, a JSON API, a PostgreSQL reporting feed, JCL and CICS versions, batch job
scripts and automated tests (end-to-end and COBOL unit tests).
Learning project built with AI assistance.

## Quick start

```bash
make              # build everything (needs GnuCOBOL 3.x with indexed file support)
make test         # COBOL unit tests + the end-to-end suite
make unit         # unit tests only (tests/unit, one driver per subprogram)
FORCE=1 jobs/SETUPDEMO.sh   # load 50 generated accounts, 300 transactions, 10 loans
make run          # main menu; first run asks you to set the ADMIN password
```

## Online programs (main menu)

| Option | Program | Who | Purpose |
|--------|---------|-----|---------|
| 1 | ACCTMNT  | all | Open, inquire, update, freeze/unfreeze, close accounts |
| 2 | TRANPOST | all | Deposit, withdrawal, transfer (reversal if the credit leg fails) |
| 4 | STMTRPT  | all | Statement for a date range |
| 5 | LOANMNT  | all | Loan disbursal, EMI, schedule, payment, foreclosure; 6 = applications (LOANAPP), 7 = collections (COLLMNT) |
| 6 | CARDMNT  | all | Card issue (Luhn), PIN change, hotlist, unlock, limit |
| 7 | ATMTXN   | all | ATM session: PIN check, withdrawal, balance, mini statement |
| 8 | DEPMNT   | all | FD / RD: open, instalment, inquiry, premature closure |
| C | CUSTMNT  | all (KYC decision: manager) | Customers, PAN/mobile/email checks, KYC, re-KYC list |
| Q | CHQMNT   | all (clearing: manager) | Cheque books, stop payment, inward clearing; option 7 = outward cheques (CTSMNT) |
| Y | PAYMNT   | all | NEFT / RTGS / IMPS / UPI with mode limits, IFSC/UPI-id checks, slab charges |
| I | SIMNT    | all | Standing instructions: monthly transfer, loan EMI, RD instalment |
| H | HLDMNT   | all | Joint holders (max 3), operating mode, nominee (guardian for minors) |
| L | LOCKMNT  | all (add lockers: manager) | Locker allotment, visits, surrender, vacancies |
| V | ACCTSCR  | all | Full-screen customer 360 view (SCREEN SECTION; F3 to exit) |
| M | CASHMNT  | all (chest / issue cash: manager) | Vault by denomination, issue cash to tellers, teller drawer and cash tally |
| 0 | MOREMENU | all (fraud: manager) | Second page: fraud alerts (FRAUDMNT), trade finance BG/LC (TFMNT), forex and NRI remittances (FXMNT), statement requests (STMTREQ) |
| Z | NACHMNT  | all (reactivate: manager) | NACH debit mandates: register (UMRN), inquiry, cancel, reactivate |
| K | CCMNT    | all (issue/limit: manager) | Credit cards: issue (BIN 9991, Luhn), purchase/cash auth, payment, refund, block |
| O | SECLOAN  | all (gold rate: manager) | Gold loan (75% LTV) and loan against FD (90%, lien on the deposit) |
| 3 | INTCALC  | manager | Monthly savings interest |
| E | EODBATCH | manager | Overdue/NPA, deposit maturity, card expiry, daily summary |
| F | CHGBATCH | manager | Service charges with GST |
| R | MISRPT   | manager | Branch summary, loan NPA %, deposit liability, top 10 |
| B | PAYBATCH | manager | Payment settlement: returns, NEFT/RTGS settlement, inward credits |
| S | SIBATCH  | manager | Run standing instructions due (retries, missed-SI fee) |
| G | GLRPT    | manager | Trial balance, P&L, balance sheet, sub-ledger reconciliation |
| A | AMLRPT   | manager | Monthly CTR/STR alerts and CTR extract |
| T | TDSRPT   | manager | TDS certificates per account for a financial year |
| X | CCBATCH  | manager | Card autopay, late fees, monthly statements with interest + GST |
| N | RATEMNT  | manager | Interest rate master (repo, spreads, CRR/SLR ...); 2-point change limit, history |
| W | APPRVMNT | manager | Maker-checker queue: approve (posts) or reject teller items above 2,00,000 |
| J | GOLDREVL | manager | Gold loan revaluation, margin calls (LTV > 75%), auction notices |
| D | DATAUTIL | admin | CSV export/import, control totals, reconciliation |
| U | USERMNT  | admin | Users, password reset/unlock, disable, audit trail viewer |
| P | USERMNT  | all | Change own password |

Roles: **T** teller, **M** manager, **A** admin. The menu only shows what the role may use, and a
refused option is written to the audit trail.

## Batch programs (`bin/runpgm <PROGRAM>`)

`RUNPGM` runs one batch program as user `BATCH`, audits job start/end and returns the step RC to the
shell. Only batch programs are allowed: INTCALC, EODBATCH, CHGBATCH, CLRBATCH, MISRPT, DATAUTIL,
TESTGEN, GLRPT, AMLRPT, TDSRPT, PAYBATCH, SIBATCH, CCBATCH, GOLDREVL, APIGW,
SQLLOAD, SQLRPT, LNRESET, CASHRPT, NACHBAT, CTSOUT, CTSRET, CIBILOUT, TDS26AS, REGRPT.

| Program | Does |
|---------|------|
| LNRESET  | Resets repo-linked loans after a repo change (keeps EMI and moves tenure, or raises EMI at the 360-month cap) |
| CASHRPT  | Branch cash position, drawers not closed or not tallied, vaults + drawers vs GL 1001 |
| NACHBAT  | Inward NACH debit file: mandate checks, debit or return with reason code, bounce fee, suspension after 3 bounces, response file |
| CTSOUT   | Outward cheque presentation file (header / detail / trailer) |
| CTSRET   | Drawee returns (with fee), then deemed-cleared credits for everything not returned |
| CIBILOUT | Monthly credit bureau file with DPD and asset class (STD / SMA0-2 / NPA) |
| TDS26AS  | TDS deducted vs Form 26AS per PAN |
| REGRPT   | CRR/SLR on NDTL, structural liquidity buckets with gap tolerances, BSR-style size classes |
| PROVBAT  | IRAC asset classification by DPD, provisioning (0.4% / 15% / 25-100%), interest accrual on standard loans only |
| COLLBAT  | Escalating reminders by DPD (SMS, letter, final notice, recall / SARFAESI notice), broken promises to pay |
| FRAUDBAT | Fraud rules on the day's journal: velocity, odd hours, new payee, dormant wake-up, pass-through (mule) |
| TFBATCH  | Expires BGs / LCs, returns margin, prints the contingent liability register |
| STMTBAT  | Monthly statements for every active account (SORT by account + posting order) and queued requests; mail / print queues |
| AUDEXRPT | Audit exceptions: repeated failed sign-ons, refused access, after-hours activity, sensitive changes |
| SQLINQ   | Account and name enquiry against the PostgreSQL copy (parameterised cursors) | Answers to prompts (run date, menu choice)
come from stdin.

| Job script | Steps |
|------------|-------|
| `jobs/EODJOB.sh [date]` | control totals → clearing → payment settlement → standing instructions → EOD → card batch → gold revaluation → reconcile → GL (abends if unbalanced) → MIS |
| `jobs/MONTHEND.sh [date]` | interest → charges → AML report → TDS certificates → reconcile → GL → CSV export |
| `jobs/SETUPDEMO.sh [n] [t] [seed]` | wipe `data/` and load generated data |

Each step has a maximum allowed RC (like `COND=` in JCL); anything higher abends the job. Logs go
to `logs/`. A job that abends can be rerun with `RESTART=Y`: finished steps are skipped, and
EODBATCH resumes after its last committed account (CHKPOINT; try `CB_ABEND_AT=EODBATCH:2` to
simulate a crash).

## JSON API (`api/`)

`APIGW` reads one JSON request per line on stdin and answers with one JSON line (ops: `health`,
`balance`, `ministatement`, `transfer`). API keys live in `data/APIKEYS.DAT` with scope R or W;
transfers are capped at 2,00,000 and go through ACCTPOST like any teller transfer.
`api/server.py` is a stdlib-only HTTP adapter (no business logic):

```
python3 api/server.py 8080
curl -H 'X-API-Key: k-read-0001' localhost:8080/api/accounts/1000000002/balance
curl -H 'X-API-Key: k-write-0002' -d '{"from":"1000000002","to":"1000000009","amount":100}' \
     localhost:8080/api/transfers
```

## SQL reporting database (`sql/`, optional)

`SQLLOAD` copies the indexed masters into PostgreSQL in one unit of work (rollback on any error);
`SQLRPT` runs cursor-based reports (deposits by branch, loan quality/NPA %, top 10 depositors,
volume by type, GL vs accounts recon). Needs PostgreSQL and the OCESQL precompiler:

```
make sqlschema      # creates the tables in database corebank
make sql            # precompiles and builds bin/SQLLOAD.so, bin/SQLRPT.so
bin/runpgm SQLLOAD && bin/runpgm SQLRPT
```

Connection comes from `CB_DB` (default `corebank@localhost:5432`), `CB_DB_USER`, `CB_DB_PASS`.

## CICS screens (`cics/`, untested)

BMS maps and pseudo-conversational programs for sign-on, menu, account inquiry and a two-step
fund transfer (one unit of work, accounts locked in key order). There is no CICS here;
`cics/check.sh` compiles the COBOL around the `EXEC CICS` commands. See `cics/README.md`.

## Documentation (`docs/`)

* `ARCHITECTURE.md` - layers, posting rules, daily cycle, simplifications
* `PROGRAMS.md` - every program with purpose, files, calls, callers (generated by `tools/callgraph.py`)
* `DATA_DICTIONARY.md` - every record, field offsets and lengths (generated by `tools/datadict.py`,
  cross-checked against `cobc -tsymbols`)
* `PERFORMANCE.md` - batch timings at 5,000 accounts / 50,000 transactions (`tools/perf.sh`) and
  what was changed because of them

## Tools (`tools/`)

| Script | Does |
|--------|------|
| `datadict.py` | Regenerates the data dictionary |
| `callgraph.py` | Regenerates the program catalogue |
| `genjcl.py` | Regenerates `jcl/CBSETUP.jcl` and the PROC's DD list from one file list |
| `addbatch.py` | Adds a program to RUNPGM's allowed list |
| `perf.sh` | Times the batch suite on generated data |

## Mainframe JCL (`jcl/`, untested)

IDCAMS definitions for every VSAM file (real record/key lengths), a catalogued procedure and EOD /
month-end job streams with `COND` codes, SORT and GDG backups. There is no z/OS here, so these
have never been submitted — see `jcl/README.md`.

## Shared subprograms

| Program | Purpose |
|---------|---------|
| ACCTPOST | Debits/credits accounts: balance, overdraft and dormancy rules, TRANLOG, audit trail, GL entry, AML screen |
| GLPOST   | Posts one balanced journal entry; seeds the chart of accounts on first use |
| AMLCHK   | Per-account daily tally and CTR/STR rules |
| LNEMIPAY | Loan EMI / foreclosure quote and collection with the principal/interest GL split |
| DEPPAY   | Term deposit payout with TDS and GL entries |
| DATEUTIL | Add months/days, days between, validation (leap years, month-end clamp) |
| DEPCALC  | Deposit rate card and FD/RD value (quarterly compounding) |
| CARDUTIL | Luhn check digit/validation, PIN hashing |
| SECUTIL  | Password hashing and password policy |
| AUDITLOG | Appends to the audit trail; user and time come from the session, not the caller |
| SIGNON   | Sign-on, first-run admin creation, lockout, password expiry |

## Data files (`data/`)

| File | Organisation | Key |
|------|--------------|-----|
| USERMAST.DAT | indexed | user id |
| CUSTMAST.DAT | indexed | customer id (from 20000001) |
| ACCTMAST.DAT | indexed | account no (from 1000000001) |
| LOANMAST.DAT | indexed | loan no (from 5000000001) |
| DEPMAST.DAT  | indexed | deposit no (from 7000000001) |
| CARDMAST.DAT | indexed | 16-digit card no |
| CHQREG.DAT   | indexed | account no + cheque no |
| CHGLOG.DAT   | indexed | account + charge code + period |
| GLMAST.DAT   | indexed | GL code |
| PAYREG.DAT   | indexed | payment reference |
| SIMAST.DAT   | indexed | standing instruction id (from 30000001) |
| ACCTREL.DAT  | indexed | account + sequence (00 header, 01-03 holders, 90 nominee) |
| LOCKMAST.DAT | indexed | locker no |
| AMLDAY.DAT   | indexed | account + date |
| GLJRNL.DAT, AMLALERT.DAT, TDSLOG.DAT, LOCKVISIT.DAT | line sequential, append only | – |
| TRANLOG.DAT  | line sequential, append only | – |
| AUDITLOG.DAT | line sequential, append only | – |

Reports: `STMT_<acct>`, `SCHED_<loan>`, `EOD_<date>`, `CHG_<yyyymm>`, `CLR_<date>`, `MIS_<date>`
(`.TXT`); clearing returns `CLROUT_<date>.DAT`; CSVs `ACCTEXP`, `ACCTIMP`, `ACCTREJ`.

## General ledger

Every money movement is a balanced double entry. Chart of accounts: 1001 cash, 1002 ATM cash,
1201 loans, 1290 loan repayment suspense, 1301 cheque clearing, 1302 payment settlement, 2101 savings,
2102 current, 2201 term deposits, 2501 GST payable, 2502 TDS payable, 2900 transfer suspense,
3001 opening balance control, 4001/4002 loan/OD interest income, 4101 fees, 4102 locker rent,
5001/5002 savings/term deposit interest paid. GLRPT checks that debits equal credits, that the
balance sheet balances, that 1290/2900 are clear and that savings, current, loans and term deposits
in the GL equal the totals of the detail masters.

## Business rules

- **Accounts**: savings min opening/min balance 1,000, 3.5% p.a. monthly interest; current min opening 5,000
- **Loans** (reducing-balance EMI): home 8.5% up to 5 cr / 360 m; personal 13.5% up to 15 lakh / 60 m;
  vehicle 9.5% up to 20 lakh / 84 m. Foreclosure 2% (none on home loans). Penalty 2% of EMI per
  overdue instalment; 3+ overdue → NPA
- **Cards**: 2 live cards per account, daily limit 40,000 default, 10,000 per ATM withdrawal in
  multiples of 100, locked after 3 wrong PINs, 5-year validity
- **Deposits**: FD min 5,000; RD min 500 (multiple of 100); 6–120 months; 6.5% / 7.0% / 7.25% / 6.75%
  by tenure, +0.5% senior citizens; premature closure at held-period rate − 1%, none under 7 days
- **KYC**: 18+, individual PAN (4th char P), only last 4 Aadhaar digits stored, address change resets
  KYC, re-KYC every 2/8/10 years for high/medium/low risk
- **Charges** (+18% GST): min balance penalty 100/month, SMS 15/quarter, card annual fee 150,
  cheque return 250. Re-running a period never charges twice
- **Cheques**: books of 10 or 25 leaves, max 50 unused leaves per account; cheques valid 3 months;
  return reasons 01, 04, 05, 12, 21, 22, 31, 32, 99
- **Interest**: savings on the sum of end-of-day balances × rate / 36500; overdraft interest the same
  way on overdrawn balances; products built by the EOD batch
- **Overdraft**: current accounts only, manager-approved limit up to 50 lakh, default 12% p.a.
- **Dormancy**: no customer-initiated activity for 24 months; credits and bank charges still allowed,
  customer debits blocked until a manager reactivates after fresh KYC
- **TDS**: 10% of deposit interest above 40,000 (50,000 for senior citizens), per deposit
- **Payments**: RTGS min 2 lakh, 07:00–18:00; IMPS max 5 lakh; UPI max 1 lakh; NEFT/RTGS settle in
  batch, IMPS/UPI instantly; returned payments are refunded without charges
- **Standing instructions**: day 1–28; two daily retries; third failure = missed + 100 + GST fee
- **AML**: CTR for 10 lakh+ cash (single or daily); STR for 3+ cash deposits of 45,000–49,999 in a day
  and for 5 lakh+ credited with 90%+ taken out the same day
- **Lockers**: small/medium/large 2,000/4,000/8,000 a year + GST, one per customer, access refused
  when rent is overdue
- **Security**: passwords 8–20 chars with a letter and a digit, expire after 90 days, 5 failures lock
  the user, temporary passwords must be changed at first sign-on, last active admin cannot be disabled

## Known limitations

- Password and PIN hashing are illustrative, not cryptographic; passwords are echoed on screen
- Transaction IDs are unique only within one run (date + per-program sequence)
- Duplicate PAN and some lookups are full scans (no alternate keys)
- Not designed for concurrent users
- Minimum balance check uses the balance on the run date, not a monthly average
- TDS threshold is applied per deposit, not per PAN per year; no Form 15G/15H handling
- Payment settlement with RBI and the other bank is simulated through files
- Cash is tracked in a teller's drawer only once a manager has issued cash to him that day
- Maker-checker covers teller cash and transfer items; master-data changes are audited, not
  dual-controlled
- Regulatory returns are simplified management versions, not the RBI submission formats
- NACH, CTS, CIBIL and 26AS files use simplified fixed-width layouts (documented in each program)
