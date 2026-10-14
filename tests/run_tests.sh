#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# run_tests.sh - end-to-end regression tests
#
# Builds a throwaway workspace (bin/ linked, empty data/), loads seeded test
# data and drives the batch programs, the job scripts and the online menus
# with piped input. Each check greps the output for the expected result.
# Exit code = number of failed checks.
# ---------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK="$(mktemp -d)"
[[ "${KEEP:-0}" == "1" ]] && echo "WORKSPACE: $WORK" || trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/data"
ln -s "$ROOT/bin" "$WORK/bin"
cp -r "$ROOT/jobs" "$WORK/jobs"
cd "$WORK" || exit 99
export COB_LIBRARY_PATH="$WORK/bin"

PASS=0
FAIL=0
OUT=""

run() {          # run PROGRAM [input lines...] -> $OUT, $RC
    local program="$1"; shift
    OUT="$(printf '%s\n' "$@" | timeout 60 ./bin/runpgm "$program" 2>&1)"
    RC=$?
}

online() {       # online [input lines...] -> $OUT  (main menu session)
    OUT="$(printf '%s\n' "$@" | timeout 60 ./bin/bankmain 2>&1)"
    RC=$?
}

check() {        # check "description" "regex that must appear in $OUT"
    if grep -Eq -- "$2" <<<"$OUT"; then
        PASS=$((PASS + 1)); echo "  PASS  $1"
    else
        FAIL=$((FAIL + 1)); echo "  FAIL  $1   (expected /$2/)"
        [[ "${VERBOSE:-0}" == "1" ]] && grep -v -E '^ [0-9A-Z]\. |^ --|=====|MAIN MENU|USER: ' <<<"$OUT" | tail -15 | sed 's/^/        | /'
    fi
}

check_rc() {     # check_rc "description" expected_rc
    if [[ "$RC" == "$2" ]]; then
        PASS=$((PASS + 1)); echo "  PASS  $1"
    else
        FAIL=$((FAIL + 1)); echo "  FAIL  $1   (rc $RC, expected $2)"
    fi
}

echo "== Test data generator"
run TESTGEN "" "" ""
check_rc "testgen ends RC 0" 0
check "50 accounts created" "ACCOUNTS CREATED +: 0000050"
check "10 loans created"    "LOANS CREATED +: 0000010"
run TESTGEN "" "" ""
check_rc "testgen refuses a non-empty data set (RC 8)" 8

echo "== Reconciliation after load"
run DATAUTIL 4
check "master agrees with transaction log" "MASTER AND LOG AGREE"

echo "== End of day"
run EODBATCH ""
check "4 back-dated loans become NPA" "NEWLY NPA +: +4"
check "summary counts the day's postings" "ALL TRANSACTIONS"
run EODBATCH ""
check "re-run is idempotent (no new NPA)" "NEWLY NPA +: +0"

echo "== Charges"
QDATE="$(date +%Y)1231"
run CHGBATCH "$QDATE"
check "quarter-end SMS charges posted" "SMS ALERTS QTR .*POSTED"
run CHGBATCH "$QDATE"
check "second run posts nothing new" "POSTED +: +0"
check "second run sees charges as already done" "ALREADY CHARGED +: +[1-9]"
run DATAUTIL 4
check "still reconciles after charges" "MASTER AND LOG AGREE"

echo "== Sign-on and roles"
online Admin2026x Admin2026x admin Admin2026x \
       U 1 teller1 "Test Teller" T DEL001 Temp1234 Temp1234 9 9
check "first run creates ADMIN" "ADMIN CREATED"
check "admin signs on" "WELCOME, SYSTEM ADMINISTRATOR"
check "admin adds a teller" "USER TELLER1  CREATED"
online teller1 Temp1234 Teller99x Teller99x E 9
check "teller must change temp password" "MUST SET A NEW PASSWORD"
check "teller is refused the EOD batch" "NOT AUTHORISED"
online teller1 wrong1 teller1 wrong2 teller1 wrong3
check "3 bad passwords end the session" "TOO MANY FAILED ATTEMPTS"
check "unknown user and bad password look the same" "INVALID USER ID OR PASSWORD"

echo "== Cheques and clearing"
online admin Admin2026x Q 1 1000000002 10 9 9
check "cheque book issued" "BOOK 0001 ISSUED: CHEQUES 100001 TO 100010"
python3 - <<'EOF'
def rec(seq, acct, chq, amt, dt, payee):
    return f"{seq:06d}{acct:010d}{chq:06d}{int(round(amt*100)):011d}{dt}{payee:<25.25}" + " " * 6
import datetime
d = datetime.date.today().strftime("%Y%m%d")
rows = [rec(1, 1000000002, 100001, 100, d, "FIRST"),
        rec(2, 1000000002, 100001, 100, d, "DUPLICATE"),
        rec(3, 1000000002, 100500, 100, d, "NOT ISSUED")]
open("data/CLRIN.DAT", "w").write("\n".join(rows) + "\n")
EOF
run CLRBATCH ""
check_rc "clearing with returns ends RC 4" 4
check "first presentment paid" "100001 +100.00  PAID"
check "duplicate returned as already paid" "RETURN 32"
check "unissued leaf returned" "RETURN 31"
run DATAUTIL 4
check "still reconciles after clearing" "MASTER AND LOG AGREE"

echo "== Import with rejects"
cat > data/ACCTIMP.CSV <<'EOF'
NAME,TYPE,BALANCE,PHONE,BRANCH
GOOD ONE,S,5000,9876543210,DEL001
BAD TYPE,Z,5000,9876543210,DEL001
EOF
run DATAUTIL 2
check_rc "import with a reject ends RC 4" 4
check "one account loaded" "ACCOUNTS LOADED +: +1"
check "reject file has the reason" "SEE data/ACCTREJ.CSV"

echo "== Audit trail"
OUT="$(cat data/AUDITLOG.DAT)"
check "logins are audited" "LOGIN +"
check "failed logins are audited" "LOGIN-FAIL"
check "postings are audited" "ACCTPOST ?DEBIT"
check "batch jobs are audited" "JOB-END"

echo "== General ledger after the day's work"
run GLRPT
check_rc "GL report ends RC 0" 0
check "trial balance agrees" "TRIAL BALANCE AGREES"
check "balance sheet balances" "BALANCE SHEET BALANCES"
check "suspense accounts clear" "1290 AND 2900 ARE CLEAR"
check "savings sub-ledger matches GL" "SAVINGS DEPOSITS \(2101\) +([0-9,.]+) +\1 +0\.00"
check "loan sub-ledger matches GL" "LOANS \(1201\) +([0-9,.]+) +\1 +0\.00"

echo "== Payments"
online admin Admin2026x Y 1 N 1000000002 "Ravi Kumar" 123456789012 HDFC0001234 15000 RENT Y \
       1 U 1000000002 Friend friend@okbank 2500 "" Y \
       1 U 1000000002 Bad nobank 100 "" \
       1 I 1000000002 Big 123456789 SBIN0004321 600000 "" 9 9
check "NEFT queued for settlement" "QUEUED FOR SETTLEMENT"
check "UPI sent instantly with a UTR" "PAYMENT SENT. REFERENCE U"
check "bad UPI id refused" "UPI ID MUST LOOK LIKE"
check "IMPS above 5 lakh refused" "IMPS MAXIMUM"
python3 - <<'PYEOF'
def inw(utr, acct, amt, name, mode="N"):
    return f"{utr:<22}{mode}{acct:<10}{int(round(amt*100)):011d}{name:<35}"
rows = [inw("UTRT0001", 1000000005, 25000, "EMPLOYER LTD"),
        inw("UTRT0001", 1000000005, 25000, "EMPLOYER LTD"),
        inw("UTRT0002", 1999999999, 500, "NOBODY")]
open("data/PAYIN.DAT", "w").write("\n".join(rows) + "\n")
PYEOF
run PAYBATCH ""
check_rc "settlement with returns ends RC 4" 4
check "NEFT settled" "OUTWARD SETTLED +: 00001"
check "inward credit posted" "INWARD CREDITED +: 00001"
check "duplicate UTR returned" "DUPLICATE UTR"
check "unknown account returned" "NO SUCH ACCOUNT"
rm -f data/PAYIN.DAT

echo "== Standing instructions"
online admin Admin2026x I 1 1000000002 T 1000000009 1500 28 "" \
       1 1000000002 T 1000000002 100 5 "" 9 9
check "transfer SI created" "SI 30000001 CREATED"
check "SI to the same account refused" "SAME ACCOUNT"
# FIRST RUN = THE 28TH OF THIS MONTH IF STILL AHEAD, ELSE NEXT MONTH
NEXT28="$(python3 -c '
import datetime as d
t = d.date.today()
if t.day < 28:
    print(f"{t.year}{t.month:02d}28")
else:
    m = t.month % 12 + 1; y = t.year + (t.month == 12)
    print(f"{y}{m:02d}28")')"
run SIBATCH "$NEXT28"
check "SI executed on its day" "EXECUTED   : 00001"
run SIBATCH "$NEXT28"
check "SI not run twice for the same month" "DUE        : 00000"

echo "== Overdraft"
run DATAUTIL 1
CA="$(grep ',C,A,' data/ACCTEXP.CSV | head -1 | cut -d, -f1)"
BAL="$(grep "^$CA," data/ACCTEXP.CSV | cut -d, -f5)"
online admin Admin2026x 1 7 "$CA" 50000 "" 9 9
check "manager sets OD limit" "OVERDRAFT LIMIT SET"
online admin Admin2026x 2 2 "$CA" "$(python3 -c "print(f'{$BAL + 30000:.2f}')")" "USE OD" \
       2 "$CA" 30000 "TOO FAR" 9 9
check "withdrawal into overdraft allowed" "NEW BALANCE : +-30,000.00"
check "withdrawal beyond OD limit refused" "INSUFFICIENT FUNDS. AVAILABLE: +20,000.00"

echo "== AML"
online admin Admin2026x 2 1 1000000007 47000 A 1 1000000007 48500 B \
       1 1000000007 49999 C 1 1000000008 1000000 BIG 9 9
run AMLRPT ""
check "structuring (3 deposits just under 50k) flagged" "STRUCTURING"
check "10 lakh cash deposit reported as CTR" "SINGLE +1,000,000.00"

echo "== Customers, joint holders, nominee, lockers"
online admin Admin2026x C 1 "Amit Kumar" 19900515 M ABCPK1234D 123456789012 \
       9811122233 amit@example.com "12 MG Road" "" Delhi Delhi 110001 L \
       1 "Priya Kumar" 19920101 F XYZPK4321Q 111122223333 9811100000 "" \
       "Main St" "" Delhi Delhi 110001 L \
       4 20000001 V OK 4 20000002 V OK 9 9
check "two customers KYC verified" "KYC STATUS UPDATED"
online admin Admin2026x H 2 1000000003 20000001 2 1000000003 20000002 \
       4 1000000003 S 4 1000000003 E \
       5 1000000003 "Rohan Kumar" Son 20150101 "" \
       5 1000000003 "Rohan Kumar" Son 20150101 "Amit Kumar" 9 9
check "primary holder linked" "PRIMARY HOLDER LINKED"
check "single mode refused with two holders" "SINGLE MODE NEEDS EXACTLY ONE"
check "either-or-survivor mode set" "OPERATING MODE SET TO E"
check "minor nominee needs a guardian" "GUARDIAN IS REQUIRED"
check "nominee registered" "NOMINEE REGISTERED"
online admin Admin2026x L 1 DEL001 M 3 2 1 20000001 1000000003 Y \
       2 2 20000001 1000000003 3 1 20000002 3 1 20000001 9 9
check "lockers added" "003 LOCKERS ADDED"
check "locker allotted with rent" "LOCKER 00001 ALLOTTED"
check "one locker per customer" "ALREADY HAS LOCKER"
check "stranger refused at the locker" "NOT THE LOCKER HOLDER"
check "holder granted access" "ACCESS GRANTED"

echo "== Interest rate master and repo-linked home loans"
online admin Admin2026x 5 1 1000000005 H 2000000 240 Y 9 9
check "home loan priced off repo" "REPO-LINKED: REPO \+ SPREAD = +8.50"
HL="$(grep -oE 'LOAN [0-9]{10} DISBURSED' <<<"$OUT" | grep -oE '[0-9]{10}')"
online admin Admin2026x N 2 REPO 9.75 2 REPO 7.00 Y 1 9 9
check "rate master seeded" "RATE MASTER CREATED"
check "3-point jump refused" "CHANGE OVER 2.00 POINTS REFUSED"
check "repo raised" "RATE REPO CHANGED TO +7.0000"
run LNRESET ""
check "home loan reset keeps EMI, extends tenure" "$HL .* 8.5000 +9.0000 .*TENURE"
online admin Admin2026x N 2 REPO 8.75 Y 9 9
run LNRESET ""
check "EMI raised when tenure would pass 360 months" "$HL .*EMI RAISED"
check_rc "EMI raise reported with RC 4" 4
online admin Admin2026x 5 2 "$HL" 9 9
check "inquiry shows repo-linked loan" "REPO-LINKED, SPREAD +2.00"
online admin Admin2026x N 2 REPO 7.00 Y 2 REPO 6.50 Y 9 9
run LNRESET ""

echo "== Maker-checker"
online teller1 Teller99x 2 1 1000000012 300000 "BIG CASH" \
       1 1000000012 250000 "SECOND" 1 1000000012 210000 "THIRD" \
       1 1000000012 150000 "SMALL" 9 9
check "teller item above limit queued" "SENT FOR APPROVAL, REF 00000001"
check "teller item within limit posted" "POSTED SUCCESSFULLY"
online teller1 Teller99x W 9 9
check "teller cannot open approvals" "NOT AUTHORISED FOR THIS OPTION"
online admin Admin2026x W 1 2 00000001 Y 3 00000002 "SIGNATURE MISMATCH" \
       2 00000002 4 9 9
check "pending list shows maker" "00000001 DEPOSIT .* TELLER1"
check "checker approval posts" "APPROVED AND POSTED. REF 00000001"
check "reject with reason" "REJECTED. REF 00000002"
check "decided item cannot be approved again" "ITEM IS NOT PENDING \(REJECTED\)"
TOMORROW="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=1)).strftime("%Y%m%d"))')"
run EODBATCH "$TOMORROW"
check "undecided item expires at EOD" "APPROVALS EXPIRED : +1"
NACCTS="$(grep -oE 'ACCOUNTS READ +: +[0-9]+' <<<"$OUT" | grep -oE '[0-9]+$')"

echo "== Branch cash: vault, teller drawer, tally"
online admin Admin2026x M 2 DEL001 200 100 200 0 0 0 500 \
       4 DEL001 teller1 50 0 100 0 0 0 0 \
       3 DEL001 1000 0 0 0 0 0 0 1 DEL001 9 9
check "cash received from chest" "RECEIVED FROM CHEST: +140,500.00"
check "drawer opened for teller" "DRAWER OPENED FOR TELLER1"
check "cannot remit notes the vault does not hold" "VAULT HAS ONLY +150 NOTES OF RS 500"
check "vault reduced by cash issued" "VAULT TOTAL +105,500.00"
online teller1 Teller99x 2 1 1000000012 5000 "CASH IN" 2 1000000012 2000 "CASH OUT" 9 \
       M 5 6 70 0 29 0 0 0 0 5 9 9
check "drawer tracks receipts and payments" "EXPECTED CASH +: +38,000.00"
check "shortage found at tally" "CASH SHORT BY +100.00"
run CASHRPT ""
check "cash report shows the short drawer" "DRAWER TELLER1 .*SHORT"
check_rc "drawer difference gives RC 4" 4
check "teller cash back in vault" "VAULT +143,400.00"

echo "== NACH mandates and inward debits"
TODAY="$(date +%Y%m%d)"
online admin Admin2026x Z 1 1000000014 "LIC OF INDIA" POL123456 5000 M "$TODAY" 0 \
       1 1000000018 "EASY HOME FINANCE" LN998877 1000000 A "$TODAY" 0 \
       1 1000000014 "LIC OF INDIA" POL123456 5000 M "$TODAY" 0 \
       1 1000000014 "BAD FREQ CO" X1 5000 W "$TODAY" 0 2 1000000014 9 9
check "mandate registered with UMRN" "MANDATE REGISTERED. UMRN CBNK${TODAY}00000001"
check "duplicate active mandate refused" "ACTIVE MANDATE ALREADY EXISTS"
check "bad frequency refused" "FREQUENCY MUST BE M, Q, Y OR A"
M1="CBNK${TODAY}00000001"; M2="CBNK${TODAY}00000002"
nach() {   # nach UMRN AMOUNT REF  -> one inward line
    python3 -c "import sys; u,a,r=sys.argv[1:4]; print(f'{u:<20}{int(round(float(a)*100)):013d}$TODAY{r:<20}{\"TEST CORP\":<30}')" "$@"
}
{ nach "$M1" 4500 R1; nach "$M1" 4500 R2; nach "$M1" 4500 R1; nach CBNKXXXXXXXX00000099 100 R6
  nach "$M2" 900000 R3; nach "$M1" 6000 R7; } > data/NACHIN.DAT
run NACHBAT ""
check "mandate debit accepted" "DEBITED +: +1 +4,500.00"
check "second debit in the month returned (08)" "RETURNED $M1 +4,500.00 REASON 08"
check "duplicate reference returned (09)" "REASON 09"
check "unknown mandate returned (04)" "RETURNED CBNKXXXXXXXX00000099 .*REASON 04"
check "insufficient balance returned (01)" "RETURNED $M2 .*REASON 01"
check "amount above mandate returned (06)" "REASON 06"
check_rc "returns give RC 4" 4
grep -q "^${M1}R1 .*0000000450000A00" data/NACHRESP_${TODAY}.TXT && RC=0 || RC=1
check_rc "response file carries acceptance" 0
for r in R8 R9; do nach "$M2" 900000 $r > data/NACHIN.DAT; run NACHBAT ""; done
check "third bounce in a row suspends the mandate" "MANDATE $M2 SUSPENDED AFTER 03 BOUNCES"
nach "$M2" 100 R10 > data/NACHIN.DAT; run NACHBAT ""
check "suspended mandate returns (05)" "REASON 05"
rm -f data/NACHIN.DAT
online admin Admin2026x Z 3 "$M2" 5 "$M2" 4 "$M1" Y 9 9
check "inquiry shows bounce history" "BOUNCES 003"
check "manager reactivates mandate" "REACTIVATED"
check "customer cancels mandate" "MANDATE $M1 CANCELLED"

echo "== Outward cheques (CTS)"
TODAY="$(date +%Y%m%d)"
OLD="$(python3 -c 'import datetime as d; print((d.date.today()-d.timedelta(days=120)).strftime("%Y%m%d"))')"
TMRW="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=1)).strftime("%Y%m%d"))')"
online admin Admin2026x Q 7 \
       1 1000000016 HDFC0001234 123456 "$TODAY" 50000 "RAMESH TRADERS" \
       1 1000000016 ICIC0000999 654321 "$TODAY" 20000 "SURESH GUPTA" \
       1 1000000016 HDFC0001234 123456 "$TODAY" 50000 "RAMESH TRADERS" \
       1 1000000016 CBNK0000001 111111 "$TODAY" 100 "ON US" \
       1 1000000016 SBIN0001111 222222 "$TMRW" 100 "FUTURE" \
       1 1000000016 SBIN0001111 333333 "$OLD" 100 "OLD" \
       1 1000000016 12340001111 444444 "$TODAY" 100 "BAD IFSC" 9 9 9
check "cheque lodged with image reference" "CHEQUE LODGED FOR CLEARING. REF 00000001"
check "same cheque cannot be deposited twice" "CHEQUE ALREADY DEPOSITED, REF 00000001"
check "on-us cheque refused" "ON-US CHEQUE"
check "post-dated cheque refused" "CHEQUE IS POST-DATED"
check "stale cheque refused" "CHEQUE IS STALE"
check "bad IFSC refused" "INVALID IFSC"
run CTSOUT ""
check "presentation file built" "CTSOUT: 000002 CHEQUE\(S\) PRESENTED, TOTAL +70,000.00"
grep -q "^T000002000000007000000$" data/CTSOUT_${TODAY}.TXT && RC=0 || RC=1
check_rc "trailer carries count and total" 0
printf '00000002R01\n00000077R01\n' > data/CTSRESP.DAT
run CTSRET "$TMRW"
check "drawee return applied" "RETURNED REF 00000002 +20,000.00 - FUNDS INSUFFICIENT"
check "unreturned cheque deemed cleared and credited" "CLEARED AND CREDITED : +1 +50,000.00"
check "unknown return reference reported" "RETURN FOR UNKNOWN REF 00000077"
rm -f data/CTSRESP.DAT
online admin Admin2026x Q 7 3 00000001 2 1000000016 9 9 9
check "status shows credited" "STATUS +: CREDITED"
check "account list shows returned cheque" "00000002 ICIC0000999 654321 .*RETURNED"

echo "== Interface files: credit bureau"
online admin Admin2026x 5 1 1000000003 P 100000 24 Y 9 9
PL="$(grep -oE 'LOAN [0-9]{10} DISBURSED' <<<"$OUT" | grep -oE '[0-9]{10}')"
run CIBILOUT ""
check "bureau file written" "CREDIT BUREAU FILE data/CIBIL_[0-9]{6}.TXT"
check_rc "loans without KYC give RC 4" 4
check "incomplete KYC listed" "INCOMPLETE: LOAN .* NO LINKED CUSTOMER / PAN"
CF="$(ls data/CIBIL_*.TXT | head -1)"
grep -iq "^BR${PL}20000001AMIT KUMAR .*ABCPK1234D19900515M110001" "$CF" && RC=0 || RC=1
check_rc "borrower KYC carried into the bureau record" 0
grep -q "^TR" "$CF" && RC=0 || RC=1
check_rc "trailer present" 0

echo "== Credit cards"
online admin Admin2026x K 1 20000001 1000000003 50000 N 9 9
check "credit card issued" "CREDIT CARD ISSUED"
CC="$(grep -oE 'FULL NUMBER +: +[0-9]{16}' <<<"$OUT" | grep -oE '[0-9]{16}')"
online admin Admin2026x K 1 20000001 1000000003 50000 N 9 9
check "one card per customer" "ALREADY HOLDS A CREDIT CARD"
online admin Admin2026x K 2 "$CC" P 8000 "BIG BAZAAR" 2 "$CC" P 60000 "CAR SHOP" \
       2 "$CC" C 3000 "ATM" 3 "$CC" 5000 A 9 9
check "purchase approved" "APPROVED. AUTH CODE"
check "over-limit purchase declined" "DECLINED - OVER LIMIT"
check "card payment from account" "PAYMENT RECEIVED"
run CCBATCH ""
(( RC <= 4 )) && RC=0; check_rc "card batch runs (RC <= 4)" 0

echo "== Gold loan"
online admin Admin2026x O 4 7000 1 1000000004 10 22 40000 12 Y 9 9
check "gold loan disbursed" "DISBURSED. EMI"
online admin Admin2026x O 1 1000000004 10 22 90000 12 9 9
check "gold loan above 75% LTV refused" "5,000 TO THE MAXIMUM"
run GOLDREVL 3000
check "revaluation raises margin call" "MARGIN CALL"

echo "== JSON API gateway"
printf '%-32s %-20s %s\n' test-key-r TESTCLIENT R test-key-w TESTCLIENT W \
    > data/APIKEYS.DAT
run APIGW '{"op":"balance","apikey":"test-key-r","account":"1000000002"}' \
    '{"op":"balance","apikey":"nope","account":"1000000002"}' \
    '{"op":"transfer","apikey":"test-key-r","from":"1000000002","to":"1000000009","amount":10}' \
    '{"op":"transfer","apikey":"test-key-w","from":"1000000002","to":"1000000009","amount":10}'
check "balance over API" '"status":"ok".*"balance"'
check "bad key rejected" "UNAUTHORISED"
check "read-only key cannot transfer" "FORBIDDEN"
check "transfer over API" '"status":"ok".*"from_balance"'

if [[ -f bin/SQLLOAD.so ]] && PGPASSWORD=corebank psql -h localhost -U corebank \
       corebank -c 'select 1' >/dev/null 2>&1; then
    echo "== SQL feed (PostgreSQL)"
    run SQLLOAD
    check "load committed" "LOAD COMMITTED"
    run SQLRPT
    check "SQL recon balanced" "RESULT: BALANCED"
    run SQLINQ 1000000003 "?KUMAR" ""
    check "SQL enquiry: account row by host variable" "ACCOUNT 1000000003"
    check "SQL enquiry: recent transactions cursor" "RECENT TRANSACTION\(S\)"
    check "SQL enquiry: LIKE name search" "MATCHING ACCOUNT\(S\)"
else
    echo "== SQL feed skipped (no PostgreSQL / not built with make sql)"
fi

echo "== Regulatory returns"
online admin Admin2026x N 2 CRR 4.00 Y 9 9
run REGRPT ""
check "NDTL computed from the ledger" "NDTL +[1-9][0-9,]+"
check "CRR uses the rate master (changed to 4%)" "CRR REQUIRED @  4.00%"
check "SLR at default 18%" "SLR REQUIRED @ 18.00%"
check "liquidity buckets printed" "OVER 5 YRS"
check "size classes printed" "SAVINGS DEP\. +[0-9]+"
python3 - "$(ls data/REGRPT_*.TXT | head -1)" <<'EOF' && RC=0 || RC=1
import sys
lines=open(sys.argv[1]).read().splitlines()
start=[i for i,l in enumerate(lines) if l.startswith('BUCKET')][0]
infl=sum(int(l[26:42].replace(',','')) for l in lines[start+1:start+10])
loans=0
for l in lines:
    if l[:14].strip() in ('HOME LOANS','PERSONAL LOANS','VEHICLE LOANS','GOLD LOANS','LOANS AGNST FD'):
        for k in range(4):
            loans+=int(l[14+21*k+5:14+21*k+21].replace(',',''))
sys.exit(0 if abs(infl-loans) <= 50 else 1)
EOF
check_rc "loan inflows across buckets add up to the loan book" 0
online admin Admin2026x N 2 CRR 3.00 Y 9 9

echo "== Loan origination"
online teller1 Teller99x 5 6 \
       1 1000000003 P 200000 36 80000 5000 760 \
       1 1000000004 P 100000 24 50000 0 780 \
       1 1000000003 P 300000 24 10000 0 760 \
       1 1000000003 H 3000000 240 300000 0 800 9 9 9
check "application assessed and graded" "APPLICATION 00000001 .*FOIR"
check "good applicant gets grade A" "SCORE 090 GRADE A"
check "no KYC-verified holder -> rejected by rule" "REJECTED BY RULE: ACCOUNT HOLDER NOT KYC-VERIFIED"
check "repayment capacity rule" "REJECTED BY RULE: REPAYMENT CAPACITY: FOIR ABOVE LIMIT"
check "large loan needs the credit committee" "NEEDS CREDIT COMMITTEE APPROVAL"
online admin Admin2026x 5 6 3 00000001 A 4 00000001 Y 3 00000002 \
       1 1000000003 P 50000 12 90000 0 780 3 00000005 5 00000001 9 9 9
check "manager sanctions" "SANCTIONED. VALID TILL"
check "approved application disbursed" "LOAN [0-9]{10} DISBURSED. FIRST EMI DUE"
check "rejected application cannot be decided again" "APPLICATION IS REJECTED"
check "maker cannot decide own application" "ANOTHER OFFICER MUST DECIDE IT"
check "status shows the loan number" "LOAN NO +: [0-9]{10}"

echo "== IRAC provisioning"
run PROVBAT ""
check "NPA loans classified sub-standard" "SUBSTANDRD +[1-9]"
check "provision charged to P&L" "CHARGED \(\+\) / RELEASED\(-\) +[1-9][0-9,]*\.[0-9]{2}"
run PROVBAT ""
check "re-run books nothing more" "CHARGED \(\+\) / RELEASED\(-\) +0\.00"

echo "== Collections and recovery"
run COLLBAT ""
check "reminder notices issued" "NOTICES IN data/REMIND_"
check "recall notices for 90+ DPD" "LOAN RECALL NOTICE +: +[1-9]"
run COLLBAT ""
check "same stage not notified twice" "ALREADY NOTIFIED AT THIS STAGE : +[1-9]"
online admin Admin2026x 5 7 1 9 9 9
mapfile -t NPAS < <(grep -oE '^ +[0-9]+ [0-9]{10} ' <<<"$OUT" | awk '{print $2}')
L1="${NPAS[0]}"; L2="${NPAS[1]}"; L3="${NPAS[2]}"
A2="$(grep -oE "^ +[0-9]+ ${L2} [0-9]{10}" <<<"$OUT" | awk '{print $3}')"
check "worklist lists overdue loans, oldest first" "OVERDUE LOAN\(S\)"
FUT="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=3)).strftime("%Y%m%d"))')"
LATER="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=10)).strftime("%Y%m%d"))')"
online admin Admin2026x 2 1 "$A2" 2000000 "OTS FUNDS" 9 \
       5 7 2 "$L3" P "$FUT" 25000 "PROMISED BY PHONE" 3 "$L3" \
       5 "$L1" Y 6 "$L1" 1000 4 "$L2" 1 9 9 9
check "promise to pay recorded" "FOLLOW-UP RECORDED FOR LOAN $L3"
check "history shows the promise" "PROMISED +25,000.00 BY $FUT"
check "NPA loan written off against provision" "LOAN $L1 WRITTEN OFF"
check "recovery after write-off booked as income" "RECOVERED +1,000.00 - BOOKED AS INCOME"
check "OTS below 60% refused" "SETTLEMENT MUST BE BETWEEN"
MINOTS="$(grep -oE 'MINIMUM OTS : +[0-9,.]+' <<<"$OUT" | grep -oE '[0-9,.]+$' | tr -d ,)"
online admin Admin2026x 5 7 4 "$L2" "$MINOTS" Y 9 9 9
check "OTS settles and closes the loan" "LOAN $L2 SETTLED AND CLOSED"
run COLLBAT "$LATER"
check "broken promise flagged" "BROKEN PROMISE: LOAN $L3"

echo "== Fraud monitoring"
# a mule-test account with no large credits today (TESTGEN dates its
# history today) and not used by the other fraud cases
MULE="$(python3 - <<'EOF'
big=set(); seen=[]
for l in open('data/TRANLOG.DAT'):
    a=l[12:22]; t=l[36:38]; amt=int(l[38:51])
    if a not in seen: seen.append(a)
    if t in ('DP','TI','IN','LD','FM','PI','OC') and amt >= 10000000: big.add(a)
skip={'1000000019','1000000026','1000000027','1000000031'}
print([a for a in seen if a not in big and a not in skip][5])
EOF
)"
online admin Admin2026x 2 2 1000000019 100 W1 2 1000000019 100 W2 \
       2 1000000019 100 W3 2 1000000019 100 W4 2 1000000019 100 W5 \
       2 1000000019 100 W6 3 1000000026 150000 BIG 1000000027 \
       1 "$MULE" 200000 IN 3 "$MULE" 190000 OUT 1000000031 9 9
run FRAUDBAT ""
check "velocity rule (6 debits)" "ALERT VL 1000000019"
check "large first transfer to a new payee" "ALERT NP 1000000026"
check "credit moved straight out (mule pattern)" "ALERT PT $MULE"
check_rc "new alerts give RC 4" 4
run FRAUDBAT ""
check "re-scan raises nothing twice" "NEW ALERTS +: +0"
TODAY="$(date +%Y%m%d)"
online admin Admin2026x 0 1 1 2 "$TODAY" "$MULE" PT F "MULE ACCOUNT" \
       2 "$TODAY" 1000000019 VL G "CUSTOMER CONFIRMED" 9 9 \
       2 2 "$MULE" 100 "AFTER FREEZE" 9 9
check "open alerts listed" "OPEN ALERT\(S\)"
check "confirmed fraud freezes the account" "ACCOUNT $MULE FROZEN"
check "genuine alert closed" "ALERT CLOSED AS GENUINE"
check "frozen account refuses debits" "NOT ACTIVE"

echo "== Trade finance (BG / LC)"
EXP1="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=365)).strftime("%Y%m%d"))')"
EXP2="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=90)).strftime("%Y%m%d"))')"
EXP3="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=60)).strftime("%Y%m%d"))')"
LATE="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=100)).strftime("%Y%m%d"))')"
online admin Admin2026x 2 1 1000000021 600000 "TF FUNDS" 9 0 2 \
       1 G 1000000021 "PWD ROADS DIVISION" 500000 "$EXP1" 20 Y \
       1 L 1000000021 "ABC EXPORTS GMBH" 200000 "$EXP2" 0 Y \
       1 G 1000000021 "LOW MARGIN CO" 100000 "$EXP1" 5 \
       2 00000002 150000 2 00000002 60000 \
       3 00000001 300000 \
       1 G 1000000021 "TENDER DEPOSIT" 100000 "$EXP3" 10 Y 4 00000003 Y \
       5 00000001 9 9 9
check "BG priced: 20% margin, 1.5% commission + GST" "MARGIN +100,000.00  COMMISSION \+ GST +8,850.00"
check "BG issued with claim period" "GUARANTEE 00000001 ISSUED"
check "LC minimum commission 1,000 + GST" "COMMISSION \+ GST +1,180.00"
check "BG margin below 10% refused" "MARGIN MUST BE 10-100% FOR A BG"
check "LC documents paid" "LC DRAWING OF +150,000.00 PAID"
check "drawing above LC balance refused" "AMOUNT MUST BE WITHIN THE AVAILABLE BALANCE"
check "BG invoked: margin used first, rest from account" "BG INVOKED - +300,000.00 PAID"
check "returned BG cancelled, margin released" "INSTRUMENT 00000003 CANCELLED, MARGIN RELEASED"
check "inquiry shows invoked status" "GUARANTEE 00000001 +INVOKED"
run TFBATCH "$LATE"
check "LC past expiry closed by batch" "EXPIRED L 00000002"
check "contingent liability register printed" "NET EXPOSURE"

echo "== Forex and NRI remittances"
online admin Admin2026x 2 1 1000000022 900000 "FX FUNDS" 1 1000000022 900000 "FX 2" 9 0 3 1 \
       4 USD 1000000022 1000 "JOHN SMITH" P1301 Y \
       5 USD 1000000022 5000 "UNIV OF TORONTO" S0305 Y \
       5 USD 1000000022 10000 "UNIV OF TORONTO" S0305 Y \
       5 USD 1000000022 240000 "PROPERTY ABROAD" S0005 \
       3 1000000023 E 2 USD 95 96 2 USD 83.50 84.70 6 1000000022 9 9 9
check "card rates shown" "USD US DOLLAR"
check "inward converted at buy rate" "INWARD REMITTANCE CREDITED: RS +83,100.00"
check "first outward under the TCS threshold" "TCS +0.00"
check "TCS 5% on education above Rs 10 lakh in the year" "TCS +13,225.00"
check "LRS USD 2.5 lakh limit enforced" "LRS LIMIT USD 2,50,000 EXCEEDED"
check "account marked NRE" "ACCOUNT 1000000023 RESIDENCY SET TO E"
check "rate jump over 5% refused" "RATE MOVES MORE THAN 5%"
check "remittance history" "003 REMITTANCE\(S\)"

echo "== Statements"
FROMD="$(date +%Y%m)01"; TODAY="$(date +%Y%m%d)"
online admin Admin2026x 0 4 1000000003 "$FROMD" "$TODAY" E 4 1000000016 "$FROMD" "$TODAY" E \
       4 1000000016 20200101 "$TODAY" P 9 9
check "request queued for e-mail" "STATEMENT REQUEST QUEUED \(E\)"
check "no e-mail on record falls back to post" "NO E-MAIL ON RECORD - WILL BE POSTED"
check "period over a year refused" "AT MOST ONE YEAR"
run STMTBAT "${TODAY:0:6}"
check "monthly statements for active accounts" "MONTHLY STATEMENTS +: +[1-9][0-9]"
check "requested statements produced" "REQUESTED STATEMENTS : +2"
check "statement with e-mail queued for mail" "QUEUED FOR E-MAIL +: +[1-9]"
grep -q "TO=amit@example.com|SUBJECT=STATEMENT 1000000003" data/MAILQ.DAT && RC=0 || RC=1
check_rc "mail queue line carries address and attachment" 0
python3 - <<'EOF' && RC=0 || RC=1
import glob, sys
bad = 0
files = glob.glob('data/STMT_*.TXT')
for f in files:
    L = open(f).read().splitlines()
    op = [l for l in L if 'OPENING BALANCE' in l][0]
    cl = [l for l in L if 'CLOSING BALANCE' in l][0]
    n = lambda s: float(s.replace(',', '').strip() or 0)
    if abs(n(op[83:100]) + n(cl[66:82]) - n(cl[50:66]) - n(cl[83:100])) > 0.005:
        bad += 1
sys.exit(1 if bad or not files else 0)
EOF
check_rc "every statement: opening + credits - debits = closing" 0
run STMTBAT "${TODAY:0:6}"
check "request queue emptied after the run" "REQUESTED STATEMENTS : +0"

echo "== Audit exception report"
run AUDEXRPT "" ""
check "failed sign-ons (3+ a day) reported" "USER TELLER1 +FAILED SIGN-ONS"
check "refused access reported" "DENIED"
check "sensitive changes listed" "RATEMNT +RATE CHANGE"
check "activity by user" "ADMIN +ACTIONS"
check_rc "exceptions give RC 4" 4

echo "== Deposit rate card versions"
TOMORROW="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=1)).strftime("%Y%m%d"))')"
YESTERDAY="$(python3 -c 'import datetime as d; print((d.date.today()-d.timedelta(days=1)).strftime("%Y%m%d"))')"
online teller1 Teller99x 0 A 2 9 9 9
check "teller cannot set a rate card" "ONLY A MANAGER CAN SET A RATE CARD"
online admin Admin2026x 0 A 4 6 N "" \
       2 "$YESTERDAY" \
       2 "$TODAY" 12 6.8 7.3 24 7.1 8.5 \
       2 "$TODAY" 12 6.8 7.3 24 7.1 7.6 60 7.2 7.7 "" \
       2 "$TODAY" 12 6.8 7.3 24 7.1 7.6 120 7.0 7.5 Y \
       2 "$TOMORROW" 12 7.3 7.8 120 7.5 8.0 Y \
       4 18 Y "" 4 18 N "$TOMORROW" 4 18 N 20200101 1 "" 3 \
       5 "$TODAY" 5 "$TOMORROW" 3 9 9 9
check "built-in card until one is set" "RATE  6.5000% P.A. \(BUILT-IN CARD\)"
check "back-dated card refused" "A CARD CANNOT BE BACK-DATED"
check "senior rate capped at 1% over general" "SENIOR RATE MUST BE 0 - 1% ABOVE GENERAL"
check "card must end at 120 months" "LAST SLAB MUST END AT 120 MONTHS"
check "card saved" "RATE CARD EFFECTIVE $TODAY SAVED"
check "senior column used today" "RATE  7.6000% P.A. \(CARD EFFECTIVE $TODAY\)"
check "future card applies from its date" "RATE  7.5000% P.A. \(CARD EFFECTIVE $TOMORROW\)"
check "old dates keep the built-in card" "RATE  7.0000% P.A. \(BUILT-IN CARD\)"
check "card in force listed" "      120       7.0000    7.5000"
check "card in force cannot be withdrawn" "ONLY A FUTURE CARD CAN BE WITHDRAWN"
check "future card withdrawn" "CARD $TOMORROW WITHDRAWN \(02 SLABS\)"
online admin Admin2026x 8 1 1000000012 10000 12 N Y 9 9
check "new FD booked on today's card" "RATE CARD EFFECTIVE $TODAY"
check "new FD gets the new rate" "RATE  6.80% P.A."

echo "== Sweep and overdraft against FD"
online admin Admin2026x 0 8 1 1000000013 10000 2000 10000 12 \
       1 1000000013 50000 60000 10000 12 \
       1 1000000013 50000 20000 7500 12 \
       1 1000000013 50000 20000 10000 12 \
       1 1000000013 50000 20000 10000 12 9 9 9
check "threshold minimum" "THRESHOLD MUST BE AT LEAST 25,000"
check "floor below threshold" "FLOOR MUST BE BELOW THE THRESHOLD"
check "FD unit in thousands" "FD UNIT MUST BE 5,000 OR MORE IN MULTIPLES OF 1,000"
check "sweep registered" "AUTO-SWEEP REGISTERED FOR 1000000013"
check "one sweep per account" "ACCOUNT ALREADY HAS AN ACTIVE SWEEP"
run SWEEPBAT ""
check_rc "sweep batch RC 0" 0
check "surplus swept into FDs" "SWEPT OUT +: +[1-9][0-9]* FDS"
SWOUT="$(grep -c "^1000000013  OUT" data/SWEEP_${TODAY}.TXT)"
run SWEEPBAT ""
check "same day re-run does nothing" "MANDATES PROCESSED :       0"
online admin Admin2026x 0 8 2 1000000013 9 9 9
check "view lists the sweep FDs" "LIVE SWEEP FDS: 0*$SWOUT "
online admin Admin2026x 0 8 3 1000000013 1 1000000013 900000 880000 10000 12 9 9 9
check "mandate cancelled and re-registered" "AUTO-SWEEP REGISTERED FOR 1000000013 - ABOVE 900,000.00"
run SWEEPBAT ""
OUT="$OUT
$(cat data/SWEEP_${TODAY}.TXT)"
check "balance below floor: newest sweep FD broken first" "1000000013  IN   7[0-9]{9} "
check "sweep-in counted" "SWEPT IN +: +[1-9]"
online admin Admin2026x 8 1 1000000024 20000 12 N Y 9 9
FDOD="$(grep -o 'DEPOSIT 7[0-9]* OPENED' <<<"$OUT" | grep -o '7[0-9]*')"
online admin Admin2026x 0 8 4 1000000012 "$FDOD" 4 1000000024 "$FDOD" Y \
       4 1000000024 "$FDOD" 9 9 8 5 "$FDOD" 9 0 8 5 "$FDOD" 9 9 9
check "OD only on current accounts" "OVERDRAFT IS ONLY FOR CURRENT ACCOUNTS"
check "OD limit = 90% of FD at FD rate + 2%" "OD LIMIT \+18,000.00 AT  8.80% P.A."
check "FD marked under lien" "FD $FDOD UNDER LIEN"
check "FD cannot be pledged twice" "FD IS ALREADY UNDER LIEN \(1000000024\)"
check "pledged FD cannot be closed" "DEPOSIT SECURES THE OVERDRAFT OF 1000000024"
check "FD released" "FD $FDOD RELEASED"

echo "== Branch stock (cheque books, cards)"
online admin Admin2026x 0 6 2 "" CB10 2 1 20 2 "" DCRD 0 1 5 2 "" CB10 1 1 1 \
       2 MUM002 CB10 0 1 10 1 "" 9 9 \
       Q 1 1000000014 10 1 1000000015 10 1 1000000016 10 9 \
       6 1 1000000014 7391 7391 9 9
check "item put under stock control" "CB10 NOW STOCK-CONTROLLED AT HO0001"
check "item cannot be set up twice" "CB10 IS ALREADY STOCK-CONTROLLED AT HO0001"
check "stock position" "CB10 CHEQUE BOOK - 10 LEAVES +2 +1 +0"
check "issue warns at reorder level" "NOTE: CB10 STOCK AT REORDER LEVEL"
check "no stock - cheque book refused" "NO 10-LEAF CHEQUE BOOKS IN STOCK AT HO0001"
check "no card blanks - card refused" "NO DEBIT CARD BLANKS IN STOCK AT HO0001"
online admin Admin2026x 0 6 4 "" CB10 MUM002 5 3 "" CB10 50 "CH/2026/88" \
       4 "" CB10 MUM002 5 5 "" CB10 3 DAMAGED 7 "" CB10 9 9 9
check "receipt adds stock" "RECEIVED. CB10 ON HAND AT HO0001: 50"
check "transfer needs stock" "ONLY 0 ON HAND AT HO0001"
check "transfer between branches" "5 CB10 TRANSFERRED HO0001 -> MUM002"
check "write-off with reason" "3 CB10 WRITTEN OFF - DAMAGED"
check "movement history" "0000[6-9] MOVEMENT\(S\)"
run INVBATCH ""
check_rc "items below reorder level give RC 4" 4
check "reorder raised automatically" "HO0001 DCRD +0 +1 +5  ORDER RAISED \(OUT\)"
run INVBATCH ""
check "no second order while one is open" "HO0001 DCRD .*ON ORDER - CHASE"

echo "== Complaints and service requests"
D9="$(python3 -c 'import datetime as d; print((d.date.today()-d.timedelta(days=9)).strftime("%Y%m%d"))')"
online teller1 Teller99x 0 7 1 1000000014 C ATMF B "CASH NOT DISPENSED" "$D9" 5000 \
       1 1000000014 C CHRG P "SMS CHARGE TWICE" \
       1 1000000015 S SERV B "ADDRESS CHANGE" \
       1 1000000015 C XXXX B "BAD" \
       4 00000001 9 9 9
check "ATM complaint logged with T+5 due date" "TICKET 00000001 LOGGED \(ATM / CARD DEBIT FAILED"
check "service request logged" "TICKET 00000003 LOGGED \(SERVICE REQUEST"
check "unknown category refused" "UNKNOWN CATEGORY XXXX"
check "maker cannot resolve own ticket" "A TELLER CANNOT RESOLVE A TICKET THEY LOGGED"
online admin Admin2026x 0 7 6 4 00000001 R "EJ SHOWS NOT DISPENSED" Y \
       4 00000002 R "SMS CHARGE REVERSED" 20000 4 00000002 R "SMS CHARGE REVERSED" 150 \
       5 00000002 N 5 00000001 Y 2 00000001 9 9 9
check "pending list flags overdue" "00000001 ATMF  1 .*OVERDUE"
check "ATM refund + Rs 100/day after T+5" "REFUND 5,000.00 \+ 4 DAY\(S\) LATE x 100 = 400.00"
check "goodwill capped" "GOODWILL COMPENSATION IS CAPPED AT 10,000"
check "dissatisfied customer reopens one level up" "TICKET 00000002 REOPENED AT LEVEL 2"
check "satisfied customer closes" "TICKET 00000001 CLOSED"
check "ticket shows refund and compensation" "REFUND +5,000.00  COMPENSATION +400.00"
D20="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=20)).strftime("%Y%m%d"))')"
D40="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=40)).strftime("%Y%m%d"))')"
run CMPBATCH ""
check_rc "nothing overdue today" 0
run CMPBATCH "$D20"
check_rc "escalations give RC 4" 4
check "service request escalated to regional office" "00000003 SERV .*LEVEL 2 REGIONAL OFFICE"
check "reopened complaint escalated to nodal officer" "00000002 CHRG .*LEVEL 3 NODAL OFFICER"
run CMPBATCH "$D40"
check "complaints pending over 30 days for ombudsman" "RBI OMBUDSMAN\): 1"
OUT="$(cat data/OMBUD_${TODAY:0:6}.TXT 2>/dev/null)"
check "monthly return written" "CUSTOMER COMPLAINTS RETURN"

echo "== Mutual funds and insurance"
online teller1 Teller99x 0 5 1 9 9 9
check "teller cannot maintain products" "ONLY A MANAGER CAN MAINTAIN PRODUCTS"
online admin Admin2026x 0 5 1 EQGRW1 "BLUECHIP EQUITY GROWTH" M EQTY H 50 1 1000 1 365 \
       1 TERM01 "TERM SHIELD 1 CR" I TERM L 30 5000 \
       1 EQGRW1 80 Y \
       2 1000000016 EQGRW1 \
       2 1000000003 EQGRW1 500 \
       2 1000000003 EQGRW1 20000 Y \
       3 1000000003 EQGRW1 100 \
       4 1000000003 EQGRW1 300 5 12 \
       4 1000000003 EQGRW1 1000 5 12 \
       7 1000000003 TERM01 100000 12000 Y \
       7 1000000003 TERM01 200000 12000 Y \
       7 1000000003 TERM01 200000 12000 Y \
       6 1000000003 9 9 9
check "fund added" "PRODUCT EQGRW1 ADDED"
check "insurance plan added" "PRODUCT TERM01 ADDED"
check "big NAV move needs confirmation" "NAV MOVES 60.00% - CONFIRM"
check "KYC required for third-party products" "IS NOT VERIFIED - NO THIRD-PARTY PRODUCTS|NO CUSTOMER LINKED"
check "below minimum refused" "AMOUNT BELOW THE PRODUCT MINIMUM"
check "units allotted at NAV" "250.000 UNITS OF EQGRW1 ALLOTTED AT NAV 80.0000"
check "exit load inside 365 days" "100.000 UNITS REDEEMED, VALUE 8,000.00 EXIT LOAD 80.00"
check "SIP minimum" "MINIMUM SIP IS 500 A MONTH"
check "SIP registered" "SIP 00000001 REGISTERED"
check "sum assured at least 10x premium" "SUM ASSURED MUST BE AT LEAST 10 x PREMIUM"
check "policy issued with commission" "FIRST PREMIUM 12,000.00 COLLECTED \(COMMISSION 3,600.00\)"
check "one policy per plan" "ACCOUNT ALREADY HOLDS POLICY"
check "portfolio value" "FUNDS: COST 12,000.00  VALUE 12,000.00  GAIN 0.00"
SIPD="$(python3 -c 'import datetime as d; t=d.date.today(); m=t.month%12+1; y=t.year+(t.month==12); print("%04d%02d05"%(y,m))')"
run MFBATCH "$SIPD"
check "SIP instalment invested" "SIPS INVESTED +: +1 "
check "collections remitted to AMC / insurer" "REMITTED TO AMC/INSURERS : +[1-9]"
run MFBATCH "$SIPD"
check "SIP not taken twice" "SIPS INVESTED +: +0 "

echo "== Branch master and inter-branch accounting"
online teller1 Teller99x 0 B 9 9
check "branch master hidden from tellers" "YOU ARE NOT AUTHORISED FOR THIS OPTION"
online admin Admin2026x 0 B 4 \
       1 PUN006 "PUNE CAMP" CB1N0PUN006 PUNE WEST B \
       1 PUN006 "PUNE CAMP" CBIN0PUN006 PUNE WEST B \
       1 HO0001 "HEAD OFFICE" CBIN0000001 MUMBAI WEST H \
       1 HO0002 "SECOND HO" CBIN0HO0002 MUMBAI WEST H \
       1 PUN006 \
       3 KOL003 3 HO0001 3 PUN006 2 9 9 9
check "branch codes on accounts registered" "0[0-9][1-9] BRANCH CODE\(S\) REGISTERED FROM ACCOUNTS"
check "IFSC format checked" "IFSC MUST BE 4 LETTERS, 0, THEN 6"
check "branch added" "BRANCH PUN006 ADDED"
check "only one head office" "THERE IS ALREADY A HEAD OFFICE"
check "duplicate branch refused" "BRANCH PUN006 ALREADY EXISTS"
check "branch with accounts cannot close" "BRANCH STILL HAS [0-9]+ OPEN ACCOUNT\(S\)"
check "head office cannot close" "THE HEAD OFFICE CANNOT BE CLOSED"
check "empty branch closed" "BRANCH PUN006 CLOSED"
check "branch list" "PUN006 PUNE CAMP .*CBIN0PUN006 B C"
run BRGLBAT ""
check_rc "inter-branch reconciled (RC 0)" 0
check "transfer legs matched" "TRANSFERS MATCHED +: +[1-9]"
check "IB positions net to zero" "INTER-BRANCH RECONCILED"
OUT="$(cat data/BRGL_${TODAY}.TXT)"
check "branch position printed" "^BANK +[0-9,]+\.[0-9]{2}"
check "branch-pair traffic printed" "BRANCH PAIRS +FROM +TO"
cp data/TRANLOG.DAT data/TRANLOG.BAK
python3 - <<PY
for l in open('data/TRANLOG.DAT'):
    if l[36:38]=='TO' and l[22:30]=='$TODAY':
        open('data/TRANLOG.DAT','a').write(l[:38]+'0000000012345'+l[51:]); break
PY
run BRGLBAT ""
check_rc "unmatched leg gives RC 4" 4
check "debit leg without credit listed" "UNMATCHED .*DEBIT LEG WITHOUT CREDIT"
mv data/TRANLOG.BAK data/TRANLOG.DAT

echo "== Ledger after all modules"
run GLRPT
check_rc "GL still balanced and reconciled (RC 0)" 0
check "trial balance still agrees" "TRIAL BALANCE AGREES"

echo "== Job streams"
OUT="$(timeout 120 bash jobs/EODJOB.sh 2>&1)"; RC=$?
check "EODJOB completes" "JOB EODJOB COMPLETE"
OUT="$(timeout 120 bash jobs/MONTHEND.sh 2>&1)"; RC=$?
check "MONTHEND completes" "JOB MONTHEND COMPLETE"


echo "== Checkpoint / restart"
D2="$(python3 -c 'import datetime as d; print((d.date.today()+d.timedelta(days=2)).strftime("%Y%m%d"))')"
CB_ABEND_AT=EODBATCH:2 run EODBATCH "$D2"
check "simulated crash after 2nd commit" "SIMULATED ABEND AFTER COMMIT"
check_rc "crash ends with RC 16" 16
run EODBATCH "$D2"
check "restart resumes after last committed account" "RESTART +1 - STEPS DONE: 00, RESUME AFTER ACCOUNT 1000000020"
check "restored counters cover every account" "ACCOUNTS READ +: +$NACCTS\b"
OUT="$(CB_ABEND_AT=EODBATCH:1 timeout 120 bash jobs/EODJOB.sh "$D2" 2>&1)"; RC=$?
check "job abends in the EOD step" "STEP STEP050 ENDED RC=16 .*JOB ABENDED"
OUT="$(RESTART=Y timeout 120 bash jobs/EODJOB.sh "$D2" 2>&1)"; RC=$?
check "job restart skips finished steps" "STEP STEP010 SKIPPED - COMPLETED BEFORE RESTART"
check "job restart completes" "JOB EODJOB COMPLETE"

echo "== Term deposit maturity with TDS, then dormancy (5 years on)"
online admin Admin2026x 2 1 1000000011 600000 "FD MONEY" 9 \
       8 1 1000000011 500000 60 N Y 9 9
check "FD opened" "DEPOSIT 7[0-9]{9} OPENED"
FUTURE="$(python3 -c 'import datetime as d; t=d.date.today(); print(f"{t.year+5}{t.month:02d}{min(t.day,28)+0:02d}")')"
FUTURE="$(python3 -c "import datetime as d; print((d.datetime.strptime('$FUTURE','%Y%m%d')+d.timedelta(days=5)).strftime('%Y%m%d'))")"
run EODBATCH "$FUTURE"
check "matured FD paid net of TDS" "MATURED, NET OF TDS"
check "inactive accounts marked dormant" "MARKED DORMANT +: +[1-9]"
run TDSRPT "" ""
run TDSRPT "${FUTURE:0:4}" ""
check "TDS certificate produced" "CERTIFICATE OF TAX DEDUCTED"
online admin Admin2026x 2 2 1000000020 100 "AFTER 5 YEARS" 9 9
check "dormant account cannot be debited" "NOT ACTIVE"
online admin Admin2026x 1 6 1000000020 Y 9 9
check "manager reactivates dormant account" "REACTIVATED"
run GLRPT
check_rc "GL balanced after maturity and TDS" 0
check "term deposits back to zero in GL" "TERM DEPOSITS \(2201\) +0\.00 +0\.00"

echo "== TDS reconciliation with Form 26AS"
online admin Admin2026x H 2 1000000011 20000002 9 9
FY="${FUTURE:0:4}"; (( 10#${FUTURE:4:2} < 4 )) && FY=$((FY - 1))
BANKTDS="$(python3 -c "
t=0
for l in open('data/TDSLOG.DAT'):
    if l[12:22]=='1000000011' and l[8:12]=='$FY': t+=int(l[49:62])
print(f'{t:013d}')")"
{ echo "XYZPK4321Q${FY}${BANKTDS}"; echo "ZZZPK0000Z${FY}0000000050000"; } > data/F26AS.DAT
run TDS26AS "$FY"
check "bank TDS matches 26AS for the PAN" "XYZPK4321Q .*MATCHED"
check "26AS credit unknown to the bank flagged" "ZZZPK0000Z .*NOT IN BANK"
check_rc "exceptions give RC 4" 4
{ echo "XYZPK4321Q${FY}0000000000100"; } > data/F26AS.DAT
run TDS26AS "$FY"
check "amount difference flagged" "XYZPK4321Q .*AMOUNT DIFFERS"
rm -f data/F26AS.DAT

echo "== Year-end closing"
YE1="$(( ${TODAY:0:4} + 6 ))0331"
YE2="$(( ${TODAY:0:4} + 7 ))0331"
run YREND "$(( ${TODAY:0:4} + 6 ))0330"
check_rc "year-end must be 31 March (RC 8)" 8
run YREND "$YE1"
check_rc "year closed (RC 0)" 0
check "net result appropriated" "TRANSFER TO STATUTORY RESERVE|LOSS ABSORBED BY GENERAL RESERVE"
check "income and expense closed" "NEW YEAR OPENS +WITH ZERO INCOME AND EXPENSE"
OUT="$(cat data/YE_${YE1:0:4}.TXT)"
check "P&L statement written" "PROFIT AND LOSS ACCOUNT - YEAR ENDED $YE1"
run GLRPT
check_rc "ledger balanced after closing" 0
check "income closed to reserves" "TOTAL INCOME +0\.00"
run YREND "$YE1"
check_rc "same year cannot close twice (RC 8)" 8
check "already-closed year named" "WAS ALREADY CLOSED"
OUT="$(timeout 120 bash jobs/YEAREND.sh "$YE2" 2>&1)"; RC=$?
check "YEAREND job completes" "JOB YEAREND COMPLETE"

echo "== CICS programs (offline syntax check, no CICS here)"
OUT="$(bash "$ROOT/cics/check.sh" 2>&1)"; RC=$?
check_rc "CBSIGN CBMENU CBINQ CBXFR compile around EXEC CICS" 0

echo
echo "RESULT: $PASS passed, $FAIL failed"
exit "$FAIL"
