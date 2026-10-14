-- ---------------------------------------------------------------------------
-- CoreBank relational schema (PostgreSQL)
--
-- The online system keeps its master data in indexed (VSAM-style) files.
-- SQLLOAD copies them into these tables every night so that reporting and
-- analytics can use SQL, the way many banks feed DB2 from VSAM.
-- Amounts are NUMERIC(15,2); dates are DATE.
-- ---------------------------------------------------------------------------
DROP TABLE IF EXISTS transactions, loans, deposits, accounts, customers,
                     gl_balances CASCADE;

CREATE TABLE customers (
    cust_id        INTEGER      PRIMARY KEY,
    name           VARCHAR(40)  NOT NULL,
    dob            DATE,
    pan            CHAR(10),
    mobile         CHAR(10),
    city           VARCHAR(20),
    state          VARCHAR(20),
    kyc_status     CHAR(1)      NOT NULL CHECK (kyc_status IN ('P','V','R')),
    risk           CHAR(1)      CHECK (risk IN ('L','M','H'))
);

CREATE TABLE accounts (
    acct_no        BIGINT       PRIMARY KEY,
    holder_name    VARCHAR(40)  NOT NULL,
    acct_type      CHAR(1)      NOT NULL CHECK (acct_type IN ('S','C')),
    status         CHAR(1)      NOT NULL,
    balance        NUMERIC(15,2) NOT NULL,
    od_limit       NUMERIC(15,2) NOT NULL DEFAULT 0,
    branch         CHAR(6)      NOT NULL,
    opened_on      DATE,
    last_txn_on    DATE
);
CREATE INDEX accounts_branch ON accounts (branch);

CREATE TABLE transactions (
    tran_id        BIGINT       NOT NULL,
    acct_no        BIGINT       NOT NULL,
    tran_date      DATE         NOT NULL,
    tran_type      CHAR(2)      NOT NULL,
    amount         NUMERIC(15,2) NOT NULL,
    balance_after  NUMERIC(15,2) NOT NULL,
    narration      VARCHAR(30),
    seq            INTEGER      NOT NULL,
    PRIMARY KEY (seq)
);
CREATE INDEX transactions_acct ON transactions (acct_no, tran_date);

CREATE TABLE loans (
    loan_no        BIGINT       PRIMARY KEY,
    acct_no        BIGINT       NOT NULL,
    loan_type      CHAR(1)      NOT NULL,
    status         CHAR(1)      NOT NULL,
    principal      NUMERIC(15,2) NOT NULL,
    rate           NUMERIC(7,4) NOT NULL,
    tenure_months  INTEGER      NOT NULL,
    emi            NUMERIC(15,2) NOT NULL,
    outstanding    NUMERIC(15,2) NOT NULL,
    overdue_emis   INTEGER      NOT NULL,
    next_due       DATE
);

CREATE TABLE deposits (
    dep_no         BIGINT       PRIMARY KEY,
    acct_no        BIGINT       NOT NULL,
    dep_type       CHAR(1)      NOT NULL CHECK (dep_type IN ('F','R')),
    status         CHAR(1)      NOT NULL,
    amount         NUMERIC(15,2) NOT NULL,
    rate           NUMERIC(7,4) NOT NULL,
    tenure_months  INTEGER      NOT NULL,
    maturity_date  DATE,
    maturity_value NUMERIC(15,2),
    deposited      NUMERIC(15,2)
);

CREATE TABLE gl_balances (
    gl_code        INTEGER      PRIMARY KEY,
    gl_name        VARCHAR(30)  NOT NULL,
    gl_type        CHAR(1)      NOT NULL,
    balance        NUMERIC(17,2) NOT NULL
);

-- Handy views for analysts
CREATE VIEW branch_deposits AS
    SELECT branch,
           count(*) FILTER (WHERE acct_type = 'S')            AS savings_accts,
           coalesce(sum(balance) FILTER (WHERE acct_type = 'S'), 0) AS savings_bal,
           count(*) FILTER (WHERE acct_type = 'C')            AS current_accts,
           coalesce(sum(balance) FILTER (WHERE acct_type = 'C'), 0) AS current_bal
      FROM accounts
     WHERE status IN ('A','D','F')
     GROUP BY branch;

CREATE VIEW loan_quality AS
    SELECT loan_type,
           count(*)                                       AS loans,
           sum(outstanding)                               AS outstanding,
           coalesce(sum(outstanding) FILTER (WHERE status = 'N'), 0) AS npa_outstanding
      FROM loans
     WHERE status <> 'C'
     GROUP BY loan_type;
