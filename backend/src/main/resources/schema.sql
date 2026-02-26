-- Schema definition that mirrors the legacy flat-file layouts described in copybooks.
-- Column widths and decimal precision follow PIC definitions (V99 => scale 2).

CREATE TABLE employees (
  employee_id VARCHAR(4) PRIMARY KEY,
  username VARCHAR(20) NOT NULL,
  password VARCHAR(20) NOT NULL,
  full_name VARCHAR(40) NOT NULL,
  hourly_rate DECIMAL(5,2) NOT NULL,
  hours_current_period DECIMAL(5,2) NOT NULL,
  ytd_wages DECIMAL(9,2) NOT NULL
);

CREATE TABLE accounts (
  employee_id VARCHAR(4) PRIMARY KEY,
  checking_balance DECIMAL(9,2) NOT NULL,
  savings_balance DECIMAL(9,2) NOT NULL,
  loan_balance DECIMAL(9,2) NOT NULL,
  last_updated TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (employee_id) REFERENCES employees(employee_id)
);

CREATE TABLE ledger (
  tx_id BIGINT NOT NULL,
  timestamp TIMESTAMP NOT NULL,
  employee_id VARCHAR(4) NOT NULL,
  entry_type CHAR(6) NOT NULL,
  account VARCHAR(10) NOT NULL,
  amount DECIMAL(9,2) NOT NULL,
  memo VARCHAR(32) NOT NULL,
  PRIMARY KEY (tx_id, entry_type, account)
);

CREATE TABLE alerts (
  alert_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  employee_id VARCHAR(4) NULL,
  severity VARCHAR(16) NOT NULL,
  message VARCHAR(128) NOT NULL,
  raised_at TIMESTAMP NOT NULL,
  FOREIGN KEY (employee_id) REFERENCES employees(employee_id)
);

-- Ledger transaction ids are sequential (COBOL wrote zero-padded numbers). Use a sequence if a relational backend is implemented.
-- The timestamp format must continue to be yyyy-MM-dd HH:mm:ss so Java serializers/parsers stay aligned with the files/DTOs.
