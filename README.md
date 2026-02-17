# Retro COBOL Payroll + Banking Demo

A terminal-based GnuCOBOL app inspired by 90s/early-2000s retail systems. It now combines payroll screens with a compact banking workflow that demonstrates transaction posting, ledger balancing, and end-of-day processing.

## Features

- Username/password login (demo-only plaintext)
- Payroll views:
  - Pay details (gross/tax/net)
  - Tax breakdown by bracket
  - ASCII payroll chart
- Banking console:
  - Account summary (checking, savings, loan)
  - Transfers between checking and savings
  - Double-entry ledger posting (debit + credit records per transaction)
  - Ledger balance audit (`Imbalanced txns`)
  - End-of-day batch run (savings interest + overdraft fee)

## Requirements

- macOS with Homebrew
- GnuCOBOL (`cobc`)

Install GnuCOBOL:

```bash
brew install gnu-cobol
cobc -V
```

## Quick Start

From the repo root:

```bash
bash scripts/reset_demo_data.sh
bash scripts/run.sh
```

`run.sh` builds and launches the app.

Demo logins:

- `jsmith` / `retail123`
- `ezero` / `none000`

## 10-Minute Demo Script

Use this exact flow for a clean, repeatable demo.

1. Start app:

```bash
bash scripts/reset_demo_data.sh
bash scripts/run.sh --no-resize
```

2. Log in as `jsmith` / `retail123`.
3. In main menu, press `1` (Pay details), then `2` (Tax breakdown), then `3` (ASCII chart).
4. Press `6` to open the Banking Console.
5. Press `1` (Account summary).
6. Press `2`, transfer `250.00` from checking to savings, then press Enter when prompted.
7. Press `4` (Recent ledger entries). You should see paired debit/credit rows for the transfer.
8. Press `6` (Ledger balance audit). Confirm `Imbalanced txns      : 0`.

## Code Map (for walkthrough)

- `src/main.cob`: app shell, main menu, dispatches payroll and banking flows.
  - Key paragraphs: `DISPLAY-MENU-PROMPT`, `HANDLE-MENU-OPTION`
- `src/auth.cob`: login screen, employee file parsing, credential validation. `CHECK-CREDENTIALS.`
- `src/payroll.cob`: gross/tax/net calculation logic.
- `src/chart.cob`: ASCII chart rendering.
- `src/banking.cob`: banking subsystem.
  - Key paragraphs: `LOAD-ACCOUNT-TABLE`, `TRANSFER-CHECKING-TO-SAVINGS`, `TRANSFER-SAVINGS-TO-CHECKING`, `POST-DOUBLE-ENTRY`, `RUN-LEDGER-AUDIT`, `RUN-END-OF-DAY-BATCH`

## Data Files

- `data/employees.dat`

```text
employee_id|username|password|full_name|hourly_rate|hours_current_period|ytd_wages
```

- `data/accounts.dat`

```text
employee_id|checking_balance|savings_balance|loan_balance
```

- `data/ledger.dat`

```text
tx_id|timestamp|employee_id|entry_type|account|amount|memo
```

- `data/alerts.dat` (reserved for risk/compliance event logging)

## Build and Test

From the repo root:

```bash
bash scripts/build.sh
bash tests/smoke_test.sh
```

## Notes and Limitations

- Plaintext passwords are intentional for retro demo authenticity.
- Tax and banking calculations are illustrative, not legal or financial guidance.
- Single-user terminal app; no concurrent writers.
