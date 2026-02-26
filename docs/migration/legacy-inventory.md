# Legacy Inventory for `legacy_inventory_74a6d2b1`

## 1. Entrypoint & Control Flow Survey

### `src/main.cob`
- Loads the ANSI styling palette, terminal dimensions (`PAYVIEW_TERM_ROWS`/`COLS` from `scripts/run.sh`), then loops between authentication (`CALL "AUTHENTICATE-EMPLOYEE"`) and the payroll/banking menu (`MENU-LOOP` → `HANDLE-MENU-OPTION`).
- Tracks exit flags (`WS-EXIT-APP`, `WS-EXIT-MENU`, `WS-HAVE-PENDING-OPTION`) to sequence chained views (e.g., returning from payroll details to main menu) and to guard against partial selections.
- Maintains display formatting and pacing using shared working-storage (left/top padding, ANSI sequences) that drives Clear/Home/Color semantics for every screen transition.

### `src/auth.cob`
- `DRAW-SIGNON-SCREEN` renders the login frame, then `FIND-EMPLOYEE-ROW` reads `data/employees.dat` line-by-line (3-attempt limit) while normalizing username/password, enforcing plaintext comparison, and returning statuses: success (`Y`), termination/EOF (`E`), or failure (`N`).
- UI cues (ANSI brackets, left padding) are tightly coupled to `PREPARE-CENTER-BLOCK`, so a future Next.js login page must preserve the prompts `User ID`, `Password`, `ENTER=Sign-on`, `F3=Exit` to keep the observable contract.
- Invalid row triggers (`ERROR: Malformed employee row`) bubble back with exit code `1`, mirroring the smoke tests that cover malformed input handling.

### `src/payroll.cob` and `src/chart.cob`
- `CALCULATE-PAYROLL` uses `copybooks/payroll_calc.cpy` to compute gross, fee, net, effective tax rate with `ROUNDED` arithmetic and bracket selection (≤500→10%, ≤1000→15%, otherwise 22%).
- `RENDER-PAYROLL-CHART` normalizes bar lengths to the largest value, enforces a minimum length of `1` for non-zero amounts, and clamps `leftPad` to 200 spaces while prefixing each printed line with `Scale: ...` and the colored delimiters from the ANSI palette.
- The ASCII chart output, emphasized by `tests/smoke_test.sh`, ensures bars are `#` sequences; the normalization logic (clamping, skip if max=0) defines how the future Next.js component should scale bars even when net/tax/gross differ significantly.

### `src/banking.cob`
- Entry begins with `LOAD-ACCOUNT-TABLE` (line-sequential `data/accounts.dat` into an OCCURS table) and `LOCATE-CURRENT-ACCOUNT` to tie the authenticated employee record to the account row; missing rows raise immediate errors.
- `DISPLAY-BANKING-MENU` and `HANDLE-BANKING-OPTION` drive banking flows: `TRANSFER-CHECKING-TO-SAVINGS`, `POST-DOUBLE-ENTRY`, `RUN-LEDGER-AUDIT`, `RUN-END-OF-DAY-BATCH`, along with summary outputs for `Checking`, `Savings`, `Loan` balances and ledger/alert counts.
- Double-entry posting stores mirrored debit/credit rows (same `tx_id`, timestamp, memo) into `data/ledger.dat`, and `RUN-LEDGER-AUDIT` tallies imbalanced transactions (`WS-IMBALANCED-COUNT`), which smoke tests assert must stay at zero after transfers + EOD.
- End-of-day batch (`RUN-END-OF-DAY-BATCH`) applies daily interest (`WS-DAILY-INTEREST-RATE`) to savings and the overdraft fee (`WS-OVERDRAFT-FEE`) to checking, then journales both actions with the ledger and alerts (`data/alerts.dat`).

## 2. Data Contract Definitions

| Data asset | Source | COBOL layout | Notes |
|------------|--------|--------------|-------|
| `employees.dat` | `copybooks/employee_record.cpy` | `EMP-ID 9(4)`, `EMP-USERNAME`/`PASSWORD`/`FULL-NAME` lengths, `EMP-HOURLY-RATE 9(3)V99`, `EMP-HOURS-PERIOD 9(3)V99`, `EMP-YTD-WAGES 9(7)V99` |
| `accounts.dat` | implicit table in `src/banking.cob` | `EMP-ID 9(4)`, `CHECKING/SAVINGS/LOAN S9(7)V99` (signed; leading zeros) - sequential lines config `WS-ACCOUNT-TABLE` |
| `ledger.dat` | `POST-DOUBLE-ENTRY` & `RUN-LEDGER-AUDIT` | `tx_id|timestamp|employee_id|entry_type|account|amount|memo`; record order is chronological but accounting integrity relies on paired DEBIT/CREDIT with same `tx_id`. |
| `alerts.dat` | `RUN-END-OF-DAY-BATCH` (and risk logging) | Stored as text lines; currently empty but reserved for risk/compliance events triggered when imbalances occur or overdrafts happen. |

- `scripts/reset_demo_data.sh` rewrites `accounts.dat`, `ledger.dat`, and zeroes `alerts.dat`, defining canonical demo states (250.00 transfer already in ledger, also used by smoke test cases). The migration must replicate this seed for tests.
- `scripts/run.sh` and `scripts/resize_terminal.sh` set `PAYVIEW_TERM_ROWS/COLS` before launching the COBOL binary; preserving those env vars ensures UI padding/calibrations in Spring Boot (for API metadata) and Next.js (for layout hints).
- `tests/smoke_test.sh` copies those data files into temp directories to execute canonical flows and asserts: login, bracket boundaries, chart output, ledger balancing, EOD effects, error handling (missing file, malformed row). These are the observable contracts the migration must replicate in the new APIs/tests.

## 3. Parity-sensitive Behaviors

- **Authentication**: Plaintext username/password match, 3-attempt limit, success status (`Y`) vs. error states (`E` for disk issues) with exit codes 0/1 and user-facing ANSI prompts. Next.js `/login` must persist prompt content and failure states, while Spring Boot must return precise HTTP status codes (401/500) and payloads to mirror the COBOL screen text.
- **Payroll calculations**: Multiply hourly rate × hours (`ROUNDED`), choose brackets (≤500 => 10%, ≤1000 => 15%, >1000 => 22%), round every computed amount to two decimals (COBOL `ROUNDED` behavior). Effective tax rate is `(tax / gross) * 100` with divide-by-zero guard resulting in 0.00. Java must use `BigDecimal` with `HALF_UP` and dot-two scaling; Next.js components should format the percentages/amounts identically to the ANSI view so `/payroll/details`, `/payroll/tax`, `/payroll/chart` show the same values as the terminal screens.
- **Chart rendering**: Bars are 1–40 `#` characters, scaled to the largest of gross/tax/net, with left padding capped at 200 spaces and zero handling that omits `#` (but still includes the scale note). The Next.js `/payroll/chart` page must reuse an equivalent normalization and textual representation (the existing `frontend/lib/chart.js` helper plus Jest tests show the desired behavior).
- **Banking flows**: Transfers trigger double-entry posting, ensuring equal debit and credit totals per `tx_id`. Ledger audit counts imbalances (`WS-IMBALANCED-COUNT`) and reports them to the screen. End-of-day applies daily interest and overdraft fees, journaling each post. Sequence and rounding (signed `S9(7)V99`) must be maintained in the Spring Boot service layer when adjusting account balances.
- **UI/CLI parity cues**: The ANSI palette (bold, colors, keywords) frames each screen (`LOGIN`, `Payroll details`, `Banking console`, etc.). New Next.js pages must carry similar headings, sanitized strings (e.g., `---- Pay Details ----`) and prompts to keep demos consistent.

## 4. Mapping to Spring Boot + Next.js Targets

| COBOL Component | Proposed Spring Boot boundary | REST/API contract | Next.js surface | Notes & parity points |
|-----------------|-------------------------------|------------------|------------------|-----------------------|
| `AUTHENTICATE-EMPLOYEE` (`src/auth.cob`: 3-attempt login + error states) | `AuthController` → `AuthService.authenticate(username,password)` | `POST /api/auth/login` returns `{employeeId, fullName, token}` or `401`/`500`, reusing legacy plaintext token for now. | `/login` page & `/api/auth/login` Next.js API wrapper | Preserve `User ID`, `Password`, `ENTER=Sign-on`, prompt/hint text. Track failure counts for UI feedback. |
| `PAYROLL` screens (`src/main.cob` options `1-3`) | `PayrollController` → `PayrollService.calculate(id)` → uses `EmployeeRepository` | `GET /api/payroll/{id}/details`, `GET /api/payroll/{id}/tax`, `GET /api/payroll/{id}/chart` return DTOs with gross/tax/net/effective rate and bar lengths normalized to max value. | `/payroll`, `/payroll/details`, `/payroll/tax`, `/payroll/chart` pages/components | DTO fields align with `copybooks/payroll_calc`, rounding ensures `HALF_UP` scale 2, chart component reuses `frontend/lib/chart.js` logic for normalization/padding. |
| `CHART` (`src/chart.cob`) | `PayrollChartFacade` (service layer prepping lengths plus amounts) | `GET /api/payroll/{id}/chart` payload includes `gross`, `tax`, `net`, `maxValue`, `barLengths`, `leftPadding` metadata. | Visualization component under `/payroll/chart` that renders same scale message and `#` bars for parity. |
| `BANKING` summary (`src/banking.cob: DISPLAY-BANKING-MENU`) | `BankingController` + `AccountService` reading `accounts.dat` equivalents | `GET /api/banking/{id}/summary` returns checking/savings/loan, ledger status, imbalance count, alerts. | `/banking` page replicating menu headings, indicates ledger/alert statuses. |
| `TRANSFER` (`TRANSFER-CHECKING-TO-SAVINGS`, `POST-DOUBLE-ENTRY`) | `TransactionService` ensuring ledger balancing and sequential `tx_id`/timestamp generation | `POST /api/banking/{id}/transfer` accepts `{fromAccount, toAccount, amount}`; response echoes updated balances and ledger entries. | `/banking/transfer` page + form, shows transfer confirmation block and ledger memo text `TRANSFER_TO_SAVINGS`. |
| `LEDGER` display (`RECENT-BUFFER`, `RUN-LEDGER-AUDIT`) | `LedgerService` reading ledger snapshot and audit counts | `GET /api/banking/{id}/ledger/recent`, `GET /api/banking/{id}/audit` respond with last N entries and imbalance count. | `/banking/ledger`, `/banking/audit` pages with table of entries, audit status message `Imbalanced txns : 0`. |
| `END-OF-DAY` batch (`RUN-END-OF-DAY-BATCH`) | `BatchService` → atomic adjustments to savings + checking, ledger/alerts | `POST /api/banking/{id}/eod` triggers interest/fee postings and returns impacted accounts, ledger entries, alerts. | `/banking/eod` page reuses same success text (`EOD batch completed.`) and reports interest and overdraft postings. |
| Alert logging (`alerts.dat`) | `AlertService` writes risk events when ledger imbalances or overdrafts occur. | `GET /api/banking/{id}/alerts` returns list of alerts derived from CSV. | `/banking/audit` or `/banking/alerts` UI element shows latest alert lines. |
| Flat-file persistence (`data/*.dat`) | `FileAdapter` layer reading/w writing `employees`, `accounts`, `ledger`, `alerts` with strict delimiting and padded decimals. | Use DTOs that mirror the PIC widths to validate lengths/rounding before persisting. | N/A (Spring-managed) | Keep rounding/regrouping logic consistent with COBOL `PIC` definitions. |

- Each API should model the COBOL status flags (OK vs errors) as HTTP status codes with a payload describing the same textual messages (e.g., `ERROR: Cannot read employee data file`).
- Data adapter must enforce sequential `tx_id` generation (increment `WS-TX-ID`/`WS-MAX-TX-ID`) and timestamp formatting `YYYY-MM-DD HH:MM:SS` per `data/ledger.dat`.
- UI should keep the retro palette strings for headings, using Next.js components that render the same `---- ... ----` banners highlighted in the COBOL screens.

## 5. Risks & Validation Checkpoints

1. **Numeric rounding/precision**: COBOL PIC definitions (e.g., `S9(7)V99`) imply `BigDecimal` with scale 2 and `HALF_UP`. Java/JavaScript defaults differ; regression tests should compare computed values to `1800.00` gross, `396.00` tax, etc., as already captured by `backend/src/test/java/com/cobol/demo/LegacyBehaviorTests.java` and `frontend/lib/chart.js` + Jest.
2. **Ledger double-entry integrity**: Each transfer/EOD action writes paired DEBIT/CREDIT entries with matching `tx_id`. Migration tests must assert debit equals credit and `Imbalanced txns` resets to zero (mirrors `tests/smoke_test.sh` checks 12–13). Guardian service wrappers should abort and log to alerts if mismatch is detected.
3. **Token/Transaction sequencing**: COBOL maintains `WS-TX-ID`/`WS-MAX-TX-ID` as integer counters. Spring Boot must persist and increment these counters deterministically when writing to `ledger.dat` or the chosen persistence store to avoid reused IDs across sessions.
4. **UI parity**: Chart bars, banking menu prompts, and login hints have specific textual/ASCII cues (banners, `Scale: ...`, `Imbalanced txns : 0`). Next.js pages must render identical strings/font weight to keep the retro demo feel.
5. **Operational scripts**: `scripts/run.sh` injects terminal size into environment variables; documentation for Spring Boot/Next.js should note that UI layout logic currently depends on `PAYVIEW_TERM_ROWS/COLS` and that similar viewport metadata may be needed when rendering front-end shells (especially if migrating to server-rendered pages). Document `scripts/reset_demo_data.sh` data seeds for reference in future tests.

## 6. References & Next Steps
- COBOL sources: `src/main.cob`, `src/auth.cob`, `src/payroll.cob`, `src/chart.cob`, `src/banking.cob` (see listed paragraphs for each flow).
- Copybooks/data: `copybooks/employee_record.cpy`, `copybooks/payroll_calc.cpy`, `data/*.dat`.
- Shell flows: `scripts/run.sh`, `scripts/reset_demo_data.sh`, `tests/smoke_test.sh`.
- Regression safety nets: `backend/src/test/java/com/cobol/demo/LegacyBehaviorTests.java` (Java rounding/ledger expectations) and `frontend/{lib/chart.js,__tests__/chartRendering.test.js}` (chart scaling). These tests define the contract the migration must satisfy before swapping in Spring Boot + Next.js components.

Post-migration, validate each REST endpoint against the documented COBOL behaviors by replaying the smoke-test scenarios (login thresholds, bracket boundaries, chart output, ledger audit) and ensure the reporting strings and ledger balances match the legacy flat-file snapshots.
