# Data Model and Service Contracts

This document captures the COBOL copybook layouts, flat-file behaviors, and the precise Spring Boot/Next.js DTOs that must preserve all observable (API + UI) contracts for the migration.

## Copybook and COBOL Layouts
| Copybook | Field | PIC | Interpretation |
| --- | --- | --- | --- |
| `copybooks/employee_record.cpy` | `EMP-ID` | `9(4)` | 4-digit employee identifier (string padded with leading zeros when written to disk). |
| | `EMP-USERNAME` | `X(20)` | Case-insensitive login handle, trimmed when presented. |
| | `EMP-PASSWORD` | `X(20)` | Plaintext credential (legacy). |
| | `EMP-FULL-NAME` | `X(40)` | Display name with trailing spaces stripped. |
| | `EMP-HOURLY-RATE` | `9(3)V99` | Decimal with scale 2, `BigDecimal` precision 5:2, rounding `HALF-EVEN`. |
| | `EMP-HOURS-PERIOD` | `9(3)V99` | Period hours (scale 2). |
| | `EMP-YTD-WAGES` | `9(7)V99` | Year-to-date earnings (scale 2, max 9 digits integer part). |
| `copybooks/payroll_calc.cpy` | `WS-GROSS-PAY` | `9(7)V99` | Derived gross pay (scale 2). |
| | `WS-TAX-AMOUNT` | `9(7)V99` | Total tax withheld. |
| | `WS-NET-PAY` | `9(7)V99` | Net pay after deductions. |
| | `WS-EFFECTIVE-TAX-RATE` | `9(3)V99` | Effective rate expressed as percent (e.g., `12.50`). |

## Line-sequential Data Files
1. `data/employees.dat`: pipe-delimited records `<employee_id>|<username>|<password>|<full_name>|<hourly_rate>|<hours_current_period>|<ytd_wages>`; numeric fields include leading zeros/ensured scale matching copybook PICs.
2. `data/accounts.dat`: `<employee_id>|<checking_balance>|<savings_balance>|<loan_balance>`; balances formatted with leading zeros, optional negative sign, scale 2.
3. `data/ledger.dat`: `<tx_id>|<timestamp>|<employee_id>|<entry_type>|<account>|<amount>|<memo>`; `tx_id` padded with zeros, `entry_type` is `DEBIT`/`CREDIT`, `timestamp` uses `yyyy-MM-dd HH:mm:ss`, `memo` describes action.
4. `data/alerts.dat`: empty placeholder (intentional empty scenario for alert APIs).

## Numeric Precision + Rounding Rules
- Legacy `PIC` and `ROUNDED` semantics translate to `BigDecimal` values with `scale = 2` and `RoundingMode.HALF_EVEN` (`backend/src/main/java/com/coboldemo/model/NumericContract.java` centralizes these constants).
- All monetary fields (hourly rate, balances, ledger amounts) and derived payroll outputs must be rounded on instantiation to avoid drift. Hours and percentage fields reuse the same scale to match `V99` behavior.
- Ledger transaction IDs are sequence numbers that may be stored with padded zeros; persistence should keep `BIGINT` for numeric comparison but serialization should render zero-padding when the UI requires it (see Next.js `/banking/ledger` rendering expectations).

## Spring Boot Domain Models (bridge to copybooks)
- `Employee` (`backend/src/main/java/com/coboldemo/model/Employee.java`): mirrors each `EMP-*` field, enforces 2-decimal scaling, documents copybook PICs for developer reference.
- `PayrollCalculation` (`.../PayrollCalculation.java`): caches gross/tax/net/effective rate derived fields with enforced precision matching `payroll_calc.cpy`.
- `BankingAccount` (`.../BankingAccount.java`): represents checking/savings/loan balances, includes helper for double-entry sanity checks.
- `LedgerTransaction` (`.../LedgerTransaction.java`): captures COBOL ledger columns plus `EntryType` enum (`DEBIT`/`CREDIT`) and parsed `Instant` for `timestamp`.
- `AlertEvent` (`.../AlertEvent.java`): placeholder domain object for risk/alert notifications; keeps `String message`/`severity` metadata even when `alerts.dat` is empty.

## Structured DTO Contracts (align to REST API surface)
| Endpoint | DTO | Fields | Notes |
| --- | --- | --- | --- |
| `POST /api/auth/login` | `LoginRequestDto` / `LoginResponseDto` | `{ username, password }` request; response returns `{ employeeId, fullName, token, message }` to keep text contracts simple for Next.js login screens. |
| `GET /api/employees/{id}` | `EmployeeDetailDto` | `{ employeeId, username, fullName, hourlyRate, hoursCurrentPeriod, ytdGross }` with BigDecimal scale 2 and string fields trimmed. |
| `GET /api/payroll/{id}/details` | `PayrollDetailsDto` | `{ employeeId, hourlyRate, hoursWorked, grossPay, taxAmount, netPay, effectiveTaxRate }` (all monetary/percentage fields use `NumericContract.scale`). |
| `GET /api/payroll/{id}/tax` | `PayrollTaxDto` | `{ employeeId, totalTax, brackets: List<TaxBracketDto> }` where each bracket includes `{ name, ratePercent, taxAmount }`; ensures UI can show breakdown identical to COBOL screen. |
| `GET /api/payroll/{id}/chart` | `PayrollChartDto` | `{ employeeId, normalizedTo, dataPoints: List<PayrollChartPointDto> }` so ASCII chart replication can rely on the same point sequence and normalized denominator. |
| `GET /api/banking/{id}/summary` | `BankingSummaryDto` | `{ employeeId, checkingBalance, savingsBalance, loanBalance, lastUpdated }` (balances scaled, lastUpdated iso timestamp). |
| `POST /api/banking/{id}/transfer` | `BankingTransferRequestDto` / `BankingTransferResponseDto` | Request carries `{ fromAccount, toAccount, amount, memo }`; response echoes `{ transactionId, postedAt, balances: BankingBalancesDto }` so UI can show new balances. |
| `GET /api/banking/{id}/ledger/recent` | `LedgerEntryDto` | returns most recent `List<LedgerEntryDto>` matching double-entry semantics (two rows per tx, `type`/`account` preserved). Timestamp uses `yyyy-MM-dd HH:mm:ss` pattern via `@JsonFormat`. |
| `GET /api/banking/{id}/audit` | `LedgerAuditDto` | `{ employeeId, imbalanceCount, lastCheckedAt, delta }` to track `Imbalanced txns` output. |
| `POST /api/banking/{id}/eod` | `EodResultDto` | `{ employeeId, processedAt, interestApplied, feesApplied, ledgerEntriesPosted }` describing the batch run. |
| `GET /api/banking/{id}/alerts` | `AlertDto` | simple `[ { employeeId, severity, message, raisedAt } ]` list; empty `alerts.dat` remains valid state.

Each DTO class lies under `backend/src/main/java/com/coboldemo/dto` and leverages `NumericContract.enforceScale` to maintain rounding consistency.

## Persistence Intent (`backend/src/main/resources/schema.sql`)
- Table `employees` mirrors `EMP-*` fields with `VARCHAR`/`DECIMAL` column types that respect PIC widths (e.g., `DECIMAL(7,2)` for `ytd_wages`).
- Table `accounts` keeps numeric precision while allowing signed checking balances to represent overdrafts.
- Table `ledger` stores every double-entry line with `tx_id BIGINT`, `entry_type CHAR(6)`, zero-padded display logic handled by the presentation layer, and `amount DECIMAL(9,2)` for `PIC 9(7)V99` numbers.
- `alerts` table is intentionally empty; schema includes `severity`/`message` columns so incoming alerts can be recorded once produced.
- `schema.sql` also documents that `tx_id` is a sequential key (simulating COBOL's write order) and that `timestamp` follows `yyyy-MM-dd HH:mm:ss` so Java `DateTimeFormatter` matches legacy formatting.

## Configuration + Environment Hooks
- `backend/src/main/resources/application.yml` exposes file paths, encoding, scale, and rounding mode under `cobol.data` so the flat-file adapter can parse and write records with the same defaults as COBOL (scale 2, `HALF_EVEN`).
- Terminal dimensions (`PAYVIEW_TERM_COLS`/`PAYVIEW_TERM_ROWS`) remain available via the `payview.term` namespace. These values inform UI layout emulation for Next.js (the chart width is still capped via `payview.chart.max-width`).
- Future serialization helpers can read placeholders such as `payview.currency` or `cobol.serialization.precision` to keep UI charts and landing telemetry aligned with COBOL formatting; mention of these hooks appears directly in `application.yml` so downstream teams know where to override.

## Next Steps for Service/UI Implementers
1. Implement Spring services that hydrate the domain objects above from `FlatFilePersistenceAdapter` or the future DB layer; keep `NumericContract` constants in sync with the copybooks.
2. Ensure each REST controller produces/consumes the DTO contracts defined in `backend/src/main/java/com/coboldemo/dto` to preserve payload shape.
3. Use `NumericContract.enforceScale` when calculating payroll taxes, ledger postings, and transfer amounts so rounding parity is maintained.
4. Keep UI components (Next.js pages under `/payroll` and `/banking`) referencing the DTO shapes above; tie `LedgerEntryDto.memo` and `AlertDto` contents directly to COBOL memo fields to maintain observability.

Pending questions noted in the downstream backlog should specify any additional tax bracket data or ledger sequence rules that are not explicitly captured in the flat files above.
