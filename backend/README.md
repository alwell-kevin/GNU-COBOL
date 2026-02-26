# COBOL Demo Backend Foundation

This module hosts the Spring Boot foundation for the COBOL-Demo migration. It wires configuration, logging, and persistence scaffolding so the upcoming auth, payroll, banking, ledger, and end-of-day services can plug in without reimplementing the flat-file contracts.

## Current scaffolding
- `CobolDemoApplication` boots the Spring context, enables `FlatFileProperties`, and exposes the shared `FlatFilePersistenceAdapter`. This adapter is the single integration point for every migration domain.
- `config.AppConfig` loads environment-driven terminal defaults (`PAYVIEW_TERM_ROWS/COLS`) and the UTC clock used for timestamping. `LoggingConfig` registers a filter that seeds MDC with `transactionId`/`transactionTimestamp` for structured logs.
- `FlatFileProperties` declares the sequential data paths, encoding, and rounding mode so the adapter can honor COBOL `PIC`/`ROUNDED` semantics out of the box.

## Persistence interfaces
- `EmployeeStore` surfaces employee lookups for `POST /api/auth/login` and payroll views.
- `AccountStore` supplies account balances for `/api/banking/{id}/summary` and transfer handlers.
- `LedgerStore` feeds `/api/banking/{id}/ledger/recent`, `/api/banking/{id}/audit`, and double-entry posting helpers.
- `AlertStore` backs `/api/banking/{id}/alerts` and end-of-day monitoring.

`FlatFilePersistenceAdapter` currently implements all of the above by reading the `data/*.dat` fixtures, normalizing numeric scales, and logging every read with transaction context.

## Next migration steps
1. **Auth service** – wire `EmployeeStore` data into a credentials validator that mirrors `AUTHENTICATE-EMPLOYEE`. Preserve plain-text lookups and normalize field widths before returning DTOs.
2. **Payroll domain** – reuse `EmployeeStore` data to drive payroll calculations, rounding bracketed tax the same way as `CALCULATE-PAYROLL`, and feed chart data to the Next.js `/payroll/chart` UI.
3. **Banking/ledger** – build services that call `AccountStore`/`LedgerStore` to post transfers, ensuring every ledger entry writes matching debit/credit rows and increments the transaction counter parsed from `ledger.dat`.
4. **EOD batch & alerts** – produce ledger audits and `AlertRecord` writes via `AlertStore`, emitting structured logs so BigQuery or downstream monitoring can track numeric precision differences.

## Observability notes
- Structured logging lives in `LoggingConfig.TransactionLoggingFilter`. Future controllers or services should keep the MDC keys `transactionId` and `transactionTimestamp` so logs can be correlated across REST and persistence calls.
- Numeric rounding uses the configured `scale` and `rounding-mode` from `application.yml`. If tighter precision is needed for new services, update the property and add instrumentation before changes reach payroll or ledger flows.
