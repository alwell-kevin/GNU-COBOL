#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

cat > "${ROOT_DIR}/data/accounts.dat" <<'DATA'
1001|2500.00|12500.00|0.00
1002|500.00|1800.00|0.00
1003|1200.00|700.00|3500.00
1004|100.00|250.00|0.00
1005|900.00|3000.00|500.00
1006|-20.00|150.00|0.00
DATA

cat > "${ROOT_DIR}/data/ledger.dat" <<'DATA'
1|2026-02-13 09:00:00|1001|DEBIT|CHECKING|100.00|ATM_WITHDRAWAL
1|2026-02-13 09:00:00|1001|CREDIT|CASH|100.00|ATM_WITHDRAWAL
DATA

: > "${ROOT_DIR}/data/alerts.dat"

echo "Reset demo banking data in ${ROOT_DIR}/data"
