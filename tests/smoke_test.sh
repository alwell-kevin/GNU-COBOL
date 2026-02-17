#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
BIN="${ROOT_DIR}/bin/payview"
TMP_BASE="$(mktemp -d)"
trap 'rm -rf "${TMP_BASE}"' EXIT

run_app() {
  local run_dir="$1"
  local input_data="$2"
  local output_file="$3"
  local rc_file="$4"

  set +e
  (cd "${run_dir}" && printf "%b" "${input_data}" | "${BIN}" > "${output_file}" 2>&1)
  echo "$?" > "${rc_file}"
  set -e
}

prepare_run_dir_with_data() {
  local run_dir="$1"
  mkdir -p "${run_dir}/data"
  cp "${ROOT_DIR}/data/employees.dat" "${run_dir}/data/employees.dat"
  cat > "${run_dir}/data/accounts.dat" <<'EOF'
1001|2500.00|12500.00|0.00
1002|500.00|1800.00|0.00
1003|1200.00|700.00|3500.00
1004|100.00|250.00|0.00
1005|900.00|3000.00|500.00
1006|-20.00|150.00|0.00
EOF
  cat > "${run_dir}/data/ledger.dat" <<'EOF'
1|2026-02-13 09:00:00|1001|DEBIT|CHECKING|100.00|ATM_WITHDRAWAL
1|2026-02-13 09:00:00|1001|CREDIT|CASH|100.00|ATM_WITHDRAWAL
EOF
  : > "${run_dir}/data/alerts.dat"
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  local label="$3"
  local cleaned
  cleaned="$(perl -pe 's/\e\[[0-9;?]*[A-Za-z]//g' "${file}")"
  if ! printf "%s" "${cleaned}" | grep -E -q -- "${pattern}"; then
    echo "FAIL: ${label}"
    echo "Pattern: ${pattern}"
    echo "Output:"
    printf "%s\n" "${cleaned}"
    exit 1
  fi
}

assert_rc() {
  local rc_file="$1"
  local expected="$2"
  local label="$3"
  local actual
  actual="$(cat "${rc_file}")"
  if [[ "${actual}" != "${expected}" ]]; then
    echo "FAIL: ${label}"
    echo "Expected RC ${expected}, got ${actual}"
    exit 1
  fi
}

"${ROOT_DIR}/scripts/build.sh"

# 1) Valid login + pay details
OUT1="${TMP_BASE}/valid.out"
RC1="${TMP_BASE}/valid.rc"
run_app "${ROOT_DIR}" "jsmith\nretail123\n1\n5\n" "${OUT1}" "${RC1}"
assert_rc "${RC1}" "0" "valid login rc"
assert_contains "${OUT1}" "---- Pay Details ----" "pay details shown"
assert_contains "${OUT1}" "Please select an option" "selection prompt"

# 2) Invalid password 3x
OUT2="${TMP_BASE}/invalid.out"
RC2="${TMP_BASE}/invalid.rc"
run_app "${ROOT_DIR}" "jsmith\nbad\njsmith\nbad\njsmith\nbad\n" "${OUT2}" "${RC2}"
assert_rc "${RC2}" "0" "invalid login rc"
assert_contains "${OUT2}" "Maximum login attempts reached\." "login retry limit"

# 3) Bracket boundary: 500.00 -> 10.00%
OUT3="${TMP_BASE}/b500.out"
RC3="${TMP_BASE}/b500.rc"
run_app "${ROOT_DIR}" "amartin\nstore456\n2\n5\n" "${OUT3}" "${RC3}"
assert_rc "${RC3}" "0" "500 rc"
assert_contains "${OUT3}" "Bracket Rate[[:space:]]*: 10\.00%" "500 bracket"

# 4) Bracket boundary: 500.01 -> 15.00%
OUT4="${TMP_BASE}/b50001.out"
RC4="${TMP_BASE}/b50001.rc"
run_app "${ROOT_DIR}" "cgreen\ncash321\n2\n5\n" "${OUT4}" "${RC4}"
assert_rc "${RC4}" "0" "500.01 rc"
assert_contains "${OUT4}" "Bracket Rate[[:space:]]*: 15\.00%" "500.01 bracket"

# 5) Bracket boundary: 1000.00 -> 15.00%
OUT5="${TMP_BASE}/b1000.out"
RC5="${TMP_BASE}/b1000.rc"
run_app "${ROOT_DIR}" "bwhite\nshop789\n2\n5\n" "${OUT5}" "${RC5}"
assert_rc "${RC5}" "0" "1000 rc"
assert_contains "${OUT5}" "Bracket Rate[[:space:]]*: 15\.00%" "1000 bracket"

# 6) Bracket boundary: 1000.01+ -> 22.00%
OUT6="${TMP_BASE}/b100001.out"
RC6="${TMP_BASE}/b100001.rc"
run_app "${ROOT_DIR}" "dblack\nfloor654\n2\n5\n" "${OUT6}" "${RC6}"
assert_rc "${RC6}" "0" "1000.01 rc"
assert_contains "${OUT6}" "Bracket Rate[[:space:]]*: 22\.00%" "1000.01 bracket"

# 7) Zero hours: no divide by zero; effective rate 0.00
OUT7="${TMP_BASE}/zero.out"
RC7="${TMP_BASE}/zero.rc"
run_app "${ROOT_DIR}" "ezero\nnone000\n2\n5\n" "${OUT7}" "${RC7}"
assert_rc "${RC7}" "0" "zero hours rc"
assert_contains "${OUT7}" "Effective Tax Rate[[:space:]]*:[[:space:]]*0\.00%" "zero hours effective rate"

# 8) Missing data file -> startup error rc=1
MISS_DIR="${TMP_BASE}/missing"
mkdir -p "${MISS_DIR}"
OUT8="${TMP_BASE}/missing.out"
RC8="${TMP_BASE}/missing.rc"
run_app "${MISS_DIR}" "" "${OUT8}" "${RC8}"
assert_rc "${RC8}" "1" "missing data rc"
assert_contains "${OUT8}" "ERROR: Cannot read employee data file" "missing data error"

# 9) Malformed row -> auth data error rc=1
BAD_DIR="${TMP_BASE}/bad"
mkdir -p "${BAD_DIR}/data"
cp "${ROOT_DIR}/data/employees.dat" "${BAD_DIR}/data/employees.dat"
echo "bad|line" >> "${BAD_DIR}/data/employees.dat"
OUT9="${TMP_BASE}/bad.out"
RC9="${TMP_BASE}/bad.rc"
run_app "${BAD_DIR}" "nouser\nnope\n" "${OUT9}" "${RC9}"
assert_rc "${RC9}" "1" "malformed data rc"
assert_contains "${OUT9}" "ERROR: Malformed employee row" "malformed row error"

# 10) ASCII chart output renders bars
OUT10="${TMP_BASE}/chart.out"
RC10="${TMP_BASE}/chart.rc"
run_app "${ROOT_DIR}" "jsmith\nretail123\n3\n5\n" "${OUT10}" "${RC10}"
assert_rc "${RC10}" "0" "chart rc"
assert_contains "${OUT10}" "---- Payroll Chart ----" "chart header"
assert_contains "${OUT10}" "Gross \\|#+[[:space:]]*\\|" "chart gross bar"
assert_contains "${OUT10}" "Tax[[:space:]]+\\|#+[[:space:]]*\\|" "chart tax bar"
assert_contains "${OUT10}" "Net[[:space:]]+\\|#+[[:space:]]*\\|" "chart net bar"

# 11) Chained selections from detail screens
OUT11="${TMP_BASE}/chained.out"
RC11="${TMP_BASE}/chained.rc"
run_app "${ROOT_DIR}" "jsmith\nretail123\n1\n2\n3\n5\n" "${OUT11}" "${RC11}"
assert_rc "${RC11}" "0" "chained selections rc"
assert_contains "${OUT11}" "---- Pay Details ----" "chained pay details"
assert_contains "${OUT11}" "---- Tax Breakdown ----" "chained tax breakdown"
assert_contains "${OUT11}" "---- Payroll Chart ----" "chained payroll chart"

# 12) Banking transfer + ledger + audit
BANK_DIR_1="${TMP_BASE}/bank1"
prepare_run_dir_with_data "${BANK_DIR_1}"
OUT12="${TMP_BASE}/bank-transfer.out"
RC12="${TMP_BASE}/bank-transfer.rc"
run_app "${BANK_DIR_1}" "jsmith\nretail123\n6\n2\n250.00\n\n1\n\n4\n\n6\n\n7\n5\n" "${OUT12}" "${RC12}"
assert_rc "${RC12}" "0" "bank transfer rc"
assert_contains "${OUT12}" "Helical Community Bank Console" "bank menu shown"
assert_contains "${OUT12}" "Transfer posted and journaled\\." "transfer journaled"
assert_contains "${OUT12}" "TRANSFER_TO_SAVINGS" "transfer ledger memo"
assert_contains "${OUT12}" "Imbalanced txns[[:space:]]*: 0" "ledger audit balanced"

# 13) End-of-day batch applies interest and overdraft fee
BANK_DIR_2="${TMP_BASE}/bank2"
prepare_run_dir_with_data "${BANK_DIR_2}"
OUT13="${TMP_BASE}/bank-eod.out"
RC13="${TMP_BASE}/bank-eod.rc"
run_app "${BANK_DIR_2}" "ezero\nnone000\n6\n5\n\n6\n\n7\n5\n" "${OUT13}" "${RC13}"
assert_rc "${RC13}" "0" "bank eod rc"
assert_contains "${OUT13}" "EOD batch completed\\." "eod completed"
assert_contains "${OUT13}" 'Interest Posted[[:space:]]*:[[:space:]]*\$[[:space:]]*0\.15' "interest posted"
assert_contains "${OUT13}" 'Overdraft Fee[[:space:]]*:[[:space:]]*\$[[:space:]]*35\.00' "overdraft fee posted"
assert_contains "${OUT13}" "Imbalanced txns[[:space:]]*: 0" "eod audit balanced"

echo "Smoke tests passed."
