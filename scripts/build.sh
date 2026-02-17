#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

mkdir -p "${ROOT_DIR}/bin"

cobc -x -free -Wall -I "${ROOT_DIR}/copybooks" \
  -o "${ROOT_DIR}/bin/payview" \
  "${ROOT_DIR}/src/main.cob" \
  "${ROOT_DIR}/src/banking.cob" \
  "${ROOT_DIR}/src/auth.cob" \
  "${ROOT_DIR}/src/payroll.cob" \
  "${ROOT_DIR}/src/chart.cob"

echo "Built ${ROOT_DIR}/bin/payview"
