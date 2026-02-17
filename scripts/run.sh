#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

ROWS=42
COLS=132
RESIZE_ENABLED=1

usage() {
  cat <<USAGE
Usage: ${0##*/} [--no-resize] [--rows N] [--cols N]

Options:
  --no-resize   Skip launch-time terminal resize
  --rows N      Target terminal rows (default: 42)
  --cols N      Target terminal cols (default: 132)
  -h, --help    Show this help
USAGE
}

is_positive_int() {
  [[ "$1" =~ ^[0-9]+$ ]] && [[ "$1" -gt 0 ]]
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --no-resize)
      RESIZE_ENABLED=0
      shift
      ;;
    --rows)
      if [[ $# -lt 2 ]]; then
        echo "error: --rows requires a value" >&2
        usage
        exit 1
      fi
      ROWS="$2"
      shift 2
      ;;
    --cols)
      if [[ $# -lt 2 ]]; then
        echo "error: --cols requires a value" >&2
        usage
        exit 1
      fi
      COLS="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if ! is_positive_int "${ROWS}"; then
  echo "error: --rows must be a positive integer" >&2
  exit 1
fi

if ! is_positive_int "${COLS}"; then
  echo "error: --cols must be a positive integer" >&2
  exit 1
fi

"${SCRIPT_DIR}/build.sh"

if [[ "${RESIZE_ENABLED}" -eq 1 ]]; then
  "${SCRIPT_DIR}/resize_terminal.sh" "${ROWS}" "${COLS}" || true
fi

effective_rows="${ROWS}"
effective_cols="${COLS}"

if [[ -t 1 ]]; then
  if tty_size="$(stty size 2>/dev/null)"; then
    read -r detected_rows detected_cols <<<"${tty_size}"
    if is_positive_int "${detected_rows}"; then
      effective_rows="${detected_rows}"
    fi
    if is_positive_int "${detected_cols}"; then
      effective_cols="${detected_cols}"
    fi
  fi
fi

export PAYVIEW_TERM_ROWS="${effective_rows}"
export PAYVIEW_TERM_COLS="${effective_cols}"

cd "${ROOT_DIR}"
exec "${ROOT_DIR}/bin/payview"
