#!/usr/bin/env bash
set -u

ROWS="${1:-42}"
COLS="${2:-132}"

is_positive_int() {
  [[ "$1" =~ ^[0-9]+$ ]] && [[ "$1" -gt 0 ]]
}

if ! is_positive_int "${ROWS}" || ! is_positive_int "${COLS}"; then
  echo "resize: skipped (invalid args)"
  exit 0
fi

try_iterm_app() {
  local app_name="$1"

  if osascript >/dev/null 2>&1 <<OSA
set targetRows to ${ROWS}
set targetCols to ${COLS}

tell application "${app_name}"
  if not running then
    return false
  end if
  if (count of windows) is 0 then
    create window with default profile
  end if
  tell current session of current window
    set columns to targetCols
    set rows to targetRows
  end tell
  activate
end tell
OSA
  then
    echo "resize: ${app_name} ok"
    return 0
  fi

  if osascript >/dev/null 2>&1 <<OSA
set targetRows to ${ROWS}
set targetCols to ${COLS}

tell application "${app_name}"
  if not running then
    return false
  end if
  if (count of windows) is 0 then
    create window with default profile
  end if
  tell current window
    set columns to targetCols
    set rows to targetRows
  end tell
  activate
end tell
OSA
  then
    echo "resize: ${app_name} ok"
    return 0
  fi

  return 1
}

try_iterm2() {
  try_iterm_app "iTerm2" && return 0
  try_iterm_app "iTerm" && return 0
  return 1
}

try_terminal() {
  local char_w=8
  local char_h=17
  local pad_w=90
  local pad_h=140
  local left=40
  local top=40
  local width=$((COLS * char_w + pad_w))
  local height=$((ROWS * char_h + pad_h))
  local right=$((left + width))
  local bottom=$((top + height))

  if osascript >/dev/null 2>&1 <<OSA
set leftPos to ${left}
set topPos to ${top}
set rightPos to ${right}
set bottomPos to ${bottom}

tell application "Terminal"
  if not running then
    return false
  end if
  if (count of windows) is 0 then
    do script ""
  end if
  set bounds of front window to {leftPos, topPos, rightPos, bottomPos}
  activate
end tell
OSA
  then
    echo "resize: Terminal ok"
    return 0
  fi

  return 1
}

try_ansi_request() {
  case "${TERM_PROGRAM:-}" in
    iTerm.app|Apple_Terminal)
      printf '\033[8;%s;%st' "${ROWS}" "${COLS}"
      echo "resize: ansi request sent"
      return 0
      ;;
  esac
  return 1
}

if try_iterm2; then
  exit 0
fi

if try_terminal; then
  exit 0
fi

if try_ansi_request; then
  exit 0
fi

echo "resize: skipped (unsupported)"
exit 0
