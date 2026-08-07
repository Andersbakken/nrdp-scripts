#!/bin/bash
# Clear the focused herdr pane's screen and scrollback.
#
# herdr port of `bind k send-keys -R \; clear-history`. herdr has no
# clear-history action and no socket method for it, but its emulator does
# honor CSI 3 J (src/pane/osc.rs contains_scrollback_clear_sequence) — the
# droid compat hack there exists precisely to stop droid's redraws from
# eating scrollback, and is scoped so "normal terminal clear-history
# behavior still works elsewhere".
#
# The catch: the sequence has to arrive as pane *output*, not input.
# `herdr pane send-text` writes to the child's stdin, where ESC[3J is just
# bytes the shell reads. So we make the shell emit it, via `pane run`,
# which is send-text plus Enter. Plain `clear` does the job on this box:
# ncurses emits E3 (= CSI 3 J) along with the screen clear, which is why
# this leaves 1 line rather than a screenful of blanks.
#
# Because it goes in as keystrokes, this only makes sense when a shell owns
# the foreground. Against an agent TUI it would submit the word "clear" as
# a prompt, so we check first and refuse.

set -u

herdr="${HERDR_BIN_PATH:-herdr}"
pane="${HERDR_ACTIVE_PANE_ID:-${HERDR_PANE_ID:-}}"

if [ -z "$pane" ]; then
    echo "herdr-clear: no pane id (HERDR_ACTIVE_PANE_ID unset)" >&2
    exit 1
fi

fg=$("$herdr" pane process-info --pane "$pane" 2>/dev/null |
    python3 -c 'import json,sys
try:
    procs = json.load(sys.stdin)["result"]["process_info"]["foreground_processes"]
except Exception:
    sys.exit(0)
print(procs[0]["name"] if procs else "")')

case "$fg" in
    bash|zsh|sh|fish|nu|dash|ksh|"")
        "$herdr" pane run "$pane" clear >/dev/null 2>&1
        "$herdr" notification show "Cleared" >/dev/null 2>&1
        ;;
    *)
        "$herdr" notification show "Not cleared" \
            --body "$fg owns this pane; clear would be typed at it" >/dev/null 2>&1
        exit 1
        ;;
esac
