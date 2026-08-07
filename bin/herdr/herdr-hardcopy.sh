#!/bin/bash
# Dump the current herdr pane to a file (and optionally open it in emacs).
#
# herdr port of tmux-hardcopy.sh. Same contract: `-e` opens the result,
# $1 is the output path, and a hydra pane yields a prose transcript
# instead of a raw capture.
#
# The session id comes from the `session` metadata token, which
# HerdrHost reports and `herdr pane get` exposes. Its presence is the
# whole test: hydra publishes it while attached to a session and
# withdraws it both on TUI exit and whenever the picker is up, so a
# token means "this pane is showing that session right now".
#
# There used to be a fallback here that walked
# `pane process-info` -> /proc/<pid>/fd/0 -> ~/.hydra-acp/tty/<pty> for
# panes running a hydra too old to report the token. It is gone on
# purpose. That sticky file is deliberately NEVER cleared, so it cannot
# represent "the picker is up": it resolved the previous session for a
# pane that had correctly stopped advertising one, which is the exact
# staleness the token exists to prevent. A fallback that is wrong in the
# case the feature was built for is worse than no fallback. Against an
# old hydra this now takes the raw-capture path, which is merely less
# useful rather than actively misleading.
#
# Do NOT read the session id out of the process cmdline instead.
# `hydra tui --session X` keeps X in argv forever, but the TUI can
# switch sessions afterward; only the token tracks the switch.

set -u

EDIT="no"
if [ "$1" = "-e" ]; then
    EDIT="yes"
    shift
fi

out="$1"

herdr="${HERDR_BIN_PATH:-herdr}"
pane="${HERDR_ACTIVE_PANE_ID:-${HERDR_PANE_ID:-}}"

if [ -z "$pane" ]; then
    echo "herdr-hardcopy: no pane id (HERDR_ACTIVE_PANE_ID unset)" >&2
    exit 1
fi

note() {
    "$herdr" notification show "$1" >/dev/null 2>&1
}

raw_capture() {
    rm -f "$out"
    # capture-pane -J -S -32768  ->  unwrapped recent scrollback.
    "$herdr" pane read "$pane" --source recent-unwrapped --lines 32768 > "$out"
}

# The session this pane is attached to right now, or nothing.
hydra_session() {
    "$herdr" pane get "$pane" 2>/dev/null |
        python3 -c 'import json,sys
try:
    print(json.load(sys.stdin)["result"]["pane"].get("tokens", {}).get("session") or "")
except Exception:
    pass'
}

sess=$(hydra_session)

if [ -n "$sess" ]; then
    # Route to a `.md` sibling so emacs picks markdown-mode
    # automatically. Preserves dir / basename; falls back to "$out.md"
    # when the input has no extension.
    if [ "${out##*.}" != "$out" ]; then
        md_out="${out%.*}.md"
    else
        md_out="$out.md"
    fi
    rm -f "$md_out"
    if hydra-acp sessions transcript "$sess" --out "$md_out" >/dev/null 2>&1; then
        out="$md_out"
        note "Hydra transcript ($sess) → $out"
    else
        raw_capture
        note "Hydra fetch failed; raw capture → $out"
    fi
else
    raw_capture
    note "Captured to: $out"
fi

if [ "$EDIT" = "yes" ]; then
    emacsedit.sh -w -n "$out" >/dev/null 2>&1
fi
