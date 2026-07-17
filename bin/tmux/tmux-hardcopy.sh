#!/bin/bash
# Dump the current pane to a file (and optionally open it in emacs).
#
# If the current pane is running the hydra TUI it advertises its
# session id via an OSC 1337 SetUserVar that tmux captures on the
# `@hydra_session` pane option — see hydra-acp cli/src/tui/terminal-user-var.ts.
# When that option is set, we dump a `hydra-acp sessions transcript`
# (prose only, no tool bullets) instead of a raw ANSI capture — the
# transcript is far more useful for review / follow-up.

EDIT="no"
if [ "$1" = "-e" ]; then
    EDIT="yes"
    shift
fi

out="$1"

sess=$(tmux show-options -pv @hydra_session 2>/dev/null)
if [ -n "$sess" ]; then
    # Route the hardcopy to a `.md` sibling so emacs picks
    # markdown-mode automatically. Preserves the input path's dir /
    # basename; falls back to "$out.md" if the input has no extension.
    if [ "${out##*.}" != "$out" ]; then
        md_out="${out%.*}.md"
    else
        md_out="$out.md"
    fi
    rm -f "$md_out"
    if hydra-acp sessions transcript "$sess" --out "$md_out" >/dev/null 2>&1; then
        out="$md_out"
        tmux display-message "Hydra transcript ($sess) → $out"
    else
        rm -f "$out"
        tmux -q capture-pane -J -S -32768 \; save-buffer "$out" \; display-message "Hydra fetch failed; raw capture → $out"
    fi
else
    rm -f "$out"
    tmux -q capture-pane -J -S -32768 \; save-buffer "$out" \; display-message "Captured to: $out"
fi

if [ "$EDIT" = "yes" ]; then
    emacsedit.sh -w -n "$out" >/dev/null 2>&1
fi
