#!/bin/sh
EDIT="no"
if [ "$1" = "-e" ]; then
    EDIT="yes"
    shift
fi

rm -f "$1"
tmux -q capture-pane -S -32768 \; save-buffer $1 \; display-message "Captured to: $1"

if [ "$EDIT" = "yes" ]; then
    emacsedit.sh -n "$1" >/dev/null 2>&1
fi
