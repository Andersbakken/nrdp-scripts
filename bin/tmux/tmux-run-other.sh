#!/bin/sh
PANE=left
if [ "$1" = "-t" ]; then
   shift
   PANE="$1"
   shift
fi
ARGS="$@"
tmux send-keys -t "$PANE" "$ARGS" C-m