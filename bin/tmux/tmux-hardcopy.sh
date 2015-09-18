#!/bin/sh
rm -f "$1"
tmux -q capture-pane -S -32768 \; save-buffer $1 \; display-message "Captured to: $1"
