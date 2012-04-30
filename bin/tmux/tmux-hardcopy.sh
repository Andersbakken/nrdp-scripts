#!/bin/sh
tmux -q capture-pane -S -32768 \; save-buffer -b 0 $1 \; delete-buffer -b 0 \; display-message "Captured to: $1"
