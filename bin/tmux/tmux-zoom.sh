#!/bin/sh
ZOOM_NAME="tmux-zoom"
CURRENT=`$HOME/bin/tmux/tmux-name.sh`
if [ "$CURRENT" = "$ZOOM_NAME" ]; then
    tmux last-window
    tmux swap-pane -s $ZOOM_NAME.0
    tmux kill-window -t $ZOOM_NAME
else
    tmux new-window -d -n $ZOOM_NAME 'clear && echo $ZOOM_NAME && read'
    tmux swap-pane -s $ZOOM_NAME.0
    tmux select-window -t $ZOOM_NAME
fi

