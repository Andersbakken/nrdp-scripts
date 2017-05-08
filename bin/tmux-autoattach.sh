#!/bin/sh
# Auto-attach to an existing tmux session, or launch a new one if none available.
# Also, if successful in attaching, do nothing else in this shell.
if [ "$TMUX" == "" ]
then
    session_list=$(tmux list-sessions | grep -v attached)
    if [ -n "$session_list" ]
    then
        candidate_session=$(echo $session_list | head -n 1 | cut -d ":" -f 1)
        tmux attach -t $candidate_session
    else
        tmux
    fi

    # After we return from the launched or attached process, exit.
    exit
fi

