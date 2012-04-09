# Session tracking

TMUX_RESURRECT="$HOME/.tmux_store"
[ -d "$TMUX_RESURRECT" ] || mkdir -p "$TMUX_RESURRECT"

function tmuxFind() {
    PANE="$1"
}

function cd_tmux_pane() {
    TMUX_PWD=
    TMUX_PWD_FILE="$TMUX_RESURRECT/cwd-${1}"
    [ -e ${TMUX_PWD_FILE} ] && TMUX_PWD=`cat ${TMUX_PWD_FILE}`
    #echo "TMUX: $TMUX_PWD"
    [ -d "$TMUX_PWD" ] && cd "$TMUX_PWD"
}
alias cdp=cd_tmux_pane

function tmuxRestore() {
    PANE="$1"
    [ -z "$PANE" ] && PANE="$TMUX_RESURRECT_PANE"
    if [ -n "$PANE" ]; then
        if [ -e "$TMUX_RESURRECT/name-$PANE" ]; then
            TMUX_NAME=`cat "$TMUX_RESURRECT/name-$PANE"`
            [ -n "$TMUX_NAME" ] && tmux rename-window "$TMUX_NAME"
        fi
        [ -e "$TMUX_RESURRECT/history-$PANE" ] && history -r "$TMUX_RESURRECT/history-$PANE"
        cd_tmux_pane "$PANE"
    fi
}
function tmuxRemove() {
    RM_PANE="$1"
    if [ -z "$RM_PANE" ]; then
        RM_PANE="$TMUX_RESURRECT_PANE"
    fi
    PANE="$RM_PANE"
    FILES="cwd history name"
    for FILE in $FILES; do
        rm -f "$TMUX_RESURRECT/${FILE}-${PANE}"
    done
    while true; do
        DONE=yes
        NEXT_PANE="$((PANE+1))"
        for FILE in $FILES; do
            if [ -e "$TMUX_RESURRECT/${FILE}-${NEXT_PANE}" ]; then
                DONE=no
                mv "$TMUX_RESURRECT/${FILE}-${NEXT_PANE}" "$TMUX_RESURRECT/${FILE}-${PANE}"
            fi
        done
        [ "$DONE" = "yes" ] && break
        PANE="$NEXT_PANE"
    done
    if [ "$RM_PANE" = "$TMUX_RESURRECT_PANE" ]; then
        tmux kill-pane
    fi
}
alias rmp=tmuxRemove

function tmuxPane() {
   TMUX_RESURRECT_PANE="$1"
   if [ -z "$TMUX_RESURRECT_PANE" ] && [ -n "$TMUX_PANE" ]; then
       TMUX_RESURRECT_PANE=`echo $TMUX_PANE | sed "s,^%,,"`
       TMUX_RESURRECT_PANE=$((TMUX_RESURRECT_PANE+1))
   fi
   export TMUX_RESURRECT_PANE
   if [ -n "${TMUX_RESURRECT_PANE}" ]; then
      export HISTFILE="$TMUX_RESURRECT/history-${TMUX_RESURRECT_PANE}"
      TMUX_PROMPT_COMMAND='$HOME/bin/tmux/tmux-name.sh > ${TMUX_RESURRECT}/name-${TMUX_RESURRECT_PANE}; echo "$PWD" > ${TMUX_RESURRECT}/cwd-${TMUX_RESURRECT_PANE}; history -a'
      add_prompt_command $TMUX_PROMPT_COMMAND
      tmuxRestore "$TMUX_RESURRECT_PANE"
   fi
}
tmuxPane

