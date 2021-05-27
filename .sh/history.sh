export HISTSIZE=500000
export HISTFILESIZE=5000000
export HISTIGNORE=$'&'

history_share_auto()
{
    if [ "$#" = 0 ]; then
        export HISTORY_SHARE_AUTO_READ=1
        export HISTORY_SHARE_AUTO_WRITE=1
    else
        for a in $@; do
            if [ "$a" = "read" ]; then
                export HISTORY_SHARE_AUTO_READ=1
            elif [ "$a" = "noread" ]; then
                export HISTORY_SHARE_AUTO_READ=0
            elif [ "$a" = "write" ]; then
                export HISTORY_SHARE_AUTO_WRITE=1
            elif [ "$a" = "nowrite" ]; then
                export HISTORY_SHARE_AUTO_WRITE=0
            elif [ "$a" = "off"  ] || [ "$a" = "none" ]; then
                export HISTORY_SHARE_AUTO_READ=0
                export HISTORY_SHARE_AUTO_WRITE=0
            elif [ "$a" = "on"  ] || [ "$a" = "all" ]; then
                export HISTORY_SHARE_AUTO_READ=1
                export HISTORY_SHARE_AUTO_WRITE=1
            fi
        done
    fi
    if [ -n "$ZSH_VERSION" ]; then
        if [ "$HISTORY_SHARE_AUTO_READ" = "1" ]; then
            setopt SHARE_HISTORY
        else
            unsetopt SHARE_HISTORY
        fi
        if [ "$HISTORY_SHARE_AUTO_WRITE" = "1" ]; then
            setopt INC_APPEND_HISTORY
        else
            unsetopt INC_APPEND_HISTORY
        fi
    fi
}

if [ -n "$BASH" ]; then
    cmd_history_share_auto()
    {
        [ "$HISTORY_SHARE_AUTO_WRITE" = "1" ] && history -a
        [ "$HISTORY_SHARE_AUTO_READ" = "1" ] && history -n
    }
    export HISTCONTROL=ignoredups
    shopt -s histappend
    add_prompt_command "cmd_history_share_auto"
elif [ -n "$ZSH_VERSION" ]; then
    export SAVEHIST="$HISTSIZE"
    export HISTFILE="$HOME/.zhistory"
    setopt APPEND_HISTORY
    setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
    setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
    setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
    setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
    setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
    setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
    setopt HIST_SAVE_NO_DUPS
fi
history_share_auto read write

