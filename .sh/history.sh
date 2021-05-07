[ -n "$BASH" ] && shopt -s histappend

export HISTSIZE=500000
export HISTFILESIZE=5000000
export HISTCONTROL=ignoredups
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
}

cmd_history_share_auto()
{
    [ "$HISTORY_SHARE_AUTO_WRITE" = "1" ] && history -a
    [ "$HISTORY_SHARE_AUTO_READ" = "1" ] && history -n
}

history_share_auto read write
add_prompt_command "cmd_history_share_auto"


