if [ -n "$BASH" ]; then
    function complete_emacsedit
    {
        local cur=${COMP_WORDS[$COMP_CWORD]};
        local emacs=`which emacs`
        if [ -x "$emacs" ] && echo $cur | grep --quiet "^--\?[A-Za-z0-9]\?"; then
            COMPREPLY=($( compgen -W "`$emacs --help 2>&1 | grep -o '^--\?[A-Za-z0-9_-]\+'`" -- "$cur" ) )
            return
        fi
        _longopt
        if [ "${#COMPREPLY[@]}" -eq "0" ] && [ -n "$cur" ]; then
            paths=`global -P "$cur" 2> /dev/null | sed -e "s,^.*$cur,$cur,"`
            symbols=`global -c "$cur" 2> /dev/null`
            COMPREPLY=($paths $symbols)
        fi
    }
    complete -F complete_emacsedit emacsedit.sh em emacs
fi
