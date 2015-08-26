
#emacs build
emake()
{
    "emacsedit.sh" -m -n "${1}"
}

#reconfigure wrapper
reconfigure()
{
    `which reconfigure` "$@"
    cd "$PWD"
}

#make wrapper
make()
{
    "ubermake.sh" "$@"
}

#gdb wrapper
gdb()
{
    EDITOR="emacsedit.sh -n" `which gdb` "$@"
}

#emacs diff
ediff()
{
    f="ediff-files"
    if [ "$1" = "-b" ]; then
        f="sam-ediff-binary-files"
        shift
    fi
    "emacsedit.sh" -r -n "($f \"${1}\" \"${2}\")"
}

#ecd
ecd()
{
    if [ -n "$1" ]; then
        emacsedit.sh -n "$1"
    else
        cdd "@"
    fi
}

#emacs tail
etail()
{
    "emacsedit.sh" -n -t "$1"
}

#global
findsym() {
    ROOT=`findancestor GPATH 2>&1 || findancestor GTAGS 2>&1|| findancestor GRTAGS 2>&1`
    if [ -z "$ROOT" ]; then
        echo "No gtags found!"
    else
        ROOT=`dirname $ROOT`
        GLOBAL_OPTS="-x"
        if [ "$1" = "-h" ]; then
            echo "findsym <option> <find>"
            echo "Options:"
            echo
            echo "-symbol: <find> a symbol."
            echo "-caller: <find> a caller of symbol."
            echo "-tag: <find> a referencd string."
            echo "-file: <find> a file."
            return 1
        elif [ "$1" = "-symbol" ] || [ "$1" = "-s" ]; then
            shift
            GLOBAL_OPTS="-xs"
        elif [ "$1" = "-caller" ] || [ "$1" = "-r" ] || [ "$1" = "-reference" ]; then
            shift
            GLOBAL_OPTS="-xr"
        elif [ "$1" = "-tag" ] || [ "$1" = "-t" ]; then
            shift
            GLOBAL_OPTS="-x"
        elif [ "$1" = "-file" ] || [ "$1" = "-f" ]; then
            shift
            GLOBAL_OPTS="-xPo"
        fi
        #echo "$ROOT :: $GLOBAL_OPTS :: $@ "
        SYM=`(cdo "$ROOT" && choose.pl -a -x "global $GLOBAL_OPTS $@")`
        if [ -n "$SYM" ]; then
            echo "$SYM"
            FILE=`echo $SYM | awk '{print $3}'`
            LINE=`echo $SYM | awk '{print $2}'`
            edit "$ROOT/$FILE:$LINE"
        else
            echo "Not found!"
        fi
    fi
}

function cat-emacs()
{
    test -z "$EMACSCLIENT" && EMACSCLIENT=emacsclient
    emacsclient -e "(misc-cat-emacs)" &> /dev/null
    cat /tmp/cat-emacs.tmp
}

function findcmake()
{
    if [ -z "$1" ]; then
        find -L "$PWD" -name CMakeLists.txt -or -name '*.cmake'
    else
        while [ -n "$1" ]; do
            if [ -d "$1" ]; then
                find -L "$1" -name CMakeLists.txt -or -name '*.cmake'
            else
                find -L "$PWD" -name CMakeLists.txt -or -name '*.cmake' | xargs grep -i "$1"
            fi
            shift
        done
    fi
}

block-icecream() {
    [ -e "/etc/icecc/icecc.conf" ] && . /etc/icecc/icecc.conf
    [ -z "$ICECC_SCHEDULER_HOST" ] && ICECC_SCHEDULER_HOST="lgux-pnavarro3.corp.netflix.com"
    echo blockcs "$1" | nc "$ICECC_SCHEDULER_HOST" 8766
}

complete-netflix ()
{
    app=${COMP_WORDS[0]}
    test -x "$app" || return;
    modified="`ls -la \"$app\" | awk '{print $5,$6,$7,$8}'`"
    if [ ! -e "/tmp/netflix-completions-helper" ] || [ "$modified" != "`head -n 1 /tmp/netflix-completions-helper`" ]; then
        echo $modified > /tmp/netflix-completions-helper
        "$app" --help | grep '^ \+-' | grep "\[value\]" | sed -e 's,|NF.*,,' -e 's,|, ,' -e 's,^ *,,' | xargs >> /tmp/netflix-completions-helper
        "$app" --help | grep '^ \+-' | grep -v "\[value\]" | sed -e 's,|NF.*,,' -e 's,|, ,' -e 's,^ *,,' | xargs >> /tmp/netflix-completions-helper
    fi
    local valueopts=`head -n 2 /tmp/netflix-completions-helper | tail -n 1`
    local cur=${COMP_WORDS[COMP_CWORD]}
    local prev=${COMP_WORDS[COMP_CWORD-1]}
    if [ -n "$prev" ]; then
       if [ "$prev" = "-x" ] || [ "$prev" = "--config-file" ]; then
           dir=$(type $app | sed -e "s,^$app is ,,")
           dir=$(dirname $dir)
           confs=`/bin/ls "$dir/data/etc/conf/" | sed -e 's,\.xml,,' | xargs`
           COMPREPLY=($(compgen -W "${confs}" -- ${cur}))
           return;
       elif printf -- "${valueopts}\n" | grep --quiet -- "$prev"; then
           COMPREPLY=()
           return;
       fi
    fi

    local nonvalueopts=`tail -n 1 /tmp/netflix-completions-helper`
    COMPREPLY=(`compgen -W "$valueopts $nonvalueopts" -- $cur`)
    if [ -n "$cur" ] && [ ${#COMPREPLY[@]} -eq 0 ] && printf -- "$cur\n" | grep --quiet -- "^-[^-]"; then
        COMPREPLY=(`compgen -W "$valueopts $nonvalueopts" -- -$cur`)
    fi
}

if [ -n "$BASH" ]; then
    complete -F complete-netflix -o default netflix ./netflix
fi
