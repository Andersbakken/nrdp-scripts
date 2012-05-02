
#emacs build
emake()
{
    "emacsedit.sh" -m -n "${1}"
}

#reconfigure
reconfigure()
{
    while [ "$1" ]; do
        if [ "$1" = "-rm" ]; then
            local RM=1
        elif [ "$1" = "-cat" ]; then
            local CAT=1
        elif [ "$1" = "-reset" ]; then
            local RESET=1
        elif [ "$1" = "-editor" ]; then
            local EDIT=1
        else
            break
        fi
        shift
    done

    if [ "$RESET" -a "$EDIT" ] || [ "$RESET" -a "$CAT" ]; then
        echo "Invalid combination of arguments." >2
        return 1
    fi

    BUILD=
    if [ -d "$1" ]; then
        BUILD="$1"
        shift
    fi
    if [ -z "$BUILD" ] || [ ! -e "$BUILD/config.status" ]; then
        BUILD=`lsdev build $BUILD -r`
    fi

    pushd "$BUILD" >/dev/null 2>&1
    SRC=`lsdev src - -r`
    echo "Root: $BUILD [$SRC]"
    if [ -e "config.status" ] && [ ! -e "configure" ]; then
        if [ "$RM" ]; then
            echo -n "Sure you want to rm -rf in $BUILD? "
            read REALLY
            if [ "$REALLY" = "y" ]; then
                rm -f "/tmp/config.status"
                cp "config.status" "/tmp/config.status"
                rm -rf *
                cp "/tmp/config.status" "config.status"
            fi
        fi
    fi
    if [ "$RESET" ]; then
        if [ -e "$SRC/configure" ]; then
            $SRC/configure "$@"
        fi
    elif [ -e "config.status" ]; then
        if [ "$EDIT" ]; then
            test -z "$EDITOR" && EDITOR=vim
            $EDITOR ./config.status
        fi

        if [ "$CAT" ]; then
            cat "config.status"
        else
            cat "config.status" | yank -c
            ./config.status "$@"
        fi
    fi
    popd >/dev/null 2>&1
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

