#!/bin/sh

EMACS=
EMACSWAIT=yes
EMACSOPTS=""
CONFIRM=no
MODE=edit
FILE="$PWD"
LINE=0
COL=
TEST=
EMACSDAEMON=no

while [ "$#" -gt 0 ]; do
    case "$1" in
    -f) EMACS="emacs" ;;
    -c) CONFIRM=yes ;;
    -e) MODE=eval ;;
    -t) MODE=tail ;;
    -m) MODE=make ;;
    -s|--dry) TEST=echo ;;
    -n) EMACSWAIT=no ;;
    -q) TEST=exists ;;
    --daemon|-d) EMACSDAEMON="yes" ;;
    -nw) unset DISPLAY ;;
    -h|--help|-help) echo "$0: [options] [file]"
        echo "Options:"
        echo "  -c    Must confirm execution"
        echo "  -t    Tail provided file"
        echo "  -e    Treat file as elisp and evaluate it"
        exit 0
        ;;
    +*) LINE=`echo $1 | sed 's,^+,,'` ;;
    -*) EMACSOPTS="$EMACSOPTS $1" ;;
    *)  FILE="$1" ;;
    esac
    shift
done
[ -z "$FILE" ] && [ "$MODE" = "make" ] && FILE="$PWD"

if [ -z "$EMACS" ]; then
    if [ -n "$EMACSCLIENT" ]; then
        EMACS="$EMACSCLIENT"
    elif which gnuclient >/dev/null 2>&1 && gnuclient -v >/dev/null 2>&1; then
        EMACS="gnuclient -q"
    elif which emacsclient >/dev/null 2>&1; then
        EMACS="emacsclient -nw"
        if [ "$EMACSDAEMON" = "yes" ]; then
            EMACS="$EMACS -a \"\""
        elif [ -z "$ALTERNATE_EDITOR" ]; then
            EMACS="$EMACS -a $(which emacs)"
        fi
        [ "$EMACSWAIT" = "no" ] && EMACS="$EMACS -n"
    else
        echo "No emacs client available!"
        return
    fi
else
    EMACS="$EMACS $EMACSOPTS"
fi

if [ -n "$FILE" ]; then
    FILE=`findfile.sh $FILE`
    if [ "CONFIRM" = "yes" ]; then
        echo -n "$FILE"
        read confirm
        [ -n "$confirm" ] && return
    fi
    if echo "$FILE" | grep ':' >/dev/null 2>&1; then
        COL=`echo $FILE | cut -d: -f3`
        LINE=`echo $FILE | cut -d: -f2`
        FILE=`echo $FILE | cut -d: -f1`
    fi
    if [ "$TEST" = "exists" ]; then
        [ -e "$FILE" ] && exit 0
        exit 1
    elif [ "$MODE" = "eval" ]; then
        $TEST $EMACS -e "$FILE"
    elif [ "$MODE" = "make" ]; then
        [ -z "$EMACSEDIT_COMPILE_DIRECTORY_DEFUN" ] && EMACSEDIT_COMPILE_DIRECTORY_DEFUN="sam-compile-directory"
        $TEST $EMACS -e "($EMACSEDIT_COMPILE_DIRECTORY_DEFUN \"$FILE\")"
    elif [ "$MODE" = "tail" ]; then
        $TEST $EMACS -e "(tailf \"$FILE\")"
    else
        JUMP=
        if [ -n "$LINE" ]; then
            [ -z "$COL" ] && COL="0"
            JUMP="${LINE}:${COL}"
        else
            JUMP=0
        fi
        $TEST eval $EMACS "+${JUMP}" "$FILE"
    fi
elif [ "$TEST" != "exists" ]; then
    $TEST eval $EMACS
fi
exit 1
