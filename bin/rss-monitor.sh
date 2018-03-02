#!/bin/bash

PIDS=
declare -a APPS
TIMER=1

while [ -n "$1" ]; do
    case "$1" in
        --timer|-t)
            shift
            TIMER="$1"
            ;;
        --pid|-p)
            shift
            PIDS="$PIDS $1"
            ;;
        --app|-a)
            shift
            APPS+=($1)
            ;;
        *)
            if echo $1 | grep --quiet "^[0-9]\+$"; then
                PIDS="$PIDS $1"
            else
                APPS+=($1)
            fi
            ;;
    esac
    shift
done

function doPid()
{
    rss=`grep ^VmRSS: /proc/$1/status | sed -e 's,^VmRSS: *,,'` 2>/dev/null
    if [ -n "$rss" ]; then
        name=`grep ^Name: /proc/$1/status | sed -e 's,^Name: *,,'` 2>/dev/null
        echo "$1 $name $rss"
    fi
}

while sleep "$TIMER"; do
    # echo "pids $PIDS apps ${APPS[@]}"
    for i in $PIDS; do
        doPid "$i"
    done
    for app in "${APPS[@]}"; do
        for pid in `pidof $app`; do
            doPid "$pid"
        done
    done
done
