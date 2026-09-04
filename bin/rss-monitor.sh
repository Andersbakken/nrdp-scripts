#!/bin/bash
PIDS=
TIMER=1
PRINT_HEADER=
declare -a APPS
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
        --header|-h)
            PRINT_HEADER=1
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
function kbToMb()
{
    if [ -z "$1" ]; then
        echo "-"
        return
    fi
    echo "$1 / 1024" | bc -l | sed -e 's,\(\.[0-9][0-9]\).*,\1,'
}
function smapsField()
{
    grep "^$2:" "/proc/$1/smaps_rollup" 2>/dev/null | sed -e "s,^$2:[^0-9]*\([0-9]\+\).*,\1,"
}
function doPid()
{
    KB=`grep ^VmRSS: /proc/$1/status | sed -e 's,^VmRSS:[^0-9]*\([0-9]\+\).*,\1,'` 2>/dev/null
    [ -n "$KB" ] || return 1
    MB=`kbToMb "$KB"`
    PRIVATE_DIRTY_KB=`smapsField "$1" Private_Dirty`
    SHARED_DIRTY_KB=`smapsField "$1" Shared_Dirty`
    PRIVATE_DIRTY_MB=`kbToMb "$PRIVATE_DIRTY_KB"`
    SHARED_DIRTY_MB=`kbToMb "$SHARED_DIRTY_KB"`
    name=`grep ^Name: /proc/$1/status | sed -e 's,^Name: *,,'` 2>/dev/null
    if [ -n "$2" ] && [ -n "$PRINT_HEADER" ]; then
        echo "----------- `date +%r` -----------"
        printf "%-24s %12s %12s %12s\n" "PID NAME" "RSS" "PRIVDIRTY" "SHAREDDIRTY"
    fi
    printf "%-24s %12s %12s %12s\n" "$1 $name" "${MB}MB" "${PRIVATE_DIRTY_MB}MB" "${SHARED_DIRTY_MB}MB"
    return 0
}
while sleep "$TIMER"; do
    # echo "pids $PIDS apps ${APPS[@]}"
    FIRST=1
    for i in $PIDS; do
        if doPid "$i" "$FIRST"; then
            FIRST=
        fi
    done
    for app in "${APPS[@]}"; do
        for pid in `pidof $app`; do
            if doPid "$pid" "$FIRST"; then
                FIRST=
            fi
        done
    done
