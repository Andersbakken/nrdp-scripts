#!/usr/bin/env bash

IFACE=
LIMIT=

while [ -n "$1" ]; do
    case "$1" in
        --help|-h)
            echo "bwlimit.sh [--help|-h] [--iface|-i] <interface> [--limit|-l] [limit_in_megabits_per_second]"
            exit 0
            ;;
        --iface|-i)
            shift
            IFACE="$1"
            ;;
        --limit|-l)
            shift
            LIMIT=$1
            ;;
        *)
            echo "bwlimit.sh [--help|-h] [--iface|-i] <interface> [--limit|-l] [limit_in_megabits_per_second]" >&2
            echo "Unknown argument $1" >&2
            exit 1
            ;;
    esac
    shift
done

if [ -z "$IFACE" ]; then
    echo "No interface selected:" >&2
    ifconfig | grep -o "^[A-Za-z0-9]\+" | sed -e 's,^,    ,' >&2
    exit 1
fi


sudo tc qdisc del dev $IFACE root &>/dev/null # clear existing
if [ -n "$LIMIT" ]; then
    echo sudo tc qdisc add dev $IFACE root handle 1:0 htb default 10
    echo sudo tc class add dev $IFACE parent 1:0 classid 1:10 htb rate ${LIMIT}mbit ceil ${LIMIT}mbit
    echo "Limiting ${IFACE} to ${LIMIT}"
else
    echo "Limits cleared for ${IFACE}"
fi
