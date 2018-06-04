#!/bin/bash

if [ ! -e "$1" ] || [ ! -x "$1" ]; then ### single source file
    while [ ! -e build.ninja ] && [ `pwd` != "/" ]; do
        cd ..
    done
    if [ ! -e build.ninja ]; then
        echo "Can't find build.ninja"
        exit 1
    fi
    "$0" `ninja -t commands | grep "$1" | head -n1`
    exit $?
fi

ARGS=()
while [ -n "$1" ]; do
    if [ "$1" = "-o" ]; then
        shift
    else
        ARGS+=("$1")
    fi
    shift
done
ARGS+=("-E")
"${ARGS[@]}"
