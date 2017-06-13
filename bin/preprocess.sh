#!/bin/bash

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
