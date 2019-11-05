#!/bin/bash

if echo "$@" | grep --quiet "\<show --no-patch --format=.*[^ ]*^{commit} --"; then
    ARGS=()
    while [ -n "$1" ]; do
        case $1 in
            show)
                ARGS+=("log")
                ARGS+=("-n1")
                ;;
            *)
                ARGS+=("$1")
                ;;
        esac
        shift
    done
    git "${ARGS[@]}"
else
    git "$@"
fi
