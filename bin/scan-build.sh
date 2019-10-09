#!/bin/bash

if [ ! -e "$1" ] || [ ! -x "$1" ]; then ### single source file
    while [ ! -e build.ninja ] && [ `pwd` != "/" ]; do
        cd ..
    done
    if [ ! -e build.ninja ]; then
        echo "Can't find build.ninja"
        exit 1
    fi
    "$0" `ninja -t commands | grep "\<$1\>" | head -n1`
    exit $?
fi

SCAN_BUILD=
if [ -x "`which scan-build`" ]; then
    SCAN_BUILD=`which scan-build`
else
    for i in `seq 20 -1 3`; do
        # echo "trying scan-build-$i `which scan-build-$i`"
        if [ -x "`which scan-build-$i`" ]; then
            SCAN_BUILD=`which scan-build-$i`
            break;
        fi
    done
fi

ARGS=()
while [ -n "$1" ]; do
    case "$1" in
        -Wa,--32|--Wa,--64|-fno-var-tracking-assignments|-fno-delete-null-pointer-checks|*ccache)
        ;;
        *)
            ARGS+=("$1")
            ;;
    esac
    shift
done

ARGS+=("-Wno-ignored-optimization-argument")

if [ -x "$SCAN_BUILD" ]; then
    echo "$SCAN_BUILD" "${ARGS[@]}"
    echo
    "$SCAN_BUILD" "${ARGS[@]}"
else
     echo "Can't find scan-build" >&2
fi

