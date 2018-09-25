#!/bin/bash


while [ ! -e compile_commands.json ] && [ `pwd` != "/" ]; do
    cd ..
done
if [ ! -e compile_commands.json ]; then
    echo "Can't find compile_commands.json"
    exit 1
fi

DIR=clang_check_$$
mkdir "$DIR"

cat compile_commands.json \
    | sed -e 's,[^ ]*fiskc ,,' -e 's,/usr/bin/ccache,,' -e 's/-Wa,--[0-9][0-9]//' -e 's,-fno-var-tracking-assignments,,' -e 's,--fisk-compiler=\([^ ]*\),\1,' -e 's, -c , -Wno-unknown-warning-option -c ,' \
          > $DIR/compile_commands.json

CLANG_CHECK=
if [ -x "`which clang-check`" ]; then
    CLANG_CHECK=`which clang-check`
else
    for i in `seq 20 -1 3`; do
        # echo "trying clang-check-$i `which clang-check-$i`"
        if [ -x "`which clang-check-$i`" ]; then
            CLANG_CHECK=`which clang-check-$i`
            break;
        fi
    done
fi

if [ -x "$CLANG_CHECK" ]; then
    while [ -n "$1" ]; do
        FILE=`grep "^ *\"file\" *: *\"[^\"]*$1\"" $DIR/compile_commands.json | head -n1 | sed -e 's/^[^:]*: *"\(.*\)\",\?/\1/'`
        echo "$CLANG_CHECK" "$FILE"
        echo
        "$CLANG_CHECK" "$FILE" -p $DIR
        shift
    done
else
    echo "Can't find clang-check" >&2
fi

