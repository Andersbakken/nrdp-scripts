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

if [ ! -x "$CLANG_CHECK" ] && [ `uname` = "Darwin" ]; then
   if [ -x `which brew` ]; then
       CLANG_CHECK=`brew list llvm | sort -r | head -n1`
   fi
   if [ ! -x "$CLANG_CHECK" ] && [ -d /usr/local/Cellar ]; then
       CLANG_CHECK=`find /usr/local/Cellar -name 'clang-check*' | sort -r | head -n1`
       ### won't be right for clang >=10
   fi
fi

if [ -x "$CLANG_CHECK" ]; then
    if [ ! -n "$1" ]; then
        set -- "$@" "."
    fi
    while [ -n "$1" ]; do
        for FILE in `grep "^ *\"file\" *: *\"[^\"]*$1" $DIR/compile_commands.json | awk -F\" '{print $4}' | sort -u`; do
            echo "$CLANG_CHECK" "$FILE"
            # echo
            "$CLANG_CHECK" -analyze "$FILE" -p $DIR
        done
        shift
    done
else
    echo "Can't find clang-check" >&2
fi

rm -rf "$DIR"
