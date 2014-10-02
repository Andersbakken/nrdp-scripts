#!/bin/bash

dirs=""
nm=`which nm`
symbol=
while [ -n "$1" ]; do
    case "$1" in
        --nm=*)
            nm=`echo $1 | sed -e 's,^[^=]*=,,'`
            ;;
        -n)
            shift
            nm="$1"
            ;;
        -s)
            shift
            symbol="$1"
            ;;
        --symbol=*)
            symbol=`echo $1 | sed -e 's,^[^=]*=,,'`
            ;;
        -h|--help)
            echo "$0 [--nm=/path/to/nm] [-n /path/to/nm] --symbol=symbol -s symbol dirs..."
            exit 0
            ;;
        *)
            if [ -z "$dirs" ]; then
                dirs="$1"
            else
                dirs="$dirs $1"
            fi
            ;;
    esac
    shift
done

if [ -z "$symbol" ]; then
    echo "No symbol specified. $0 [--nm=/path/to/nm] --symbol=foobar dirs..."
    exit 1
fi

out=`mktemp`
err=`mktemp`
test -z "$dirs" && dirs="."
# echo $out $err
# echo $dirs
find $dirs -name "*.so*" -or -name "*.a" -or -name "*.o" -or -name "*.dylib" | while read file; do
    # echo $file
    $nm $file >$out 2> $err
    if grep "no symbols" $err --quiet; then
        $nm -D $file >$out 2> $err
    fi

    if grep "$symbol" -l $out --quiet; then
        echo "$file:"
        echo -----------------------------
        grep $symbol $out
    fi
done

rm -f "$out" "$err"
