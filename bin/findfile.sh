#!/bin/sh

COL=
LINE=
FILE="$1"
if echo "$FILE" | grep ':$' >/dev/null 2>&1; then
    FILE=`echo "$FILE" | sed "s,:$,,"`
fi
if echo "$FILE" | grep ':' >/dev/null 2>&1; then
    COL=`echo $FILE | cut -d: -f3`
    LINE=`echo $FILE | cut -d: -f2`
    FILE=`echo $FILE | cut -d: -f1`
fi

if echo "$FILE" | grep "^//" >/dev/null 2>&1; then
    P4FILE=`echo $FILE | sed "s,[@#].*$,,g"`
    W=`p4 where "$P4FILE" 2>&1`
    if echo "$W" | grep 'not in client view' >/dev/null 2>&1; then
        TEMP=`mktemp`
        p4 print "$P4FILE" >"$TEMP" && FILE="$TEMP"
    else
        FILE=`echo $W | awk '{print $3}'`
    fi
elif [ ! -e "$FILE" ] && FILES=`global -P "$FILE" 2>/dev/null`; then
    [ -n "$FILES" ] && FILE="$(choose.pl $FILES)"      
fi
if [ -z "$LINE" ]; then
    echo "${FILE}"
elif [ -z "$COL" ]; then
    echo "${FILE}:${LINE}"
else
    echo "${FILE}:${LINE}:${COL}"
fi
