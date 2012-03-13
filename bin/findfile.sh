#!/bin/sh

COL=
LINE=
FILE="$1"
if echo "$FILE" | grep ':[^0-9].*$' >/dev/null 2>&1; then
    FILE=`echo "$FILE" | sed "s,:[^0-9].*$,,"`
fi
if echo "$FILE" | grep ':' >/dev/null 2>&1; then
    COL=`echo $FILE | cut -d: -f3`
    LINE=`echo $FILE | cut -d: -f2`
    FILE=`echo $FILE | cut -d: -f1`
fi

if echo "$FILE" | grep "^//" >/dev/null 2>&1 && test -n "$P4CONFIG" -o -n "$P4PORT"; then
    P4FILE=`echo $FILE | sed "s,[@#].*$,,g"`
    W=`p4 where "$P4FILE" 2>&1`
    if echo "$W" | grep 'not in client view' >/dev/null 2>&1; then
        TEMP=`mktemp`
        p4 print "$P4FILE" >"$TEMP" && FILE="$TEMP"
    else
        FILE=`echo $W | awk '{print $3}'`
    fi
elif [ ! -e "$FILE" ]; then
    FILES=`global -P "$FILE" 2>/dev/null`
    if [ -n "$FILES" ]; then
        FILE="$(choose.pl $FILES)"
    else
        #SYMBOLS=`global -x "$FILE" | awk '{print "-r \"" $4,$5,$6,$7,$8,$9"\" " $3":"$2}'`
        SYMBOLS=`global -x "$FILE" | awk '{print $3":"$2}'`
        [ -n "$SYMBOLS" ] && FILE="$(choose.pl $SYMBOLS)"
    fi
fi
if [ -z "$LINE" ]; then
    echo "${FILE}"
elif [ -z "$COL" ]; then
    echo "${FILE}:${LINE}"
else
    echo "${FILE}:${LINE}:${COL}"
fi
