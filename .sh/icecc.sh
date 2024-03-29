[ -z "$ICECC_JOBS_MAX" ] && ICECC_JOBS_MAX=10
[ -z "$ICECC_JOBS_MIN" ] && ICECC_JOBS_MIN=1
if [ -z "$ICECC_SCHEDULER_HOST" ]; then
    [ -e "/etc/icecc/icecc.conf" ] && . /etc/icecc/icecc.conf 
    [ -e "/etc/icecc/icecc.conf.override" ] && . /etc/icecc/icecc.conf.override 
fi
[ ! -d "$ICECC_DIR" ] && ICECC_DIR=
if [ ! -z "$ICECC_DIR" ]; then
   true #do nothing
elif [ -d "$HOME/bin/icecc" ]; then
    ICECC_DIR="$HOME/bin/icecc"
elif [ -d "/usr/lib/icecc/bin" ]; then
    ICECC_DIR="/usr/lib/icecc/bin"
fi

canblockcs() {
    HOST=`nslookup $1  | grep 'name = ' | sed 's,.*name = ,,'`
    echo "$1 -> $HOST"
    for w in smagnuson pnavarro abakken jhanssen ppissanetzky mdxapp2 lgud01-nrdp lgud-02; do
        if echo "$HOST" | grep -q "$w"; then
            echo " + WHITELISTED!"
            return 1
        fi
    done
    for w in 10.2.84.164 10.2.84.165; do
        if echo "$1" | grep -q "$w"; then
            echo " + WHITELISTED!"
            return 1
        fi
    done

    return 0
}

blockcs() {
    for a in $@; do
        canblockcs $a && echo blockcs "$a" | nc "$ICECC_SCHEDULER_HOST" 8766
    done
}

unblockcs() {
    for a in $@; do
        nslookup "$a"
        echo unblockcs "$a" | nc "$ICECC_SCHEDULER_HOST" 8766
    done
}

listblocks() {
    echo listblocks | nc "$ICECC_SCHEDULER_HOST" 8766
}

cleancs() {
    CLEAN_FILE="/tmp/cleancs.tmp"
    rm -f "$CLEAN_FILE"
    for a in `echo listcs | nc $ICECC_SCHEDULER_HOST 8766 | grep '^ ' | sed "s,^ *\(.*\) (\(.*\)).* jobs=[0-9]\+/\([0-9]\+\) .*$,HOST:\3:\1:\2:," | grep HOST`; do
        #echo "Trying: $a"
        JOBS=`echo $a | cut -d: -f2`
        NAME=`echo $a | cut -d: -f3`
        IP=`echo $a | cut -d: -f4`
        PORT=`echo $a | cut -d: -f5`
        if [ "$JOBS" = "0" ]; then
            echo "$NAME($JOBS) is harmless!"
        elif nc $IP $PORT -z -w 2 >/dev/null 2>&1; then
            echo "$NAME($JOBS) is listening!"
        else
            echo "$NAME($JOBS) is not listening!"
            canblockcs $IP && echo "blockcs $IP" >>"$CLEAN_FILE"
        fi
    done
    [ -e "$CLEAN_FILE" ] && cat "$CLEAN_FILE" | nc "$ICECC_SCHEDULER_HOST" 8766
}

seticecc() {
   if [ -z "$ICECC_DIR" ]; then
#      echo "No IceCream found!!"
      return
   fi
   if [ "$1" != "on" ] && [ "$1" != "off" ]; then
        echo "seticecc must be on or off ($1)"
    return
   fi
#   if [ "$1" = "on" ] && [ -z "$ICECC_VERSION" ]; then
#     NATIVE_ICECC_VERSION="$HOME/.native-icecc.tar.gz"
#     if [ ! -e "$NATIVE_ICECC_VERSION" ]; then
#     N=`(cd /tmp && icecc --build-native 2>&1 | grep creating | sed "s,creating \(.*.tar.gz\),\1,")`
#      if [ -z "$N" ] || [ ! -e "/tmp/$N" ]; then
#	return
#     fi
#     mv "/tmp/${N}" "${NATIVE_ICECC_VERSION}"
#     fi
#     ICECC_VERSION="${NATIVE_ICECC_VERSION}"
#   fi

   NUMJOBS=
   ICECC_OFF="-off"
   if [ "$1" = "on" ]; then
       unset ICECC
       unset ICECC_DISABLED
       if echo $PATH | grep "${ICECC_DIR}${ICECC_OFF}:" >/dev/null 2>&1; then
           PATH=`echo $PATH | sed "s,${ICECC_DIR}${ICECC_OFF}:,,g"`
       elif echo $PATH | grep "${ICECC_DIR}:" >/dev/null 2>&1; then
          PATH=`echo $PATH | sed "s,${ICECC_DIR}:,,g"`
       fi
       PATH="${ICECC_DIR}:$PATH"
#       which icecc >/dev/null 2>&1 && NUMJOBS=`icecc -numjobs 2>/dev/null | tail -1`
       MAXJOBS="$ICECC_JOBS_MAX"
       [ -z "$MAXJOBS" ] && MAXJOBS=30
       if [ -z "$NUMJOBS" ] || [ "$NUMJOBS" -gt "$MAXJOBS" ]; then
       NUMJOBS="$MAXJOBS"
       fi
   else
       export ICECC=no
       export ICECC_DISABLED=1
       if echo $PATH | grep $ICECC_DIR >/dev/null 2>&1; then
           PATH=`echo $PATH | sed "s,${ICECC_DIR},${ICECC_DIR}${ICECC_OFF},g"`
       fi
   fi
   MAKE=$(which make)
   if [ -n "$MAKE" ] >/dev/null 2>&1 && $MAKE -v 2>&1 | grep GNU >/dev/null || uname -a 2>&1 | grep BSD >/dev/null 2>&1; then
       if [ "$1" = "on" ] && [ ! -z "$NUMJOBS" ]; then
          MAKEFLAGS=-j${NUMJOBS}
       else
          MAKEFLAGS=-j${ICECC_JOBS_MIN}
       fi
   fi
   export PATH MAKEFLAGS ICECC_VERSION
   hash -r
}
