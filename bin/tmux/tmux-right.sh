#!/bin/bash
RESULT=`uptime | sed 's/.*load average: \([0-9.]*\), \([0-9.]*\), \([0-9.]*\)/Load: \1 \2 \3/'`

JOBS_TOTAL=0
for jobs in `/bin/echo -e "listcs\nquit\n" | nc lgux-abakken4.corp.netflix.com 8766 | grep jobs= | sed 's,.*jobs=\([0-9]*/[0-9]*\).*,\1,'`; do
    JOBS_USE=`echo $jobs | cut -d/ -f1`
    JOBS_COUNT=`echo $jobs | cut -d/ -f2`
    JOBS_COUNT=$((JOBS_COUNT-$JOBS_USE))
    JOBS_TOTAL=$((JOBS_TOTAL+$JOBS_COUNT))
done

if [ "$JOBS_TOTAL" != 0 ]; then
    JOBS_COMP=0
    JOBS_WAIT=0
    HOST=`hostname`
    for job in `/bin/echo -e "listjobs\nquit\n" | nc lgux-abakken4.corp.netflix.com 8766 | grep "$HOST" | awk '{print $2}'`; do
        if [ "$job" = "COMP" ]; then
            JOBS_COMP=$((JOBS_COMP+1))
        elif [ "$job" = "WAIT" ]; then
            JOBS_WAIT=$((JOBS_WAIT+1))
        fi
    done
    MY_JOBS=
    if false && which spark.sh >/dev/null 2>&1; then
        JOBS_FILE="$HOME/.tmux.icecc.jobs"
        TMUX_SESSION=`tmux display-message -p '#S'`
        [ -n "$TMUX_SESSION" ] && JOBS_FILE="${JOBS_FILE}.${TMUX_SESSION}"
        echo "JOBS: $JOBS_FILE" >&2
        which lockfile >/dev/null 2>&1 && lockfile -1 -r1 "${JOBS_FILE}.lock"
        SPARK_JOBS=`tail -1 $JOBS_FILE | awk '{if(NF < 10) printf "%s", $0; else for(i=NF-10+1;i<=NF;++i) { printf "%s ", $i; } }'`
        SPARK_JOBS="$SPARK_JOBS $((JOBS_WAIT+$JOBS_COMP))"
        echo "$SPARK_JOBS" >"$JOBS_FILE"
        rm -f "${JOBS_FILE}.lock"
        MY_JOBS=`spark.sh -range 0:$JOBS_COUNT $SPARK_JOBS`
    fi
    if [ "$JOBS_COMP" != 0 ] || [ "$JOBS_WAIT" != 0 ]; then
        JOBS_COMP=$((JOBS_COMP+JOBS_WAIT))
        [ -z "$MY_JOBS" ] && MY_JOBS="${JOBS_COMP}"
    fi
    if [ -n "$MY_JOBS" ]; then
        RESULT="($MY_JOBS|${JOBS_TOTAL}) $RESULT"
    else
        RESULT="(${JOBS_TOTAL}) $RESULT"
    fi
fi

if [ -e "$HOME/.current-pwd" ]; then
    CURRENT_PWD=`cat $HOME/.current-pwd`
    if [ -n "$CURRENT_PWD" ]; then
        pushd "$CURRENT_PWD" >/dev/null
        SRC_PWD=`git rev-parse --show-toplevel 2>/dev/null`
        [ -z "$SRC_PWD" ] && which lsdev.pl >/dev/null && SRC_PWD=`lsdev.pl -p -tS 2>/dev/null`
        if [ -n "$SRC_PWD" ]; then
            pushd "$SRC_PWD" >/dev/null
            GIT_SUMMARY=`git summarize`
            [ -n "$GIT_SUMMARY" ] && RESULT="(GIT:${GIT_SUMMARY}) $RESULT"
            popd >/dev/null
        fi
        which lsdev.pl >/dev/null && LSDEV=`lsdev.pl -p -ts 2>/dev/null`
        [ -n "$LSDEV" ] && RESULT="$LSDEV $RESULT"
        popd >/dev/null
    fi
fi

echo "$RESULT"
