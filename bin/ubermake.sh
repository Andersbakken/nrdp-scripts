#!/bin/bash

findancestor() {
    file="$1"
    dir="$2"
    [ -z "$dir" ] && dir="$PWD"
    (cd $dir && while true; do
            if [ -e "$file" ]; then
                echo "$PWD/$file"
                return 1
            elif [ "$PWD" = "/" ]; then
                break
            else
                cd ..
            fi
        done)
    return 0
}
SUCCESS_POST_COMMAND=
ERROR_POST_COMMAND=

numcores() {
    if [ -e "/proc/cpuinfo" ]; then
        grep -c "^processor" "/proc/cpuinfo"
    else
        sysctl -n hw.ncpu
    fi
}

resolvelink () {
    filename="$1"
    max=10
    while [ $max -gt 0 -a -L "$filename" ]; do
        max=$((max - 1))
        link=$(readlink "$filename")
        if echo "$link" | grep --quiet "^/"; then
            filename="$link"
        else
            filename="$(dirname $filename)/$link"
        fi
    done
    echo $filename
}

finish() {
    if [ "$1" -eq 0 ]; then
        [ -n "$SUCCESS_POST_COMMAND" ] && eval "$SUCCESS_POST_COMMAND"
    else
        [ -n "$ERROR_POST_COMMAND" ] && eval "$ERROR_POST_COMMAND"
    fi
    exit "$1"
}

build() {
    local BUILD_DIR="$1"
    echo $BUILD_DIR | grep --quiet "/$" || BUILD_DIR="${BUILD_DIR}/"
    if [ -e "${BUILD_DIR}Makefile" ]; then
        [ "$VERBOSE" = "1" ] && MAKE_OPTIONS="AM_DEFAULT_VERBOSITY=1 $MAKE_OPTIONS"
        true #ok, make it is...
    elif [ -x "`which scons`" ] && [ -e "${BUILD_DIR}SConstruct" ]; then
        SCONS_OPTIONS=
        for opt in $MAKEFLAGS $MAKE_OPTIONS; do
            case $opt in
                clean|distclean) SCONS_OPTIONS="$SCONS_OPTIONS -c" ;;
                *) SCONS_OPTIONS="$SCONS_OPTIONS $opt" ;;
            esac
        done
        (cd $BUILD_DIR && scons $SCONS_OPTIONS)
        return
    elif [ -e "${BUILD_DIR}Sakefile.js" ]; then
        SAKE_OPTIONS=
        for opt in $MAKEFLAGS $MAKE_OPTIONS; do
            case $opt in
                -j[0-9]*) ;;
                help) SAKE_OPTIONS="$SAKE_OPTIONS -T" ;;
                distclean) SAKE_OPTIONS="$SAKE_OPTIONS clobber" ;;
                *) SAKE_OPTIONS="$SAKE_OPTIONS $opt" ;;
            esac
        done
        (cd $BUILD_DIR && sake $SAKE_OPTIONS)
        return
    else
        if [ -n "$RTAGS" ]; then
            DIR="$BUILD_DIR"
            [ -z "$DIR" ] && DIR=.
            COMPILATION_DATABASEJSON=`findancestor compile_commands.json $DIR`
            # echo "FOUND IT $COMPILATION_DATABASEJSON"
            if [ -e "$COMPILATION_DATABASEJSON" ]; then
                rc -J "$COMPILATION_DATABASEJSON"
                return 0
            fi
        fi
        if which ninja >/dev/null 2>&1; then
            NINJA_DIR=$BUILD_DIR
            [ -z "$NINJA_DIR" ] && NINJA_DIR=.
            NINJA=`findancestor build.ninja $NINJA_DIR`
            if [ -e "$NINJA" ]; then
                cd `dirname $NINJA`
                if [ -n "$RTAGS" ]; then
                    ninja -t commands | rc --compile
                    return 0
                fi
                NINJA_OPTIONS=
                [ "$VERSION" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS --version"
                [ "$VERBOSE" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS -v"
                for opt in $MAKEFLAGS $MAKE_OPTIONS; do
                    case $opt in
                        clean|distclean) NINJA_OPTIONS="$NINJA_OPTIONS -t clean" ;;
                        -k) NINJA_OPTIONS="$NINJA_OPTIONS -k 1000" ;;
                        *) NINJA_OPTIONS="$NINJA_OPTIONS $opt" ;;
                    esac
                done
                NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e "s,-j \([0-9]\+\),-j\1,g"`
                NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e 's,-j ,-j1000 ,g' -e 's,-j$,-j1000,'`
                # START=`date +%s%N | cut -b1-13`
                NUM="`echo $NINJA_OPTIONS | grep -o -- "-j *[0-9]\+" | sed -e 's,-j *,,' | tail -n1`"
                if [ -n "$NUM" ]; then
                    CORES=`numcores`
                    MAX=$(expr $(expr ${CORES} \* 150) / 100) # 1.5 * $CORES
                    if [ "$NUM" -gt "$MAX" ]; then
                        # echo "max is $max num is $num"
                        LINE=`ninja -t commands | grep "\.o" 2>/dev/null | head -n1`
                        for i in $LINE; do
                            [ ! -e "$i" ] && continue
                            echo "$i" | grep --quiet "\\(.*ccache\\|.*rtags-gcc-prefix.sh\\|.*cc_prefix.sh\\|make$\\)" && continue
                            # echo "$i" | grep --quiet "\\(icecream\\|icecc\\|plast\\)" && break

                            if [ -L "$i" ]; then
                                RESOLVED="`resolvelink $i`"
                                if [ "$(basename $RESOLVED)" = "gcc-rtags-wrapper.sh" ]; then
                                    for f in $(which -a "$(basename "$i")"); do
                                        RESOLVED="`resolvelink $f`"
                                        [ "$(basename $RESOLVED)" != "gcc-rtags-wrapper.sh" ] && break
                                    done
                                fi
                                i="$RESOLVED"
                            fi
                            case "$i" in
                                *icecc|*plastc)
                                    # echo "It's icecream $f $i"
                                    ;;
                                *) ### no build farm, lets reduce jobs
                                    NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e "s,-j[0-9]\+,-j$MAX,g"`
                                    # echo "Found thing $i"
                                    ;;
                            esac
                            break
                        done
                    fi
                fi
                # END=`date +%s%N | cut -b1-13`
                # expr $END - $START
                ninja $NINJA_OPTIONS
                return $?
            fi
        fi
    fi
    which -a make | while read i; do
        if [ -L "$i" ] && readlink "$i" | grep --quiet ubermake.sh; then
            continue
        fi
        [ -z "$BUILD_DIR" ] && BUILD_DIR=`dirname "\`findancestor Makefile .\`"`
        [ -z "$BUILD_DIR" ] && BUILD_DIR=.
        if [ -n "$RTAGS" ]; then
            RTAGS_RMAKE=1 "$i" -C "$BUILD_DIR" -B
            return 0
        fi
        "$i" -C "$BUILD_DIR" $MAKE_OPTIONS #go for the real make
        break
    done

    return $?
}

ALL=
VERSION=
MAKE_DIR=
MAKE_OPTIONS=
RTAGS=
LSDEV_ARGS=
while [ "$#" -gt 0 ]; do
    case $1 in
        -C) shift; MAKE_DIR="$1" ;;
        -C*) MAKE_DIR=`echo $1 | sed 's,^-C,,'` ;;
        --all) ALL="1" ;;
        -r|--rtags) RTAGS="1" ;;
        --verbose) VERBOSE="1" ;;
        -v) VERSION="1"; MAKE_DIR="${PWD}/" ;; #disable lsdev
        -l) shift; LSDEV_ARGS="$LSDEV_ARGS $1" ;;
        -s) shift; SUCCESS_POST_COMMAND="$1" ;;
        -e) shift; ERROR_POST_COMMAND="$1" ;;
        *) MAKE_OPTIONS="$MAKE_OPTIONS $1" ;;
    esac
    shift
done

if [ -z "$MAKE_DIR" ]; then
    NAME=`lsdev.pl -p -ts`
    if [ -n "$NAME" ] && [ -n "$ALL" ]; then
        SOURCE_PATH=`lsdev.pl -r -tp source`
        REST_PATH=`lsdev.pl -p -tr`
        [ "$REST_PATH" = "<root>" ] && REST_PATH=
        if [ -d "$SOURCE_PATH" ]; then
            (cd "$SOURCE_PATH" && lsdev.pl -l -tp -b | while read p; do
                        if [ "$p" != "$SOURCE_PATH" ]; then
                            BUILD_PATH="${p}${REST_PATH}"
                            if [ -d "$BUILD_PATH" ]; then
                                echo
                                echo "============================================================"
                                echo "Building: $BUILD_PATH"
                                echo "============================================================"
                                build "$BUILD_PATH" || exit 1
                            fi
                        fi
                    done; exit $?)
            finish $?
        fi
    fi

    if [ -e "Makefile" ] || [ -e "build.ninja" ] || [ -e "Sakefile.js" ] || [ -e "SConstruct" ]; then
        build "${PWD}/"
    elif [ -n "$NAME" ]; then
        build `lsdev.pl build -tp $LSDEV_ARGS`
    else
        build "${PWD}/"
    fi
else
    build "${MAKE_DIR}/"
fi

finish $?
