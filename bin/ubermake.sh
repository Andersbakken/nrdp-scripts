#!/bin/bash

SCRIPT_DIR="$(dirname ${BASH_SOURCE[0]} )"

SUCCESS_POST_COMMAND=
ERROR_POST_COMMAND=
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
        *) MAKE_OPTIONS="$MAKE_OPTIONS \"$1\"" ;;
    esac
    shift
done

# TRAP SIGNALS
trap 'cleanup' QUIT EXIT

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

if [ -n "$UBERMAKE_REDUCE_RTAGS_LOAD" ] && [ -x "`which rc`" ]; then
    NUM=$UBERMAKE_REDUCE_RTAGS_LOAD
    if ! echo $NUM | grep --quiet "^[0-9]\+$"; then
        NUM=1
    fi
    rc -j push:$NUM --silent
fi

cleanup()
{
    if [ -n "$UBERMAKE_REDUCE_RTAGS_LOAD" ] && [ -x "`which rc`" ]; then
        rc -j pop --silent
    fi
}

findmake() {
    which -a make | while read i; do
        if [ -L "$i" ] && readlink "$i" | grep --quiet ubermake.sh; then
            continue
        fi
        echo $i
        break
    done
}

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

    CONFIG_STATUS=`findancestor config.status $BUILD_DIR`
    if [ -e "$CONFIG_STATUS" ]; then
        TOOLCHAIN=`grep "Toolchain used: " "$CONFIG_STATUS" | sed -e 's,^.*Toolchain used: \(.*\),\1,'`
        if [ -n "$TOOLCHAIN" ] && [ ! -d "$TOOLCHAIN" ]; then
            echo "Toolchain has been upgraded, rerunning config.status"
            echo "===================================================="
            ( cd `dirname $CONFIG_STATUS` && $CONFIG_STATUS )
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
            NINJA_OPTIONS="-l 1000"
            #NINJA_OPTIONS="$NINJA_OPTIONS -d keeprsp"
            [ "$VERSION" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS --version"
            [ "$VERBOSE" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS -v"
            for opt in $MAKEFLAGS $MAKE_OPTIONS; do
                case $(eval echo $opt) in
                    clean|distclean) NINJA_OPTIONS="$NINJA_OPTIONS -t clean" ;;
                    -k) NINJA_OPTIONS="$NINJA_OPTIONS -k 1000" ;;
                    *) NINJA_OPTIONS="$NINJA_OPTIONS \"$opt\"" ;;
                esac
            done
            NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e "s,-j \([0-9]\+\),-j\1,g"`
            NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e 's,-j ,-j1000 ,g' -e 's,-j\("\),-j1000\1,'`
            # START=`date +%s%N | cut -b1-13`
            NUM="`echo $NINJA_OPTIONS | grep -o -- "-j *[0-9]\+" 2>/dev/null | sed -e 's,-j *,,' | tail -n1`"
            if [ -n "$NUM" ]; then
                CORES=`numcores`
                MAX=$(expr $(expr ${CORES} \* 150) / 100) # 1.5 * $CORES
                if [ "$NUM" -gt "$MAX" ]; then
                    # echo "max is $max num is $num"
                    LINE=`ninja -t commands | grep " -c\>" 2>/dev/null | grep "\.o\>" 2>/dev/null | head -n1`
                    for i in $LINE; do
                        [ ! -e "$i" ] && [ ! -e "`which $i`" ] && continue
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
                            *icecc|*plastc|*fisk*)
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
            eval ninja $NINJA_OPTIONS
            return $?
        fi
    fi

    if [ -x "`which scons`" ] && [ -e "${BUILD_DIR}SConstruct" ]; then
        SCONS_OPTIONS=
        for opt in $MAKEFLAGS $MAKE_OPTIONS; do
            case $(eval echo $opt) in
                clean|distclean) SCONS_OPTIONS="$SCONS_OPTIONS -c" ;;
                *) SCONS_OPTIONS="$SCONS_OPTIONS \"$opt\"" ;;
            esac
        done
        (cd $BUILD_DIR && eval scons $SCONS_OPTIONS)
        return
    elif [ -e "${BUILD_DIR}Sakefile.js" ]; then
        SAKE_OPTIONS=
        for opt in $MAKEFLAGS $MAKE_OPTIONS; do
            case $(eval echo $opt) in
                -j[0-9]*) ;;
                help) SAKE_OPTIONS="$SAKE_OPTIONS -T" ;;
                distclean) SAKE_OPTIONS="$SAKE_OPTIONS clobber" ;;
                *) SAKE_OPTIONS="$SAKE_OPTIONS \"$opt\"" ;;
            esac
        done
        (cd $BUILD_DIR && eval sake $SAKE_OPTIONS)
        return
    fi
    if [ -x `which npm` ]; then
        NPMROOTDIR=$BUILD_DIR
        [ -z "$NPMROOTDIR" ] && NPMROOTDIR=.
        PACKAGEDOTJSON=`findancestor package.json $NPMROOTDIR`
        if [ -f "$PACKAGEDOTJSON" ]; then
            NPMARGS="$MAKE_OPTIONS"
            [ -z "$NPMARGS" ] && NPMARGS="build"

            cd $NPMROOTDIR && eval $SCRIPT_DIR/transform-ts-errors.js npm run $NPMARGS
            return $?
        fi
    fi
    [ "$VERBOSE" = "1" ] && MAKE_OPTIONS="AM_DEFAULT_VERBOSITY=1 $MAKE_OPTIONS"

    MAKE=`findmake`
    if [ -z "$MAKE" ]; then
        echo "Can't find make"
        exit 1
    fi

    [ -z "$BUILD_DIR" ] && BUILD_DIR=`dirname "\`findancestor Makefile .\`"`
    [ -z "$BUILD_DIR" ] && BUILD_DIR=.
    if [ -n "$RTAGS" ]; then
        RTAGS_RMAKE=1 "$i" -C "$BUILD_DIR" -B
        return 0
    fi
    eval "$MAKE" -C "$BUILD_DIR" $MAKE_OPTIONS #go for the real make
    return $?
}

if [ -z "$MAKE_DIR" ]; then
    SOURCE_PATH=""
    NAME=`lsdev.pl -p -ts`
    if [ -n "$NAME" ]; then
        SOURCE_PATH=`lsdev.pl -r -tp source`
        if [ -n "$NAME" ] && [ -n "$ALL" ]; then
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
    fi
    if [ -z "$SOURCE_PATH" ] || [ ! -d "$SOURCE_PATH" ]; then
        SOURCE_PATH="$PWD"
    fi
    if [ -e "${SOURCE_PATH}/Makefile" ] || [ -e "${SOURCE_PATH}/build.ninja" ] || [ -e "${SOURCE_PATH}/Sakefile.js" ] || [ -e "${SOURCE_PATH}/SConstruct" ] || [ -e "${SOURCE_PATH}/package.json" ]; then
        build "${SOURCE_PATH}/"
    elif [ -n "$NAME" ]; then
        build `lsdev.pl build -tp $LSDEV_ARGS`
    else
        build "${PWD}/"
    fi
else
    build "${MAKE_DIR}/"
fi

finish $?
