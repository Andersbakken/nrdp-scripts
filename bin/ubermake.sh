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

finish() {
    if [ "$1" -eq 0 ]; then
        [ -n "$SUCCESS_POST_COMMAND" ] && eval "$SUCCESS_POST_COMMAND"
    else
        [ -n "$ERROR_POST_COMMAND" ] && eval "$ERROR_POST_COMMAND"
    fi
    exit "$1"
}

VERSION=
MAKE_DIR=
MAKE_OPTIONS=
UBERTAGS=
LSDEV_ARGS=
while [ "$#" -gt 0 ]; do
    case $1 in
        -C) shift; MAKE_DIR="$1" ;;
        -C*) MAKE_DIR=`echo $1 | sed 's,^-C,,'` ;;
        -r|--rtags) UBERTAGS=1 ;;
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
    if [ -e "Makefile" ] || [ -e "build.ninja" ] || [ -e "Sakefile.js" ] || [ -e "SConstruct" ]; then
        MAKE_DIR="${PWD}/"
    else
        NAME=`lsdev.pl -p -ts`
        if [ -n "$NAME" ]; then
            MAKE_DIR=`lsdev.pl build -tp $LSDEV_ARGS`
        else
            MAKE_DIR="${PWD}/"
        fi
    fi
fi

echo $MAKE_DIR | grep --quiet "/$" || MAKE_DIR="${MAKE_DIR}/"
if [ -e "${MAKE_DIR}Makefile" ]; then
    [ "$VERBOSE" = "1" ] && MAKE_OPTIONS="AM_DEFAULT_VERBOSITY=1 $MAKE_OPTIONS"
    true #ok, make it is...
elif [ -x "`which scons`" ] && [ -e "${MAKE_DIR}SConstruct" ]; then
    SCONS_OPTIONS=
    for opt in $MAKEFLAGS $MAKE_OPTIONS; do
        case $opt in
            clean|distclean) SCONS_OPTIONS="$SCONS_OPTIONS -c" ;;
            *) SCONS_OPTIONS="$SCONS_OPTIONS $opt" ;;
        esac
    done
    (cd $MAKE_DIR && scons $SCONS_OPTIONS)
    return
elif [ -e "${MAKE_DIR}Sakefile.js" ]; then
    SAKE_OPTIONS=
    for opt in $MAKEFLAGS $MAKE_OPTIONS; do
        case $opt in
            -j[0-9]*) ;;
            help) SAKE_OPTIONS="$SAKE_OPTIONS -T" ;;
            distclean) SAKE_OPTIONS="$SAKE_OPTIONS clobber" ;;
            *) SAKE_OPTIONS="$SAKE_OPTIONS $opt" ;;
        esac
    done
    (cd $MAKE_DIR && sake $SAKE_OPTIONS)
    return
else
    if [ -n "$UBERTAGS" ]; then
        DIR="$MAKE_DIR"
        [ -z "$DIR" ] && DIR=.
        COMPILATION_DATABASEJSON=`findancestor compile_commands.json $DIR`
        # echo "FOUND IT $COMPILATION_DATABASEJSON"
        if [ -e "$COMPILATION_DATABASEJSON" ]; then
            rc -J "$COMPILATION_DATABASEJSON"
            finish 0
        fi
    fi
    if which ninja >/dev/null 2>&1; then
        NINJA_DIR=$MAKE_DIR
        [ -z "$NINJA_DIR" ] && NINJA_DIR=.
        NINJA=`findancestor build.ninja $NINJA_DIR`
        if [ -e "$NINJA" ]; then
            cd `dirname $NINJA`
            if [ -n "$UBERTAGS" ]; then
                ninja -t commands | rc --compile
                finish 0
            fi
            NINJA_OPTIONS=
            [ "$VERSION" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS --version"
            [ "$VERBOSE" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS -v"
            for opt in $MAKEFLAGS $MAKE_OPTIONS; do
                case $opt in
                    clean|distclean) NINJA_OPTIONS="$NINJA_OPTIONS -t clean" ;;
                    *) NINJA_OPTIONS="$NINJA_OPTIONS $opt" ;;
                esac
            done
            NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e 's,-j ,-j1000 ,g' -e 's,-j$,-j1000,'`
            ninja $NINJA_OPTIONS
            finish $?
        fi
    fi
fi
[ -z "$MAKE_DIR" ] && MAKE_DIR=`dirname "\`findancestor Makefile .\`"`
[ -z "$MAKE_DIR" ] && MAKE_DIR=.
if [ -n "$UBERTAGS" ]; then
    RTAGS_RMAKE=1 `which make` -C "$MAKE_DIR" -B
    finish 0
fi
`which make` -C "$MAKE_DIR" $MAKE_OPTIONS #go for the real make
finish $?
