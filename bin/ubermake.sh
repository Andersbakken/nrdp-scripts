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
                NINJA_OPTIONS=`echo $NINJA_OPTIONS | sed -e 's,-j ,-j1000 ,g' -e 's,-j$,-j1000,'`
                ninja $NINJA_OPTIONS
                return $?
            fi
        fi
    fi
    [ -z "$BUILD_DIR" ] && BUILD_DIR=`dirname "\`findancestor Makefile .\`"`
    [ -z "$BUILD_DIR" ] && BUILD_DIR=.
    if [ -n "$RTAGS" ]; then
        RTAGS_RMAKE=1 `which make` -C "$BUILD_DIR" -B
        return 0
    fi
    `which make` -C "$BUILD_DIR" $MAKE_OPTIONS #go for the real make
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
        SOURCE_PATH=`lsdev.pl -tp source`
        if [ -d "$SOURCE_PATH" ]; then
            (cd "$SOURCE_PATH" && lsdev.pl -l -tp -b) | while read path; do
                if [ "$path" != "$SOURCE_PATH" ]; then
                    echo -e "=======================\nBuilding $path\n==================\n"
                    build "$path"
                fi
            done
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
