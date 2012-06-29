#!/bin/sh

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

MAKE=yes
MAKE_DIR="."
MAKE_OPTIONS=
while [ "$#" -gt 0 ]; do
    case $1 in
    -C) shift; MAKE_DIR="$1" ;;
    -C*) MAKE_DIR=`echo $1 | sed 's,^-C,,'` ;;
    *) MAKE_OPTIONS="$MAKE_OPTIONS $1" ;;
    esac
    shift
done
if [ ! -e "${MAKE_DIR}/Makefile" ]; then
   if which ninja >/dev/null 2>&1; then
       NINJA=`findancestor build.ninja $MAKE_DIR`
       if [ -e "$NINJA" ]; then
           NINJA_OPTIONS=
           for opt in $MAKEFLAGS $MAKE_OPTIONS; do
               case $opt in
               clean|distclean) NINJA_OPTIONS="$NINJA_OPTIONS -t clean" ;;
               *) NINJA_OPTIONS="$NINJA_OPTIONS $opt" ;;
               esac
           done
           ninja -C `dirname $NINJA` $NINJA_OPTIONS || return
           MAKE=no
       fi
   fi
fi
[ "$MAKE" = "yes" ] && `which make` -C "$MAKE_DIR" $MAKE_OPTIONS
