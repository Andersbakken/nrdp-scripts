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
if [ -e "${MAKE_DIR}/Makefile" ]; then
    [ "$VERBOSE" = "1" ] && MAKE_OPTIONS="AM_DEFAULT_VERBOSITY=1 $MAKE_OPTIONS"
    true #ok, make it is...
elif [ -e "${MAKE_DIR}/SConstruct" ]; then
    SCONS_OPTIONS=
    for opt in $MAKEFLAGS $MAKE_OPTIONS; do
         case $opt in
         clean|distclean) SCONS_OPTIONS="$SCONS_OPTIONS -c" ;;
         *) SCONS_OPTIONS="$SCONS_OPTIONS $opt" ;;
         esac
    done
    (cd $MAKE_DIR && scons $SCONS_OPTIONS)
    return
elif [ -e "${MAKE_DIR}/Sakefile.js" ]; then
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
   if which ninja >/dev/null 2>&1; then
       NINJA=`findancestor build.ninja $MAKE_DIR`
       if [ -e "$NINJA" ]; then
           NINJA_OPTIONS=
           [ "$VERBOSE" = "1" ] && NINJA_OPTIONS="$NINJA_OPTIONS -v"
           for opt in $MAKEFLAGS $MAKE_OPTIONS; do
               case $opt in
               clean|distclean) NINJA_OPTIONS="$NINJA_OPTIONS -t clean" ;;
               *) NINJA_OPTIONS="$NINJA_OPTIONS $opt" ;;
               esac
           done
           ninja -C `dirname $NINJA` $NINJA_OPTIONS
           return
       fi
   fi
fi
`which make` -C "$MAKE_DIR" $MAKE_OPTIONS #go for the real make
