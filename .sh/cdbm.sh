#cdbm
cd() {
    D=`cdbm "$@"`
    builtin cd "$D"
}
cdl() {
    dir="$1"
    [ -z "$dir" ] && dir="$PWD"
    for d in `ls -t $dir`; do
        if [ -d "$d" ]; then
            cd "$d"
            break
        fi
    done
}
cdo() {
   eval DIR="$1"
   builtin cd "$DIR"
}


