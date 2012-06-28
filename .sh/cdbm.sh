#cdbm

__cdl_helper()
{
    dir="$1"
    [ -z "$dir" ] && dir="$PWD"

    ls -t "$dir" | while read i; do
        builtin cd "$i" &>/dev/null
        if [ "$?" -eq 0 ]; then
            echo "$i"
            return
        fi
    done
}

cd() {
    D=`cdbm "$@"`
    builtin cd "$D"
}
cdl() {
    dir="`__cdl_helper $1`"
    test -d "$dir" && cd "$dir"
}
cdo() {
   eval DIR="$1"
   builtin cd "$DIR"
}


