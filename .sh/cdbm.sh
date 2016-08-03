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
    [ -n "$D" ] && builtin cd "$D"
}
cdl() {
    D="`__cdl_helper $1`"
    [ -n "$D" ] && test -d "$D" && cd "$D"
}
cdo() {
   eval DIR="$1"
   [ -n "$D" ] && builtin cd "$DIR"
}


