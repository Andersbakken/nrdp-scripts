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

cdo() {
   eval DIR="$1"
   [ -n "$D" ] && builtin cd "$DIR"
}

cdbm_cd_command() {
    builtin cd "$1"
}

test -z "$CDBM_CD_COMMAND" && export CDBM_CD_COMMAND="cdbm_cd_command"

cd() {
    D=`cdbm "$@"`
    [ -n "$D" ] && "$CDBM_CD_COMMAND" "$D" "$@"
}
cdl() {
    D="`__cdl_helper $1`"
    [ -n "$D" ] && test -d "$D" && cd "$D"
}
