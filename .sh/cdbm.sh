#cdbm
cd() { 
   if [ -e "$HOME/bin/cdbm" ]; then
       D=`$HOME/bin/cdbm "$@"` 
       builtin cd "$D"
   else
       builtin cd "$@"
   fi
}
cdl() {
    dir="$1"
    [ -z "$dir" ] && dir="$PWD"
    for d in `ls -lt $dir`; do
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


