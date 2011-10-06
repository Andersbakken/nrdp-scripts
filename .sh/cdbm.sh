#cdbm
cd() { 
   if [ -e "$HOME/bin/cdbm" ]; then
       D=`$HOME/bin/cdbm "$@"` 
       builtin cd "$D"
   else
       builtin cd "$@"
   fi
}
cdo() {
   eval DIR="$1"
   builtin cd "$DIR"
}


