adddev() {
  DIR=
  NAME=
  FORCE=no
  ACTION=add
  while [ "$#" -gt 0 ]; do
     case "$1" in
     -f) FORCE=yes ;;
     -d) ACTION=del ;;
     *)
        if [ -z "$DIR" ]; then
            DIR="$1"
        elif [ -z "$NAME" ]; then
            NAME="$1"
        fi
        ;;
     esac
     shift
  done
  if echo "$DIR" | grep '^-' >/dev/null 2>&1; then
      ACTION=del
      DIR=`echo "$DIR" | sed 's,^-,,'`
      NAME="$DIR"
  fi
  [ -n "$DIR" ] && [ -z "$NAME" ] && NAME=`basename $DIR`

  if [ "$FORCE" != "yes" ]; then
      CONFIG_FILE=`findancestor config.status`
      if [ -n "$CONFIG_FILE" ]; then
          echo "In build tree. ($CONFIG_FILE)"
          return 1
      fi
  fi

  SHADOWS_FILE=`findancestor .shadows`
  if [ "$ACTION" = "del" ]; then
      if [ -z "$SHADOWS_FILE" ]; then
          echo "No shadows."
      elif grep "^${DIR}=" "$SHADOWS_FILE" >/dev/null 2>&1; then
          echo "Removed. ${SHADOWS_FILE}"
          sed -e "s,^${DIR}=.*\$,," -e "/^$/d" -i "$SHADOWS_FILE"
      elif grep "^.*=${DIR}\$" "$SHADOWS_FILE" >/dev/null 2>&1; then
          echo "Removed. ${SHADOWS_FILE}"
          sed -e "s,^.*=${DIR}\$,," -e "/^$/d" -i "$SHADOWS_FILE"
      else
          echo "Not found. ${SHADOWS_FILE}"
      fi
  else
      if [ -z "$DIR" ] || [ ! -d "$DIR" ]; then
          echo "adddev dir [name]"
          return 1
      fi
      [ -z "$SHADOWS_FILE" ] && SHADOWS_FILE="$PWD/.shadows"
      if grep "^${NAME}=$DIR\$" "$SHADOWS_FILE" >/dev/null 2>&1; then
          echo "Unchanged. ($SHADOWS_FILE)"
      elif grep "^${NAME}=" "$SHADOWS_FILE" >/dev/null 2>&1; then
          echo "Changed. ($SHADOWS_FILE)"
          sed -e "s,^${NAME}=\.*$,$NAME=$DIR," -i "$SHADOWS_FILE"
      else
          echo "Added. ($SHADOWS_FILE)"
          echo "${NAME}=${DIR}" >>"$SHADOWS_FILE"
      fi
  fi
}

#lsdev
lsdev() {
   "lsdev.pl" "$@"
}
lsd() {
   OPTS="$@"
   [ -z "$OPTS" ] && OPTS="-"
   DIR=`lsdev -l $OPTS`
   if [ -n "$DIR" ]; then
      echo "$DIR"
      ls "$DIR"
   fi
}

editdev() {
    EDIT=edit
    if [ "$1" = "-vi" ]; then
       EDIT=vi
       shift
    fi
    FILE="$1"
    if [ -z "$FILE" ]; then
        echo "Must specify file to edit"
        return 1
    fi
    shift
    if [ ! -e "$FILE" ]; then
       DIR="-"
       if echo "$FILE" | grep '^%' >/dev/null 2>&1; then
          DIR=`echo "$FILE" | sed "s,^\(%.*/\).*,\1,g"`
          FILE=`echo "$FILE" | sed "s,^$DIR,,"`
       fi
       DIR=`lsdev $DIR`
       FILE="$DIR/$FILE"
    fi
    if [ -e "$FILE" ]; then
        $EDIT $@ $FILE
    else
        echo "Unable to $EDIT [$FILE]!"
    fi
    return 0
}
alias edd=editdev

whatdev() {
    NAME=`lsdev -p $@`
    export WHATDEV_LAST_ARGS="x${@}x${PWD}"
    export WHATDEV_LAST_NAME="$NAME"

    if [ -n "$NAME" ]; then
      echo "$NAME"
    else
      echo "$PWD: Not a tree" >&2
      return 1;
    fi
    return 0
}
alias wdd=whatdev

setdev() {
    DIR=`lsdev -w $@`
    if [ -n "$DIR" ]; then
      echo "Set: $DIR"
      return 0
    fi
    echo "Not set."
    return 1
}
cddev() {
    DIR=`lsdev -w $@`
    [ -z "$DIR" ] && [ "$#" = "1" ] && [ -d "$1" ] && DIR="$1"
    if [ -n "$DIR" ]; then
      echo "$DIR"
      cd "$DIR" && return 0
    fi
    echo "Not found."
    return 1
}
alias cdd=cddev

makedev() {
    DIR=`lsdev -w $@`
    if [ -n "$DIR" ]; then
      echo "$DIR"
      make -C "$DIR" && return 1
    else
      echo "Not found."
      return 1
    fi
}
alias mdd=makedev

complete-cddev ()
{
    local cur="${COMP_WORDS[$COMP_CWORD]}"
    COMPREPLY=()
    local nondirs=()
    local idx=1
    while [ $idx -lt ${#COMP_WORDS[@]} ]; do
        local arg="${COMP_WORDS[${idx}]}"
        if echo "$arg" | grep --quiet /; then
            test -z "$cur" && return
        elif test -n "$arg"; then
            nondirs=(${nondirs[@]} $arg)
        fi
        idx=$((idx + 1))
    done
    local realdir
    if [ "${#nondirs[@]}" -ge 0 -a -z "$cur" ]; then
        realdir=`lsdev -tp -a -l ${nondirs[@]} 2>/dev/null`
        if [ `echo "$realdir" | wc -l` != 1 ]; then 
            realdir=
        fi
    elif echo "$cur" | grep --quiet /; then
        realdir=`lsdev -tp -a -l ${nondirs[@]} 2>/dev/null`
        if [ `echo "$realdir" | wc -l` != 1 ]; then 
            return
        fi
    fi

    if [ -n "$realdir" ]; then
        COMPREPLY=(`compgen -d $realdir/$cur | sed -e 's,//*,/,g' -e "s,^$realdir,," -e 's,/*$,/,'`)
        if [ ${#COMPREPLY[@]} = 1 ]; then
            COMPREPLY=(${COMPREPLY[@]} ${COMPREPLY}non-existing-dir)
        fi
    else
        local matches=`lsdev -tn -a -l ${nondirs[@]} 2>&1`
        if [ -z "$matches" ]; then
            return;
        elif [ "$matches" != "$cur" ]; then
            COMPREPLY=(${matches[@]})
        fi
    fi
}

complete -F complete-cddev cdd cddev mdd makedev editdev edd
