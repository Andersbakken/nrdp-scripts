add_lsdev() {
  DIR=
  NAME=
  FORCE=no
  ACTION=add
  FILE=
  while [ "$#" -gt 0 ]; do
     case "$1" in
     -file) shift; FILE="$1" ;;
     -f) FORCE=yes ;;
     -d) ACTION=del ;;
     *)
        if [ -z "$DIR" ]; then
            DIR="$1"
            echo "$DIR" | grep -v -q "^/" && DIR="$PWD/$DIR"
        elif [ -z "$NAME" ]; then
            NAME="$1"
        fi
        ;;
     esac
     shift
  done
  if [ -z "$FILE" ]; then
      echo "No file specified!"
      return 1
  fi
  if echo "$DIR" | grep '^-' >/dev/null 2>&1; then
      ACTION=del
      DIR=`echo "$DIR" | sed 's,^-,,'`
      NAME="$DIR"
  fi
  [ -n "$DIR" ] && [ -z "$NAME" ] && NAME=`basename $DIR`

  if [ "$ACTION" = "del" ]; then
      if [ -z "$FILE" ]; then
          echo "No shadows."
      elif grep "^${DIR}=" "$FILE" >/dev/null 2>&1; then
          echo "Removed. ${FILE}"
          sed -e "s,^${DIR}=.*\$,," -e "/^$/d" -i "$FILE"
      elif grep "^.*=${DIR}\$" "$FILE" >/dev/null 2>&1; then
          echo "Removed. ${FILE}"
          sed -e "s,^.*=${DIR}\$,," -e "/^$/d" -i "$FILE"
      else
          echo "Not found. ${FILE}"
      fi
  else
      if [ -z "$DIR" ] || [ ! -d "$DIR" ]; then
          echo "adddev dir [name]"
          return 1
      fi
      if grep "^${NAME}=$DIR\$" "$FILE" >/dev/null 2>&1; then
          echo "Unchanged. ($FILE)"
      elif grep "^${NAME}=" "$FILE" >/dev/null 2>&1; then
          echo "Changed. ($FILE)"
          sed -e "s,^${NAME}=\.*$,$NAME=$DIR," -i "$FILE"
      else
          echo "Added. ($FILE)"
          echo "${NAME}=${DIR}" >>"$FILE"
      fi
  fi
}

adddev() {
  FILE="$HOME/.dev_directories"
  add_lsdev -file "$FILE" "$@"
}

addbuild() {
  FILE=`findancestor .shadows`
  [ -z "$FILE" ] && FILE="$PWD/.shadows"
  add_lsdev -file "$FILE" "$@"
}

#lsdev
lsdev() {
   if [ "$1" = "-e" ]; then
       shift
       EXECUTE="$1"
       shift
       LSDEV_PATH=`lsdev.pl -tp "$@"`
       if [ -n "$LSDEV_PATH" ]; then
           if echo "$EXECUTE" | grep -q "{}"; then
               EXECUTE=`echo $EXECUTE | sed "s,{},$LSDEV_PATH,g"`
           else
               E=`echo $EXECUTE | cut -d' ' -f1`
               if [ -x "$LSDEV_PATH/$E" ]; then
                   EXECUTE="$LSDEV_PATH/$EXECUTE"
               else
                   EXECUTE="$EXECUTE $LSDEV_PATH"
               fi
           fi
           echo "Executing: $EXECUTE" >&2
           eval $EXECUTE
       fi
   else
       lsdev.pl "$@"
   fi
}
lsd() {
   OPTS="$@"
   [ -z "$OPTS" ] && OPTS="-"
   DIR=`lsdev -l $OPTS`
   echo "$DIR"
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

lsdevsetenvs() {
    eval `lsdev -a -l -ta | awk '{ var=$1; val=$2; gsub(/[._-]+/, "_", var); print toupper(var),val }' | sed -e "s,^,$1," -e 's, \[,=,' -e 's,\]$,,'`
}

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
alias wdd="whatdev -ta"

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
    if [ "$#" = 1 ] && echo "$1" | grep '^@' >/dev/null 2>&1; then
        MATCH=`echo $1 | sed "s,^@,,"`
        DIR=`emacsclient -e "(sam-what-directory \"$MATCH\")" | sed 's,"\(.*\)",\1,'`
    else
        DIR=`lsdev -w $@`
    fi
    [ -z "$DIR" ] && [ "$#" = "1" ] && [ -d "$1" ] && DIR="$1"
    if [ -d "$DIR" ] && [ -n "$DIR" ]; then
      echo "$DIR"
      cd "$DIR" && return 0
    fi
    echo "Not found."
    return 1
}
alias cdd=cddev
alias cdds="cdd src"
alias cddb="cdd build"
alias cds=cdds
alias cdb=cddb

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
    if [ "${COMP_WORDS[0]}" = "cdds" ] || [ "${COMP_WORDS[0]}" = "cds" ]; then
        nondirs=(${nondirs[@]} src)
    elif [ "${COMP_WORDS[0]}" = "cddb" ] || [ "${COMP_WORDS[0]}" = "cdb" ]; then
        nondirs=(${nondirs[@]} build)
    elif [ "${COMP_WORDS[0]}" = "lsdev" ]; then
        [ "${COMP_WORDS[$idx]}" = "-e" ] && idx=3
    fi

    while [ "$idx" -lt "${#COMP_WORDS[@]}" ]; do
        if [ "$idx" -ne "$COMP_CWORD" ]; then
            local arg="${COMP_WORDS[${idx}]}"
            if echo "$arg" | grep --quiet /; then
                test -z "$cur" && return
            elif test -n "$arg"; then
                nondirs=(${nondirs[@]} $arg)
            fi
        fi
        idx=$((idx + 1))
    done
    local realdir
    if [ "${#nondirs[@]}" -ge 0 -a -z "$cur" ]; then
        tmp=`lsdev -tp -a -l ${nondirs[@]} 2>/dev/null`
        [ `echo "$tmp" | wc -w` == 1 ] && realdir="$tmp"
    elif echo "$cur" | grep --quiet "/"; then
        realdir=`lsdev -tp -a -l ${nondirs[@]} 2>/dev/null`
        [ `echo "$realdir" | wc -w` != 1 ] && return
    fi

    if [ -n "$realdir" ]; then
        COMPREPLY=(`compgen -d $realdir/$cur | sed -e 's,//*,/,g' -e "s,^$realdir,," -e 's,/*$,/,'`)
        if [ ${#COMPREPLY[@]} = 1 ]; then
            COMPREPLY=(${COMPREPLY[@]} ${COMPREPLY}non-existing-dir)
        fi
    else
        COMPREPLY=()
        local words=`for a in $(lsdev -tn -a -l ${nondirs[@]} 2>&1 | sed -e 's,[^A-Za-z0-9.-][^A-Za-z0-9.-]*, ,g'); do echo $a; done | sort -u | xargs`
        idx=0
        while [ "$idx" -lt "${#nondirs[@]}" ]; do
            words=`echo $words | sed "s,\<${nondirs[${idx}]}\>,,g"`
            idx=$((idx + 1))
        done
        local subwords
        [ `echo $cur | wc -c` -gt 2 ] && subwords=`for a in $words; do echo $a; done | grep -o "${cur}[A-Za-z0-9.]*$"`
        COMPREPLY=($(compgen -W "${words} ${subwords}" -- ${cur}))
        #[ "${#COMPREPLY[@]}" = 0 ] && COMPREPLY=(${words})
    fi
}

if [ -n "$BASH" ]; then
    complete -F complete-cddev cdd cddev mdd makedev editdev edd cds cdds cddb cdb lsdev
fi
