#!/bin/sh

if [ -z "$BROWSER" ]; then
   BROWSER=google-chrome
fi

if [ -z "$IMGVIEWER" ] || '!' which "$IMGVIEWER" >/dev/null 2>&1; then
    IMGVIEWER="$BROWSER"
fi

if [ -z "$DEBUGGER" ]; then
  if [ -n "$INSIDE_EMACS" ]; then
     DEBUGGER="emacsclient"
  elif false && which cgdb >/dev/null 2>&1; then
     DEBUGGER=cgdb
  else
     DEBUGGER=gdb
  fi
fi

APP=
mode=
if [ "$1" = "-debug" ]; then
   mode=debug
   shift
elif [ "$1" = "-edebug" ]; then
   mode=edebug
   shift
elif [ "$1" = "-ldd" ]; then
    mode=ldd
    shift
elif [ "$1" = "-rcc" ]; then
   mode=rccdump
   shift
elif [ "$1" = "-browse" ]; then
   mode=browse
   shift
elif [ "$1" = "-open" ]; then
   mode="open"
   shift
elif [ "$1" = "-edit" ] || [ "$1" = "-emacs" ]; then
   mode="edit"
   shift
elif [ "$1" = "-leak" ]; then
   mode=check_leaks
   shift
elif [ "$1" = "-trace" ]; then
   mode="trace"
   shift
fi
APP=
if [ "$#" != 0 ] && [ `echo "${1}" | cut -b1` != '-' ]; then
    if [ -e "${1}" ] || [ "$mode" = "edit" ] || [ "$mode" = "open" ]; then
      APP="${1}"
      shift
    elif "$HOME/bin/emacsedit.sh" -q "${1}"; then
      mode=edit
      APP="${1}"
      shift
    fi
fi
[ -z "$APP" ] && [ -e `basename $PWD` ] && APP=`basename $PWD`
if [ -z "$APP" ]; then
    for a in *; do
       if [ -x "$a" ] && [ ! -d "$a" ]; then
         if [ -z "$APP" ]; then
           APP="$a"
         else
           APP=
           break
         fi
       fi
    done
    if [ -z "$APP" ]; then
       echo "Complete failure"
       return
    fi
fi
exec="$APP"
[ `echo "$exec" | cut -b1` != '/' ] && exec="${PWD}/${exec}"
if [ -d "${exec}" ]; then
    [ -z "$mode" ] && mode=open
elif [ -x "${exec}" ]; then
    true
elif echo "$exec" | grep ".h$" >/dev/null 2>&1 || echo "$exec" | grep ".cpp$" >/dev/null 2>&1 ||
    echo "$exec" | grep ".pro$" >/dev/null 2>&1 || echo "$exec" | grep ".c$" >/dev/null 2>&1; then
    [ -z "$mode" ] && mode=edit
elif echo "$exec" | grep ".html$" >/dev/null 2>&1 || echo "$exec" | grep ".htm$" >/dev/null 2>&1; then
    [ -z "$mode" ] && mode=browse
elif echo "$exec" | grep ".png$" >/dev/null 2>&1 || echo "$exec" | grep ".jpg" >/dev/null 2>&1; then
    [ -z "$mode" ] && mode=image
else
    [ -z "$mode" ] && mode=open
fi
if [ "$mode" = "check_leaks" ]; then
  valgrind $exec "$@"
elif [ "$mode" = "browse" ]; then
  $BROWSER $exec "$@"
elif [ "$mode" = "image" ]; then
  $IMGVIEWER $exec "$@"
elif [ "$mode" = "trace" ]; then
  sudo strace $exec "$@" &
elif [ "$mode" = "debug" ] || [ "$mode" = "edebug" ]; then
  DEBUG_FILE=$PWD/.rez.debug.`whoami`
  rm -f "$DEBUG_FILE"
  env | sed "s,\(.*\)=\(.*\),set env \1 \2,g" >>"$DEBUG_FILE"
  echo "cd $PWD" >>"$DEBUG_FILE"
  if [ "$DEBUGGER" = "emacsclient" ] || [ "$mode" = "edebug" ]; then
      commandline="gdb --annotate=3 -x \\\"$DEBUG_FILE\\\" --args \\\"$exec\\\""
      if [ "$#" -gt 0 ]; then
           for i in "$@"; do commandline="$commandline \\\"$i\\\""; done
      fi
      if which gnuclient >/dev/null 2>&1 && gnuclient -v >/dev/null 2>&1; then
          EMACSCLIENT="gnuclient -q"
      elif which emacsclient >/dev/null 2>&1; then
          EMACSCLIENT="emacsclient -nw -n"
      fi
      if [ -z "$EMACSCLIENT" ]; then
          echo "Unable to find emacs!"
      else
         $EMACSCLIENT -e "(gdb \"$commandline\")"
      fi
  else
      if [ "$#" -gt 0 ]; then
          echo -n "set args" >>"$DEBUG_FILE"
          for i in "$@"; do echo -n " '$i'" >>"$DEBUG_FILE"; done
          echo  >>"$DEBUG_FILE"
      fi
      EDITOR="$HOME/bin/emacsedit.sh -n" $DEBUGGER -x "$DEBUG_FILE" "$exec"
  fi
  #rm -f "$DEBUG_FILE"
elif [ "$mode" = "edit" ] || [ "$mode" = "open" ]; then
  "$HOME/bin/emacsedit.sh" -n "$exec"
elif [ "$mode" = "ldd" ]; then
  ldd "$exec"
else
  $mode "$exec" "$@"
fi
