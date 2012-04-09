#!/bin/sh

#printf "\033k$1\033\\"
if [ "$1" = "emacs" ]; then
   shift
   /home/smagnuson/bin/emacsedit.sh "$@"
else
   "$@"
fi
