#!/bin/bash

[[ $(type -P "gpg2") ]] && GPG="gpg2" || GPG="gpg"

while [ -n "`ps aux | grep "\<gpg[^-]" | grep -v grep`" ]; do # apparently pidof gpg returns pid for gpg-agent
    sleep 1
done

PASSWORD=`${GPG} --no-tty -d "$1" 2>/dev/null | tail -n 1`
echo -n "$PASSWORD"
