#!/bin/bash

while [ -n "`ps aux | grep "\<gpg[^-]" | grep -v grep`" ]; do # apparently pidof gpg returns pid for gpg-agent
    sleep 1
done

password=`gpg --no-tty -d "$1" | tail -n 1`
echo -n "$password"
