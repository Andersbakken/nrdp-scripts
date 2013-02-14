#!/bin/sh
tmux list-windows | grep '(active)' | sed 's,^[0-9]*: \(.*\)\* .*$,\1,'