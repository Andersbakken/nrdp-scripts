#!/bin/bash

p4commits=`git log | grep "git-p4:.*change = [0-9]\+" | wc -l`
othercommits=`git log --pretty=oneline | wc -l`

if [ $p4commits != $othercommits ]; then
    echo "$p4commits p4 commits and $othercommits other commits"
fi
