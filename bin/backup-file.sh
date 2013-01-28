#!/bin/bash

file=`readlink -f $1`
shift

if [ ! -r "$file" ]; then
    echo "Can't open $file for reading"
    exit 1
fi

dir="$HOME/.backups"
if [ ! -d "$dir/.git" ]; then
    mkdir "$dir"
    cd "$dir"
    git init > /dev/null
else
    cd "$dir"
fi
dir=`readlink -f $dir`
if echo "$file" | grep -q "$dir"; then
    exit 0
fi

path="${dir}${file}"
mkdir -p `dirname $path`
cat "$file" > "$path"
git add "$path" >/dev/null
git commit -m "Update $file $@" > /dev/null
