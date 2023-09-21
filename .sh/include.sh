#!/usr/bin/env bash

include_sh() {
    src="$1"
    if [ -f "$src" ]; then
        source "$src"
    else
        for DIR in $(echo $SH_DIRECTORY | sed -e 's,:, ,'); do
            sh="${DIR}/${src}"
            if [ -d "$sh" ]; then
                sh="${DIR}/${src}/${src}"
            fi

            if [ ! -f "$sh" ]; then
                if [ -f "${sh}.sh" ]; then
                    sh="${sh}.sh"
                elif [ -n "$BASH" ] && [ -f "${sh}.bash" ]; then
                    sh="${sh}.bash"
                elif [ -n "$ZSH_VERSION" ] && [ -f "${sh}.zsh" ]; then
                    sh="${sh}.zsh"
                else
                    sh=""
                fi
            fi

            if [ -n "$sh" ]; then
                # echo "Including: $src ($sh)"
                source "$sh"
                return
            fi
        done
        echo "NotFound: $src" >&2
    fi
}
