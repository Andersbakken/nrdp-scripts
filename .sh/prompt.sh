add_prompt_command()
{
    if echo $PROMPT_COMMAND | grep "$@" >/dev/null 2>&1; then
        true #already did
    elif [ -n "$PROMPT_COMMAND" ]; then
        export PROMPT_COMMAND="$PROMPT_COMMAND; $@"
    else
        export PROMPT_COMMAND="$@"
    fi
}

