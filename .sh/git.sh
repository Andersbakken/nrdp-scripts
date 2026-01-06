#!/usr/bin/env bash

export FILTER_BRANCH_SQUELCH_WARNING=1.

run_git()
{
    # Parse git switches (like -C) before the command
    local git_switches=()
    local cmd=""
    local cmd_args=()
    local parsing_switches=1
    local need_value=""

    for arg in "$@"; do
        if [ -n "$parsing_switches" ]; then
            # If previous switch needs a value, consume this arg
            if [ -n "$need_value" ]; then
                git_switches+=("$arg")
                need_value=""
                continue
            fi

            case "$arg" in
                # Switches that take a separate argument
                -C|-c|--git-dir|--work-tree|--namespace|--super-prefix|--config-env|--exec-path|--attr-source|--list-cmds)
                    git_switches+=("$arg")
                    need_value=1
                    ;;
                # Switches with = that include their value
                -C=*|-c=*|--git-dir=*|--work-tree=*|--namespace=*|--super-prefix=*|--config-env=*|--exec-path=*|--attr-source=*|--list-cmds=*)
                    git_switches+=("$arg")
                    ;;
                # Boolean/standalone switches
                -v|--version|-h|--help|-p|--paginate|-P|--no-pager|--no-replace-objects|--bare|--no-optional-locks|--no-advice|--html-path|--man-path|--info-path|--glob-pathspecs|--noglob-pathspecs|--literal-pathspecs|--icase-pathspecs)
                    git_switches+=("$arg")
                    ;;
                # Unknown switch - could be a global option we don't know about
                -*)
                    git_switches+=("$arg")
                    ;;
                # Not a switch - this is the command
                *)
                    cmd="$arg"
                    parsing_switches=""
                    ;;
            esac
        else
            cmd_args+=("$arg")
        fi
    done

    # Check if this command has operation config enabled
    local use_operation=""
    if [ -n "$cmd" ] && [ "$cmd" != "operation" ]; then
        use_operation=$(command git "${git_switches[@]}" config "${cmd}.operation" 2>/dev/null)
    fi

    # Run the command, optionally with operation save/pop
    local run_cmd
    if $(command git "${git_switches[@]}" rev-parse --git-dir &> /dev/null); then
        run_cmd=(command git "${git_switches[@]}")
    else
        SRC=$(lsdev.pl -l -tS -p)
        if [ -n "$SRC" ]; then
            run_cmd=(command git -C "$SRC" "${git_switches[@]}")
        else
            run_cmd=(command git "${git_switches[@]}")
        fi
    fi

    if [ "$use_operation" = "true" ]; then
        "${run_cmd[@]}" operation save --pending
        "${run_cmd[@]}" "$cmd" "${cmd_args[@]}"
        local RC=$?
        "${run_cmd[@]}" operation pop --pending
        return $RC
    else
        "${run_cmd[@]}" "$cmd" "${cmd_args[@]}"
    fi
}

git_is_worktree()
{
    if [ $(run_git rev-parse --is-inside-work-tree) == "true" ]; then
        return 0
    fi
    return 1
}

git()
{
    if [ "$1" = "--version" ] || [ "$1" = "--help" ] || [ "$1" = "init" ]; then
        command git $1
        shift
    elif [ "$1" = "--skip-submodule" ]; then
        shift
        run_git "$@"
    elif [ "$1" = "reflog" ]; then
        run_git "$@" --date=relative
    elif [ "$1" = "clone" ]; then
        run_git "$@" --recursive
    elif [ "$1" = "clean" ]; then
        run_git "$@" && git_is_worktree && run_git submodule foreach --recursive git "$@"
    elif [ "$1" = "status" ]; then
        git_is_worktree && run_git submodule foreach --quiet --recursive git "$@" --porcelain
        run_git "$@"
    elif [ "$1" = "describe" ]; then
        git_is_worktree && run_git submodule status
        run_git "$@"
    elif [ "$1" = "pull" ]; then
        run_git "$@" && git_is_worktree && run_git submodule update --init --recursive
    elif [ "$1" = "merge" ]; then
        run_git "$@" && git_is_worktree && run_git submodule update --init --recursive
    elif [ "$1" = "fetch" ]; then
        run_git "$@" && git_is_worktree && run_git submodule foreach git fetch --tags
    elif [ "$1" = "checkout" ]; then
        if echo "$@" | grep -e "--force" >/dev/null || echo "$@" | grep -e "-f" >/dev/null; then
            run_git "$@" && git_is_worktree && run_git submodule update --init --recursive --force
        else
            run_git "$@" && git_is_worktree && run_git submodule update --init --recursive
        fi
    elif [ "$1" = "reset" ]; then
        if echo "$@" | grep -e "--hard" >/dev/null; then
            run_git "$@" && git_is_worktree && run_git submodule update --init --recursive --force
        else
            run_git "$@" && git_is_worktree && run_git submodule update --init --recursive
        fi
    else
        run_git "$@"
    fi
}

update_current_pwd()
{
    echo "$PWD" >$HOME/.current-pwd
}
add_prompt_command "update_current_pwd"

lsdev_git_sync() #sync a tree
{
    ACTION="sync"
    LSDEV_FLAGS="-l -tS -p"
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --status) ACTION="status" ;;
            --push) shift; ACTION="push $1" ;;
            --pushf) shift; ACTION="push -f $1" ;;
            *) LSDEV_FLAGS="src $1" ;;
        esac
        shift
    done
    SRC=$(lsdev.pl $LSDEV_FLAGS)
    if [ -n "$SRC" ]; then
        (cd "$SRC" && command git "$ACTION")
    elif $(git rev-parse --git-dir &> /dev/null); then
        command git $ACTION
    else
        echo "No dev dirs found" >&2
    fi
}
