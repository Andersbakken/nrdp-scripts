
run_git()
{
    if $(command git rev-parse --git-dir &> /dev/null); then
        command git "$@"
    else
        SRC=$(lsdev.pl -l -tp)
        if [ $(echo $SRC | wc -l) = "1" ]; then
            (cd "$SRC" && command git "$@")
        else
            command git "$@"
        fi
    fi
}

git() #make git checkout commands usable with submodules
{
    if [ "$1" = "--version" ] || [ "$1" = "--help" ] || [ "$1" = "init" ]; then
        command git $1
        shift
    elif [ "$1" = "--skip-submodule" ]; then
        shift
        run_git "$@"
    elif [ "$1" = "clone" ]; then
        run_git "$@" --recursive
    elif [ "$1" = "clean" ]; then
        run_git "$@" && run_git submodule foreach --recursive git "$@"
    elif [ "$1" = "status" ]; then
        run_git submodule foreach --quiet --recursive git "$@" --porcelain
        run_git "$@"
    elif [ "$1" = "describe" ]; then
        run_git submodule status
        run_git "$@"
    elif [ "$1" = "pull" ]; then
        run_git "$@" && run_git submodule update --init --recursive
    elif [ "$1" = "merge" ]; then
        run_git "$@" && run_git submodule update --init --recursive
    elif [ "$1" = "fetch" ]; then
        run_git "$@" && run_git submodule foreach git fetch --tags
    elif [ "$1" = "checkout" ]; then
        if echo "$@" | grep -e "--force" >/dev/null || echo "$@" | grep -e "-f" >/dev/null; then
            run_git "$@" && run_git submodule update --init --recursive --force
        else
            run_git "$@" && run_git submodule update --init --recursive
        fi
    elif [ "$1" = "reset" ]; then
        if echo "$@" | grep -e "--hard" >/dev/null; then
            run_git "$@" && run_git submodule update --init --recursive --force
        else
            run_git "$@" && run_git submodule update --init --recursive
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
    LSDEV_FLAGS="src "
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --status) ACTION="status" ;;
            --push) shift; ACTION="push $1" ;;
            --pushf) shift; ACTION="push -f $1" ;;
            *) LSDEV_FLAGS="$LSDEV_FLAGS $1" ;;
        esac
        shift
    done
    SRC=$(lsdev.pl -r -l -tp $LSDEV_FLAGS)
    if [ $(echo $SRC | wc -l) = "1" ]; then
        (cd "$SRC" && command git "$ACTION")
    elif $(git rev-parse --git-dir &> /dev/null); then
        command git $ACTION
    fi
}
