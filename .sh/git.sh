
git() #make git checkout commands usable with submodules
{
    if [ "$1" == "--skip-submodule" ]; then
        shift
        command git "$@"
    elif [ "$1" == "clone" ]; then
        command git "$@" --recursive
    elif [ "$1" == "clean" ]; then
        command git "$@" && git submodule foreach --recursive git "$@"
    elif [ "$1" == "pull" ]; then
        command git "$@" && git submodule update --init --recursive
    elif [ "$1" == "checkout" ]; then
        command git "$@" && git submodule update --init --recursive --force
    elif [ "$1" == "reset" ]; then
        if echo "$@" | grep -e "--hard" >/dev/null; then
            command git "$@" && git submodule update --init --recursive --force
        else
            command git "$@" && git submodule update --init --recursive
        fi
    else
        command git "$@"
    fi

}

gs() #sync a tree
{
    ACTION="sync -r"
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
    if [ `lsdev.pl -r -l $LSDEV_FLAGS | wc -l` = "1" ]; then
        git lsdev $LSDEV_FLAGS -- $ACTION
    elif $(git rev-parse --git-dir &> /dev/null); then
        git $ACTION
    fi
}

gss()
{
    gs --status "$@"
}
