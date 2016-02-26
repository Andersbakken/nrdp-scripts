
git() #make git checkout commands usable with submodules
{
    if [[ $@ == clone* ]]; then
        gitargs=$(echo "$@" | cut -c6-)
        command git clone --recursive $gitargs
    elif [[ $@ == pull* ]]; then
        command git "$@" && git submodule update --init --recursive
    elif [[ $@ == checkout* ]]; then
        command git "$@" && git submodule update --init --recursive
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
