#sync a tree
gs()
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
