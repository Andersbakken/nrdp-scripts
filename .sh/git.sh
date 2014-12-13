#sync a tree
gs()
{
    ACTION="sync -r"
    CDD_FLAGS="src "
    while [ "$#" -gt 0 ]; do
      case "$1" in
      --status) ACTION="status" ;;
      --push) shift; ACTION="push $1" ;;
      --pushf) shift; ACTION="push -f $1" ;;
      *) CDD_FLAGS="$CDD_FLAGS $1" ;;
      esac
      shift
    done
    if [ "`lsdev.pl -r -l $CDD_FLAGS | wc -l`" != "1" ] && $(git rev-parse --git-dir &> /dev/null); then
        git $ACTION
    else
        git cdd $CDD_FLAGS -- $ACTION
    fi
}

gss()
{
    gs --status "$@"
}
