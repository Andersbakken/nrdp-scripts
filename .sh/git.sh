#sync a tree
gs()
{
    CDD_FLAGS="-r"
    PULL_FLAGS="--autostash"
    PUSH_FLAGS=
    PULL=sync
    PUSH=
    while [ "$#" -gt 0 ]; do
      case "$1" in
      -f) PUSH_FLAGS="$PUSH_FLAGS -f" ;;
      -push) PUSH=push ;;
      -pushf) PUSH=push; PUSH_FLAGS="$PUSH_FLAGS -f" ;;
      -pull) PULL=pull ;;
      -sync) PULL=sync ;;
      *) CDD_FLAGS="$CDD_FLAGS $1" ;;
      esac
      shift
    done
    [ -n "$PULL" ] && git cdd $CDD_FLAGS -- $PULL $PULL_FLAGS
    [ -n "$PUSH" ] && git cdd $CDD_FLAGS -- $PUSH $PUSH_FLAGS
}

gss()
{
    CDD_FLAGS="-r"
    git cdd $CDD_FLAGS -- status $PUSH_FLAGS
}
