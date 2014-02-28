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
    if [ "$CDD_FLAGS" = "-r" ] && [ "`lsdev.pl -r -l | wc -l`" != "1" ] && $(git rev-parse --git-dir &> /dev/null); then
        if [ -n "$PULL" ]; then
            git $PULL $PULL_FLAGS
            git submodule update
        fi
        [ -n "$PUSH" ] && git $PUSH $PUSH_FLAGS
    else
        if [ -n "$PULL" ]; then
            git cdd $CDD_FLAGS -- $PULL $PULL_FLAGS
            git cdd $CDD_FLAGS -- submodule update
        fi
        [ -n "$PUSH" ] && git cdd $CDD_FLAGS -- $PUSH $PUSH_FLAGS
    fi
}

gss()
{
    CDD_FLAGS="-r"
    git cdd $CDD_FLAGS -- status $PUSH_FLAGS
}
