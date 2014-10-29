#sync a tree
gs()
{
    if [ "`lsdev.pl -r -l | wc -l`" != "1" ] && $(git rev-parse --git-dir &> /dev/null); then
        git sync -r --autostash
    else
        git cdd -- sync -r --autostash
    fi
}

gp4_sync()
{
   P4CHANGE=`git p4 change HEAD | awk '{print $4}'`
   echo "Syncing to: $P4CHANGE"
   for m in $@; do
       (cd $m; GITCHANGE=`git p4 change $P4CHANGE | awk '{print $4}'`; echo "Sync $m @ $GITCHANGE ($P4CHANGE)"; git checkout "$GITCHANGE")
   done
}

gss()
{
    CDD_FLAGS="-r"
    git cdd $CDD_FLAGS -- status $PUSH_FLAGS
}
