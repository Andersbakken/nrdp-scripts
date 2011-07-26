#!/bin/sh

#GIT_PREFIX=git://git-nokia.trolltech.com.au/qtsoftware/
GIT_PREFIX=git@scm.dev.nokia.troll.no:

if ! git remote >/dev/null 2>/dev/null ; then
    echo "$0: not currently in a git repository" 1>&2
    exit 1
fi

CURRENT_REMOTES=`git remote`

function is_existing_remote()
{
    for r in $CURRENT_REMOTES ; do
        if test "x$r" = "x$1" ; then
            return 0
        fi
    done
    return 1
}

function fetch_remote()
{
    REMOTE=$1
    REPO=$GIT_PREFIX$2

    if ! is_existing_remote $REMOTE ; then
        git remote add $REMOTE $REPO 1>&2
    fi
    echo Fetching $REPO to compare histories ... 1>&2
    git fetch $REMOTE 1>&2

    return 0
}

fetch_remote qt qt/qt.git
echo "" 1>&2

# Find all of the branch HEAD's.
BRANCH_LIST=`git branch |sed -e '1,$s/^[ *]*//g'`

qt_45_commit=`git rev-parse qt/4.5`
qt_master_commit=`git rev-parse qt/master`

# Determine the age of all local branches.
for branch in $BRANCH_LIST ; do
    branch_commit=`git rev-parse $branch`
    common_45=`git merge-base $qt_45_commit $branch_commit 2>/dev/null`
    common_master=`git merge-base $qt_master_commit $branch_commit 2>/dev/null`
    common_both=`git merge-base $common_45 $common_master 2>/dev/null`
    age_45=""
    age_master=""
    if test "x$common_45" = "x$qt_45_commit" ; then
        age_45="HEAD"
    else
        if test "x$common_45" != "x" -a "x$common_both" = "x$common_master" ; then
            age_45=`git show --pretty='format:%cr' $common_45 2>/dev/null | head -1`
        fi
    fi
    if test "x$common_master" = "x$qt_master_commit" ; then
        age_master="HEAD"
    else
        if test "x$common_both" != "x$common_master" ; then
            if test "x$common_master" != "x" ; then
                age_master=`git show --pretty='format:%cr' $common_45 2>/dev/null | head -1`
            fi
        fi
    fi
    if test "x$age_45" = "x" ; then
        if test "x$age_master" = "x" ; then
            echo "$branch		"
        else
            echo "$branch	qt/master	\"$age_master\""
        fi
    else
        if test "x$age_master" = "x" ; then
            echo "$branch	qt/4.5	\"$age_45\""
        else
            echo "$branch	qt/master	\"$age_master\""
        fi
    fi
done

exit 0
