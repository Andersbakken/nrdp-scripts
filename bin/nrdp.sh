#!/bin/bash

version=main
output=netflix

while [ -n "$1" ]; do
    case "$1" in
        -h|--help)
            echo "$0 Usage"
            echo "./nrdp.sh [args]..."
            echo "  --version=[arg]                   Override version (default $version)"
            echo "  --output=[arg]                    Set output file (default $output)"
            echo "  --build=[arg]                     Which Jenkins build to use (default last)"
            exit 0
            ;;
        --version=*)
            version=`echo $1 | sed -e 's,.*=,,'`
            ;;
        --output=*)
            output=`echo $1 | sed -e 's,.*=,,'`
            ;;
        --build=*)
            build=`echo $1 | sed -e 's,.*=,,'`
            ;;
        *)
            echo "Unknown switch %1"
            exit 1
            ;;
    esac
    shift
done


[ -z $build ] && build=`curl "http://builds.netflix.com/view/PPD/view/PPD-NRDAPP/view/all/job/PPD-NRDAPP-$version/" \
                        | grep "Last successful build (#[0-9]" \
                        | sed -e 's,^.*Last successful build (#\([0-9]\+\).*$,\1,'`
wget "http://builds.netflix.com/view/PPD/view/PPD-NRDAPP/view/all/job/PPD-NRDAPP-$version/lastSuccessfulBuild/artifact/nrdapp_$version-Debug.tar.gz" -O "$output"
