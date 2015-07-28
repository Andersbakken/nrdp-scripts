#!/bin/bash

var=
case "$1" in
    on|1|enabled|true)
        var=0
        ;;
    off|0|disabled|false)
        var=1
        ;;
    *)
        echo "Invalid option. Usage: $0 (on|1|enabled|true|off|0|disabled|false)"
        exit 1
        ;;
esac

echo "$var" | sudo tee /proc/sys/net/ipv6/conf/all/disable_ipv6
