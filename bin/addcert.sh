#!/bin/bash

HOST="$1"
PORT=443
if [ -z "$HOST" ]; then
   echo "Must supply a host[:port]!"
   exit 1
fi
if [[ $EUID -ne 0 ]]; then
    echo "addcert needs to run as root!"
    sudo $0 $HOST
    exit 0
fi

if echo "$HOST" | grep ":" >/dev/null 2>&1; then
   PORT=`echo $HOST | cut -d: -f2` 
   HOST=`echo $HOST | cut -d: -f1` 
fi
openssl s_client -showcerts -connect $HOST:$PORT </dev/null 2>/dev/null|openssl x509 -outform PEM >/etc/ssl/certs/$HOST
cat /etc/ssl/certs/$HOST >>/etc/ssl/certs/ca-certificates.crt
