#!/usr/bin/env bash

# check usage
if [ "$1" != "start" ] || [ "$1" != "stop" ] || [ "$1" != "restart" ]; then
    echo ""
    echo "--- dnscrypt-service ---"
    echo "Usage: dnscrypt-service [start|stop|restart]"
    echo "example: dnscrypt-service stop"
    exit 0
fi


sudo dnscrypt-proxy -config ~/.dnscrypt/dnscrypt-proxy.toml -service $1
