#!/bin/bash

if [ "$1" == "" ] ; then
    echo "no server set, using localhost"
    HOSTNAME="localhost"
else
    HOSTNAME="$1"
fi

#runhaskell -i../../clckwrks/clckwrks/ -i../clckwrks-theme-happstack/ -i../../clckwrks/clckwrks-plugin-media Main.hs 192.168.0.5
runhaskell -i../clckwrks-theme-happstack/ -i../../clckwrks/clckwrks-plugin-media Main.hs --theme-path ../clckwrks-theme-happstack/ --http-port 8000 --hostname="$HOSTNAME"
