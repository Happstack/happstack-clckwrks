#!/bin/bash

# (cd ../clckwrks-theme-n-heptane/swatchmaker ; make)

if [ "$1" == "" ] ; then
    echo "no server set, using localhost"
    HOSTNAME="localhost"
else
    HOSTNAME="$1"
fi

export clckwrks_theme_happstack_datadir="../clckwrks-theme-happstack"
runhaskell -i../clckwrks-theme-happstack/ -i../clckwrks-theme-happstack/dist/build/autogen Main.hs --http-port 8000 --hostname="$HOSTNAME"
