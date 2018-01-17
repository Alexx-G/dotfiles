#!/usr/bin/env bash

BASEDIR=`pwd`

echo ">>> Create soft links for Emacs config"
EMACS_DIR="$HOME/.emacs.d/"

if [ ! -d "$EMACS_DIR" ]; then
    mkdir "$EMACS_DIR"
fi

for conf in `find $BASEDIR/emacs.d/ -mindepth 1 -maxdepth 1`; do
    targetfile="$EMACS_DIR${conf##*/}"
    if [ -L "$targetfile" ]; then
        echo "!!! There's an existing softlink for $targetfile"
    else
        ln -s "$conf" "$targetfile"
    fi
done

