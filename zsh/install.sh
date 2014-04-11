#!/bin/sh
set -x

cd $(dirname $0)
c () { readlink -f $1 ; }

ln -snvf `c zshrc`   ~/.zshrc
ln -snvf `c zshenv`  ~/.zshenv
ln -snvf `c zfunc`   ~/.zfunc
