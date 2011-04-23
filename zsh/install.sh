#!/bin/bash -x

cd $(dirname $0)
function c () { readlink -f $1 ; }

ln -snvf `c zshrc`   ~/.zshrc
ln -snvf `c zshenv`  ~/.zshenv
ln -snvf `c zfunc`   ~/.zfunc
