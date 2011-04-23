#!/bin/bash -x

cd $(dirname $0)
function c () { readlink -f $1 ; }

ln -svf `c vimrc`   ~/.vimrc
ln -svf `c vim`     ~/.vim
