#!/bin/bash -x

cd $(dirname $0)
function c () { readlink -f $1 ; }

ln -nsvf `c vimrc`   ~/.vimrc
ln -nsvf `c vim`     ~/.vim
