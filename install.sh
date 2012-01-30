#!/bin/bash -x

function c () { readlink -f $1 ; }

for i in zsh bin; do
    [[ -x $i/install.sh ]] && $i/install.sh
done

ln -svnf `c gitconfig`      ~/.gitconfig
ln -svnf `c gitignore`      ~/.gitignore
ln -svnf `c hgrc`           ~/.hgrc

ln -svnf `c tidyrc`         ~/.tidyrc
#ln -svnf `c xinitrc`        ~/.xinitrc
ln -svnf `c screenrc`       ~/.screenrc
#ln -svnf `c stalonetrayrc`  ~/.stalonetrayrc
