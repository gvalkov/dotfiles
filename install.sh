#!/bin/bash -x

function c () { readlink -f $1 ; }

for i in vim zsh; do
    [[ -x $i/install.sh ]] && $i/install.sh
done

ln -svnf `c gitconfig`   ~/.gitconfig
ln -svnf `c gitignore`   ~/.tidyrc
ln -svnf `c hgrc`        ~/.hgrc

ln -svnf `c tidyrc`      ~/.tidyrc
ln -svnf `c screenrc`    ~/.hgrc
