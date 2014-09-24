#!/bin/sh
set -x

c () { readlink -f $1 ; }

[[ -x zsh/install.sh ]] && zsh/install.sh

ln -svnf `c gitconfig`      ~/.gitconfig
ln -svnf `c gitignore`      ~/.gitignore
ln -svnf `c hgrc`           ~/.hgrc

ln -svnf `c screenrc`       ~/.screenrc
ln -svnf `c tmux.conf`      ~/.tmux.conf
ln -snvf `c lessrc`         ~/.lessrc
