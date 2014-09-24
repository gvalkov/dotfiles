#!/bin/sh
set -x

c () { readlink -f $1 ; }

[ -x zsh/install.sh ] && zsh/install.sh

ln -svnf `c gitconfig`      ~/.gitconfig
ln -svnf `c gitignore`      ~/.gitignore
ln -svnf `c hgrc`           ~/.hgrc

ln -svnf `c bashrc`         ~/.bashrc
ln -svnf `c tidyrc`         ~/.tidyrc
ln -svnf `c screenrc`       ~/.screenrc
ln -svnf `c tmux.conf`      ~/.tmux.conf
ln -snvf `c xmonad`         ~/.xmonad
ln -snvf `c ctags`          ~/.ctags
ln -snvf `c compton.conf`   ~/.compton.conf
ln -snvf `c user-dirs.dirs` ~/.config/user-dirs.dirs
ln -snvf `c vimperatorrc`   ~/.vimperatorrc
ln -snvf `c mostrc`         ~/.mostrc
ln -snvf `c lessrc`         ~/.lessrc
#ln -svnf `c xinitrc`        ~/.xinitrc
#ln -svnf `c stalonetrayrc`  ~/.stalonetrayrc
