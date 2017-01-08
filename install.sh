#!/bin/sh
set -x

c () { readlink -f $1 ; }

[ -x zsh/install.sh ] && zsh/install.sh

ln -svnf `c gitconfig`      ~/.gitconfig
ln -svnf `c gitignore`      ~/.gitignore
ln -svnf `c hgrc`           ~/.hgrc

ln -svnf `c bashrc`         ~/.bashrc
ln -svnf `c tidyrc`         ~/.tidyrc
ln -svnf `c tmux.conf`      ~/.tmux.conf
ln -snvf `c user-dirs.dirs` ~/.config/user-dirs.dirs
ln -snvf `c vimperatorrc`   ~/.vimperatorrc
ln -snvf `c mostrc`         ~/.mostrc
ln -snvf `c lessrc`         ~/.lessrc
ln -snvf `c flake8rc`       ~/.config/flake8
ln -snvf `c pythonstartup`  ~/.config/pythonstartup

ln -snvf `c flake8rc`       ~/.config/flake8
ln -snvf `c i3`             ~/.config/i3
ln -snvf `c i3status`       ~/.config/i3status

ln -snvf `c Xmodmap`        ~/.Xmodmap

mkdir -p ~/.config/rofi
ln -snvf `c rofi`           ~/.config/rofi/config

mkdir -p ~/.config/qxkb
ln -snvf `c qxkb.cfg`       ~/.config/qxkb/config

# ln -svnf `c xinitrc`        ~/.xinitrc
# ln -svnf `c stalonetrayrc`  ~/.stalonetrayrc
# ln -snvf `c bspwm`          ~/.config/bspwm
# ln -snvf `c sxhkd`          ~/.config/sxhkd
# ln -snvf `c ctags`          ~/.ctags
# ln -snvf `c compton.conf`   ~/.compton.conf
# ln -snvf `c xmonad`         ~/.xmonad
# ln -svnf `c screenrc`       ~/.screenrc
