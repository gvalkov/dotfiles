#!/bin/sh
set -x

c () { readlink -f $1 ; }
lns () { ln -svnf $(c "$1") "$2" ; }

[ -x zsh/install.sh ] && zsh/install.sh

lns gitconfig      ~/.gitconfig
lns gitignore      ~/.gitignore
#lns hgrc           ~/.hgrc

lns ideavimrc      ~/.ideavimrc
lns bashrc         ~/.bashrc
lns tidyrc         ~/.tidyrc
lns tmux.conf      ~/.tmux.conf
lns user-dirs.dirs ~/.config/user-dirs.dirs

lns mostrc         ~/.mostrc
lns lessrc         ~/.lessrc
lns flake8rc       ~/.config/flake8
lns pythonstartup  ~/.config/pythonstartup

lns flake8rc       ~/.config/flake8
lns sway           ~/.config/sway
lns i3             ~/.config/i3
lns i3status       ~/.config/i3status
lns polybar        ~/.config/polybar
lns kitty          ~/.config/kitty

mkdir -p ~/.config/dunst
lns dunstrc        ~/.config/dunst/dunstrc

mkdir -p ~/.config/mako
lns mako.conf        ~/.config/mako/config

lns Xmodmap        ~/.Xmodmap

mkdir -p ~/.config/rofi
lns rofi           ~/.config/rofi/config

mkdir -p ~/.config/qxkb
lns qxkb.cfg       ~/.config/qxkb/config

# ln -svnf `c xinitrc`        ~/.xinitrc
# ln -svnf `c stalonetrayrc`  ~/.stalonetrayrc
# ln -snvf `c bspwm`          ~/.config/bspwm
# ln -snvf `c sxhkd`          ~/.config/sxhkd
# ln -snvf `c ctags`          ~/.ctags
ln -snvf `c compton.conf`   ~/.compton.conf
# ln -snvf `c xmonad`         ~/.xmonad
# ln -svnf `c screenrc`       ~/.screenrc
