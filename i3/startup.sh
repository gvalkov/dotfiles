#!/usr/bin/env bash

if [ -d /proc/acpi/battery/BAT* ]; then
    is_notebook=true
fi

startup_notebook() {
    xinput set-prop "Elan Touchpad" 276 1
    xinput set-prop "Elan Touchpad" 286 1
    xinput set-prop "Elan Touchpad" 296 1
}

startup_shared() {
    nm-applet &
    redshift-gtk &
    xsetroot -solid rgb:29/80/B9 &
    emacs --daemon &
    qxkb &
    clipit &
    xmodmap ~/.Xmodmap &
    xautolock -time 60 -locker "i3lock --ignore-empty-password -c 2980b9" &
    xss-lock -- i3lock --ignore-empty-password -c 2980b9 &
}

startup_shared
is_notebook && startup_notebook
