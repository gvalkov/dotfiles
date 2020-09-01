#!/usr/bin/env bash

if [[ -d /proc/acpi/battery/BAT* || -d /sys/class/power_supply/BAT* ]]; then
    is_notebook=true
fi

startup_shared() {
    /usr/libexec/geoclue-2.0/demos/agent &
    emacs --daemon &

    swayidle -w \
        timeout 300 'swaylock -f -c 000000' \
        timeout 600 'swaymsg "output * dpms off"' \
        resume 'swaymsg "output * dpms on"' \
        before-sleep 'swaylock -f -c 000000' &
}


startup_shared
is_notebook && startup_notebook
