#!/bin/sh
xrandr --output DisplayPort-1 --off \
       --output LVDS --mode 1920x1080 --pos 1920x0 --rotate normal \
       --output DisplayPort-2 --off --output VGA-0 --mode 1920x1080 --pos 0x0 --rotate normal \
       --output DisplayPort-0 --off
