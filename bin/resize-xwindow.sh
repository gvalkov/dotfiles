#!/bin/sh
focused=$(xdotool getwindowfocus)
dmenu -p "Width Height:" | xargs xdotool windowsize "$focused"
