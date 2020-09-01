#!/bin/sh -x

# Only one key binding is required, the first keypress performs
# initialisation and hides the terminal again.
if ! xdotool search --classname "org-protocol-capture"; then
    emacsclient -c -e '(org-capture)' -F '(quote (name . "org-protocol-capture"))' &
    # Set the instance to identify the scratchpad.
    xdotool getwindowfocus set_window --classname "org-protocol-capture"
    i3-msg '[instance="org-protocol-capture"] resize set 80 ppt 80 ppt'
    i3-msg '[instance="org-protocol-capture"] move absolute position 2112 24'
    i3-msg '[instance="org-protocol-capture"] move scratchpad'
else
    i3-msg '[instance="org-protocol-capture"] scratchpad show'
fi
