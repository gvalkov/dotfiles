#!/bin/bash

profile='/apps/gnome-terminal/profiles/Default'

colors="\
#202020 #555555 black
#5D1A14 #DA4939 red
#424E24 #A5C261 green
#6F5028 #FFC66D yellow
#263E4E #6D9CBE blue
#3E1F50 #A256C7 magenta
#234E3F #62C1A1 cyan
#979797 #FFFFFF white"

create_palette () {
    echo "$colors" \
    | awk '{c1=c1$1 ; c2=c2$2} END {print c1 c2}' \
    | sed -re 's,#(..)(..)(..),:#\1\1\2\2\3\3,g'  \
    | cut -c2-
}

palette=`create_palette`

gconftool-2 --set "$profile/use_theme_background" --type bool false
gconftool-2 --set "$profile/use_theme_colors" --type bool false
gconftool-2 --set "$profile/palette" --type string "$palette"
# gconftool-2 --set "$profile/background_color" --type string "#00002B2B3636"
# gconftool-2 --set "$profile/foreground_color" --type string "#65657B7B8383"


