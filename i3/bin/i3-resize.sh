#!/bin/sh

set -o nounset
direction="$1"
distance="5 px or 5 ppt"

move() {
  i3-msg resize "$1" "$2" "$distance" | grep -qF '"success":true' || i3-msg resize "$3" "$4" "$distance"
}

case $direction in
  up)    move grow up shrink down ;;
  down)  move shrink up grow down ;;
  left)  move shrink right grow left ;;
  right) move grow right shrink left ;;
esac
