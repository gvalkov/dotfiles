#!/bin/sh -ue

pos=`qdbus org.mpris.MediaPlayer2.vlc \
     /org/mpris/MediaPlayer2 \
     org.mpris.MediaPlayer2.Player.Position` 2>/dev/null 

echo "scale=3; $pos / 1000000" | bc | tr '\n' ' '
