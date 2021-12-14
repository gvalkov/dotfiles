#!/usr/bin/env bash

cd $watch_folder
[[ "$3" =~ xt=urn:btih:([^&/]+) ]] || exit;
echo "d10:magnet-uri${#3}:${3}e" | ssh $1 tee "$2/meta-${BASH_REMATCH[1]}.torrent"
