#!/bin/sh

lock() {
    i3lock -c 000000
}

case "$1" in
	lock)
		lock
		;;
	logout)
		i3-msg exit
		;;
	suspend)
		lock && systemctl suspend
		;;
	reboot)
		sudo systemctl reboot
		;;
	hibernate)
		sudo systemctl hibernate
		;;
	shutdown)
		sudo systemctl poweroff
		;;
esac
