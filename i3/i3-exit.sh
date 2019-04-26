#!/bin/sh

lock() {
	:
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
	shutdown)
		sudo systemctl poweroff
		;;
esac
