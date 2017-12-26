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
		systemctl reboot
		;;
	shutdown)
		systemctl poweroff
		;;
esac
