#!/usr/bin/env bash

set -eu

find /sys/bus/usb/devices/usb* -maxdepth 0 -type l | while read usb_ctrl; do
	pci_path="$(dirname "$(realpath "${usb_ctrl}")")";
       	echo "Bus $(cat "${usb_ctrl}/busnum") --> $(basename $pci_path) (IOMMU group $(basename $(realpath $pci_path/iommu_group)))"
       	lsusb -s "$(cat "${usb_ctrl}/busnum"):"
       	echo
done
