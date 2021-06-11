#!/usr/bin/env bash

set -ue

find /sys/kernel/iommu_groups/* -maxdepth 0 -type d | sort -V | while read path; do
    n="${path##*/}"
    for dev in $path/devices/*; do
        printf 'IOMMU Group %02d ' "$n"
        lspci -nns "${dev##*/}"
    done
done
