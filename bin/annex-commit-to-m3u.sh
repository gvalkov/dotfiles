#!/usr/bin/env bash
set -ue

curdir=`git rev-parse --show-toplevel`
cmd="git show '$1' --name-status | grep '^A\s' | tr '\t' ' ' | cut -d' ' -f2-"

IFS=$'\n'
for i in $(eval $cmd); do
    echo "$curdir/$i"
done
