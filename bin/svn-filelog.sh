#!/bin/sh

incremental=0
full=1

while getopts 'in' arg; do
  case "$arg" in
    i) incremental=0; shift;;
    f) full=0; shift;;
  esac
done

if which colordiff  1>/dev/null 2>&1; then
  opts="--diff-cmd colordiff"
else

revs=$(svn log $@ | grep -E 'r[0-9]+' -o | cut -c2-)
for rev in $revs; do
  echo -e "\nr${rev} --------------------------------"
  svn diff -c "$rev" $jjjj
done

# url=$1  # current url of file
# svn log -q $url | grep -E -e "^r[[:digit:]]+" -o | cut -c2- | sort -n | {

# #       first revision as full text
#         echo
#         read r
#         svn log -r$r $url@HEAD
#         svn cat -r$r $url@HEAD
#         echo

# #       remaining revisions as differences to previous revision
#         while read r
#         do
#             echo
#             svn log -r$r $url@HEAD
#             svn diff -c$r $url@HEAD
#             echo
#         done
