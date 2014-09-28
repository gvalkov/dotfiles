#!/bin/sh
# usage: ./script [-t tool] [-r] <path>

# default tool used for merging
tool="meld"

# automatically mark file as resolved afterwards
resolve=1

while getopts 'rt:' arg; do
  case "$arg" in
    t) tool=$OPTARG; shift; shift ;;
    r) resolve=0;    shift;;
  esac
done

if [ -z "$1" ]; then
  base=$(find . -name '*.working' | head -1)
  base="${base%.working}"
else
  base="$1"
fi

if [ -z "$base" ] && [ ! -r "$base" ]; then
  echo 'error: no files to diff'
  exit 1
fi

working="${base}.working"
left=$(ls -1 "${base}".merge-left.* | head -1)
right=$(ls -1 "${base}".merge-right.* | head -1)
eval "$tool" "$left" "$working" "$right"

if [ $resolve -eq 0 ]; then
  mv "$working" "${base%.working}"
  svn resolved "${base%.working}"
fi
