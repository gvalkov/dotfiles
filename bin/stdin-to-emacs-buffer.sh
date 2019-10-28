#!/bin/sh

set -x

tmpfile=$(mktemp)
sponge > $tmpfile

elisp="
(let ((fname \"${tmpfile}\"))
  (find-file fname)
  (fundamental-mode)
  (set-visited-file-name nil)
  (delete-file fname)
  (message \"got %d bytes\" (- (point-max) (point-min)))
)"

emacsclient -c -n -e "$elisp"
