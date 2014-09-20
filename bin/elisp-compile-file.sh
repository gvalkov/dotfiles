#!/bin/sh

sitelisp="/usr/share/emacs/site-lisp"
elisp -batch -L "$sitelisp" -q -f batch-byte-compile "$@"
