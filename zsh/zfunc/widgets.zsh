global-alias-tilde() {
  if [[ $LBUFFER = "git"* ]] || [[ $PENDING -ge 1 ]]; then
   LBUFFER+="~"
  else
    LBUFFER+="~/"
  fi
}

after-first-word() {
  zle beginning-of-line
  zle vi-forward-blank-word
  zle backward-char
  LBUFFER+=' '
}

global-alias-dirstack() {
  LBUFFER+="cd -"
  zle expand-or-complete
}

zle -N global-alias-tilde
zle -N after-first-word
zle -N global-alias-dirstack
