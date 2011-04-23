function mcd () {
    mkdir -p "$1" && cd "$1";
}

insert_sudo () { zle beginning-of-line; zle -U "sudo "; }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
