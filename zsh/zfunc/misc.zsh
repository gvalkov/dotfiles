function mcd () {
    mkdir -p "$1" && cd "$1";
}

function vw () {
    vim `which $1`
}

function cd () {
    case "$*" in
        "...")
            cd ../../ ;;
        "....")
            cd ../../../ ;;
        *)
            builtin cd "$@";;
    esac
}

function _cmd_on_last () {
    _tmp=$(mktemp)
    trap "rm $_tmp" SIGINT SIGTERM EXIT

    eval $(fc -l -n -1) > $_tmp
    $1 $_tmp
}

function vl () {
    _cmd_on_last vim
}

function gl () {
    _cmd_on_last gvim
}

function insert_sudo () {zle beginning-of-line; zle -U "sudo ";}
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
