function mcd () {
  mkdir -p "$1" && cd "$1";
}

function cdls () {
  builtin cd "$1" && ll
}

function vw () {
  vim `which $1`
}

function _cmd_on_last () {
  _tmp=$(mktemp)
  trap "rm $_tmp" SIGINT SIGTERM EXIT

  eval $(fc -l -n -1) > $_tmp
  $1 $_tmp
}

function find-exec {
  find . -type f -iname "*${1:-}*" -exec "${2:-file}" '{}' \;
}

function vl () {
  _cmd_on_last vim
}

function gl () {
  _cmd_on_last gvim
}

function insert_sudo () {
  zle beginning-of-line;
  zle -U "sudo "
}

function rpm-extract () {
  src=$(readlink -f "$1")
  dest=$(basename "$1" .rpm)
  mkdir -p "$dest"
  ( cd "$dest" && rpm2cpio "$src" | cpio -idmv )
}

function find-simple () {
  dir=${2:-'.'}
  regex=${1:-'^.*$'}

  excludes=(".git" "\*.pyc" "\*.pyo")
  for m in $excludes; do
    _excludes="${_excludes}-name ${m} -prune -o "
  done

  [[ ${regex[1]} != '^' ]] && regex=".*${regex}"
  [[ ${regex[-1]} != '$' ]] && regex="${regex}.*"

  eval find ${dir} ${_excludes} -regextype posix-extended -regex "${regex}" -print
}

zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
