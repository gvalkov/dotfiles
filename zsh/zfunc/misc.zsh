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
  local dir=${2:-'.'}
  local regex=${1:-'^.*$'}

  local excludes
  excludes=(".git" ".hg" ".svn" "\*.pyc" "\*.pyo" "__pycache__")

  for m in $excludes; do
    local _excludes="${_excludes}-name ${m} -prune -o "
  done

  [[ ${regex[1]} != '^' ]] && regex=".*${regex}"
  [[ ${regex[-1]} != '$' ]] && regex="${regex}.*"

  if [[ "$OSTYPE" =~ "freebsd" ]]; then
    local findargs="-E ${dir}"
  else
    local findargs="${dir} -regextype posix-extended"
  fi

  eval find ${findargs} ${_excludes} -regex "${regex}" -print
}

function pkg () {
  case "$1" in
    search)
      shift
      command pkg search "$@" \
        | nl -nrn -w6 \
        | tee /tmp/pkg-search.tmp \
        | awk '{printf("%s (%s)\n", $2, $1)}' \
        | column -t -s ' '
      ;;
    install)
      shift

      local -a regex args
      local pkgs;

      # $@ = "%1 bash %15" --> regex = "^(1|15)"; args = "bash"
      for arg in "$@"; do
        if [[ ${arg:0:1} == '%' ]]; then
          shift
          regex+=( ${arg:1} )
        else
          args+=( ${arg} )
        fi
      done
      regex="(${(j:|:)regex})"
      pkgs=$(awk -v "query=$regex" '$1 ~ query {printf("%s ", $2)}' /tmp/pkg-search.tmp)
      command pkg install "$args" "$pkgs"
      ;;
    *)
      command pkg "$@";;
  esac
}

zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
