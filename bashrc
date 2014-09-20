# -*- mode: sh -*-

#-----------------------------------------------------------------------------
# work in progress
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# movement aliases
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias d="dirs -v -l"
alias 1="pushd"
alias 2="pushd +2"
alias 3="pushd +3"
alias 4="pushd +4"
alias 5="pushd +5"
alias 6="pushd +6"
alias 7="pushd +7"
alias 8="pushd +8"
alias 9="pushd +9"
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ls aliases
alias ls="ls --color=auto -F --group-directories-first"
alias ll="ls -lsh"
alias l='ls -1A'
alias lr='ll -R'
alias lx='ll -XB'
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# command aliases
alias c="cats"
alias h='history'
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# globbing config
shopt -s extglob
shopt -s dotglob
shopt -s nullglob
shopt -s cdspell

#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# history config
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth
export HISTCONTROL=ignorespace
export HISTCONTROL=erasedups
export HISTIGNORE="&:ls:[bf]g:exit"
export HISTFILESIZE=10000
export HISTSIZE=10000
shopt -s histappend
shopt -s cmdhist
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# bash config
unset MAILCHECK
setterm -bfreq 0
#shopt -o noclobber
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# preferred programs
export EDITOR="vim"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="1;32" #green
export PIP_DOWNLOAD_CACHE="$HOME/.pip/download-cache"
export TERM=xterm-256color
