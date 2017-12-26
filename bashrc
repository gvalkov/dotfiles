# -*- mode: sh -*-

#-----------------------------------------------------------------------------
# work in progress
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# running non-interactive
[[ -z "$PS1" ]] && return

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
# alias rm="rm -i"
# alias mv="mv -i"
# alias cp="cp -i"
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# globbing config
shopt -s extglob
shopt -s dotglob
#shopt -s nullglob
shopt -s cdspell
shopt -s globstar
#shopt -s nocaseglob
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# general config
shopt -s checkwinsize
shopt -s cdspell
shopt -s no_empty_cmd_completion
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# set tty tabs
tabs 4,9,13,17,21,25,29,33,37,41 &> /dev/null
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# completion options
#complete -d cd
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# prompt
case "$TERM" in
  xterm-color)     color_prompt=0;;
  xterm-256-color) color_prompt=0;;
esac

if [[ "$color_prompt" -eq 0 ]]; then
  PS1='\[\033[01;33m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\] \$ '
fi

if [ $ASCIINEMA_REC -eq 1 ]; then
  PS1='> '
fi

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
#setterm -bfreq 0
#shopt -o noclobber
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# preferred programs
export EDITOR="vim"
#export GREP_OPTIONS="--color=auto"
export GREP_COLOR="1;32" #green
export PIP_DOWNLOAD_CACHE="$HOME/.pip/download-cache"
#export TERM=xterm-256color
