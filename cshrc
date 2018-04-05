# -*- mode: sh -*-

# environment
setenv EDITOR 'vim'
setenv WINEDITOR 'vim'

# aliases
alias ll 'ls-F -lF'
alias cp 'cp -ip'
alias mv 'mv -i'

alias f  'find-simple'
alias r  'ranger'
alias d  'dirs -v'
alias ls 'ls --color=auto -F --group-directories-first'
alias ll 'ls -F -lhF'
alias lS 'ls -LrsS'
alias rl 'readlink -f'
alias emc 'emacsclient -n -c'


# options
set nobeep
set autocorrect
set autolist
set autoexpand
#set printexitvalue
set rmstar

set color
set colorcat

set complete = 'enhance'
set correct = 'cmd'
set echo_style = 'both'
set histdup = 'prev'
set history = 10000
set savehist = (10000 merge)
set histfile = "$HOME/.csh-histfile"
set ignoreeof = 1

set implicitcd = verbose
set listjobs = long

if ($?prompt) then
    # keys
    stty sane
    stty echoe
    stty erase "^H"
    stty erase "^?"

    # prompts
    set prompt = "%B%n%b@%m %~ %# %L"
endif


bindkey -e
bindkey "^W"     backward-delete-word
bindkey "^H"     backward-delete-char
bindkey "^?"     backward-delete-char
bindkey "^[[3~"  delete-char
bindkey "^[[28~" run-help
bindkey "^[[1~"  beginning-of-line
bindkey "^[[4~"  end-of-line
bindkey -k up    history-search-backward
bindkey -k down  history-search-forward
bindkey "^R"     i-search-back
+bindkey "^[m"   copy-prev-word
+bindkey "^[."   insert-last-word

## vt100 sends ^? for backspace key and ^[[3~ for delete key
# bindkey "^?" delete-char


# auto complete
complete unalias 'n/*/a/'
complete alias   'p/1/a/'
complete unset   'n/*/s/'
complete uncomplete 'n/*/X/'
complete {cd,pushd,popd} 'p/1/d/'
complete {unset,print}env 'n/*/e/'

complete -%*   'c/%/j/'
complete kill  'c/-/S/' 'c/%/j/'
complete limit 'c/-/"(h)"/' 'n/*/l/'
complete {fg,bg,stop} 'c/%/j/' 'p/1/"(%)"//'
complete {pkill,man,killall,exec,sudo,fakeroot} 'p/1/c/'

complete env  'c/*=/f/' 'p/1/e/=/' 'p/2/c/'
complete set  'c/*=/f/' 'p/1/s/=' 'n/=/f/'
complete exec 'p/1/c/'

## misc
complete find 'p/1/d/' 'c/-/(name size mtime perm type print print0 user  \
                             fstype name perm prune type user nouser \
                             group nogroup size inum atime mtime ctime exec \
                             ok print ls cpio ncpio newer xdev depth \
                             daystart follow maxdepth mindepth noleaf version \
                             anewer cnewer amin cmin mmin true false uid gid \
                             ilname iname ipath iregex links lname empty path \
                             regex used xtype fprint fprint0 fprintf \
                             print0 printf not a and o or)/'
complete man    'p/1/c/'
complete gunzip 'p/*/f:*.{gz2,tgz}/'
complete unzip  'p/*/f:*.zip/'
complete where  'p/1/c/'
complete which  'p/*/c/'
complete vim    'n/*/f:^*.[oa]/'


[ -f $HOME/.cshrc.local ] && source $HOME/.cshrc.local
