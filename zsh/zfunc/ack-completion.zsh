#compdef ack ack-grep
# ------------------------------------------------------------------------------
# Copyright (c) 2011 zsh-users
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the zsh-users nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ZSH-USERS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Description
# -----------
#
#  Completion script for ack 1.94 (http://betterthangrep.com).
#
#  Source: https://github.com/zsh-users/zsh-completions
#
# ------------------------------------------------------------------------------
# Authors
# -------
#
#  * Julien Nicoulaud <julien.nicoulaud@gmail.com>
#
# ------------------------------------------------------------------------------
# -*- mode: zsh; sh-indentation: 2; indent-tabs-mode: nil; sh-basic-offset: 2; -*-
# vim: ft=zsh sw=2 ts=2 et
# ------------------------------------------------------------------------------


_ack() {
  local curcontext="$curcontext" state line cmds update_policy ret=1

  zstyle -s ":completion:${curcontext}:" cache-policy update_policy
  [[ -z "$update_policy" ]] && zstyle ":completion:${curcontext}:" cache-policy _ack_types_caching_policy

  unset _ack_raw_types
  if ( [[ ${+_ack_raw_types} -eq 0 ]] || _cache_invalid "ack-grep" ) && ! _retrieve_cache "ack-grep"; then
    _ack_raw_types=(${(S)${(f)${${"$(_call_program types $words[1] --help=types)"}#*--\[no\]}}#*no\]})
    [[ $#_ack_raw_types -gt 0 ]] && _store_cache "ack-grep" _ack_raw_types
  fi

  _arguments -C \
    '(- 1 *)--version[display version and copyright information]' \
    '(- 1 *)--help[print a short help statement]' \
    '(- 1 *)--man[print the manual page]' \
    '(-a --all -u --unrestricted)'{-a,--all}'[operate on all files, regardless of type (but still skip directories like blib, CVS, etc.)]' \
    '(-A --after-context -C --context)'{-A,--after-context}'[print N lines of trailing context after matching lines]:number' \
    '(-B --before-context -C --context)'{-B,--before-context}'[print N lines of leading context before matching lines]:number' \
    '(-C --context -A --after-context -B --before-context)'{-C,--context}'[print N lines (default 2) of context around matching lines]:number' \
    '(-c --count)'{-c,--count}'[suppress normal output; instead print a count of matching lines for each input file]' \
    '(--nocolor)--color[highlight the matching text]' \
    '(--color --color-filename --color-match --color-lineno)--nocolor[supress the color]' \
    '(--nocolor --color)--color-filename[sets the color to be used for filenames]:color:->colors' \
    '(--nocolor --color)--color-match[sets the color to be used for matches]:color:->colors' \
    '(--nocolor --color)--color-lineno[sets the color to be used for line numbers]:color:->colors' \
    '--column[show the column number of the first match]' \
    '(--noenv)--env[enable environment processing]' \
    '(--env)--noenv[disable all environment processing, no .ackrc is read and all environment variables are ignored]' \
    '--flush[flush output immediately]' \
    '-f[only print the files that would be searched, without actually doing any searching]' \
    '(--nofollow)--follow[follow symlinks]' \
    '(--follow)--nofollow[don'\''t follow symlinks]' \
    '-G[only paths matching the given regex are included in the search]:regex' \
    '-g[print files where the relative path + filename matches the given regex]:regex' \
    '(--nogroup)--group[group matches by file name]' \
    '(--group)--nogroup[do not group matches by file name]' \
    '(-H --with-filename -h --no-filename)'{-H,--with-filename}'[print the filename for each match]' \
    '(-h --no-filename -H --with-filename)'{-h,--no-filename}'[suppress the prefixing of filenames on output when multiple files are searched]' \
    '(-i --ignore-case)'{-i,--ignore-case}'[ignore case in the search strings]' \
    '*--ignore-dir[ignore directory]:directory:_files -/' \
    '*--noignore-dir[don'\''t ignore directory]:directory:_files -/' \
    '--line[only print given line of each file]:number' \
    '(-l --files-with-matches -L --files-without-matches)'{-l,--files-with-matches}'[only print the filenames of matching files, instead of the matching text]' \
    '(-L --files-without-matches -l --files-with-matches)'{-L,--files-without-matches}'[only print the filenames of files that do NOT match]' \
    '--match[specify the regular expression explicitly]:regex' \
    '(-m --max-count)'{-m,--max-count}'[stop reading a file after N matches]:number' \
    '(-r -R --recurse -n --no-recurse)'{-r,-R,--recurse}'[recurse into sub-directories]' \
    '(-n --no-recurse -r -R --recurse)'{-n,--no-recurse}'[no descending into subdirectories]' \
    '-o[show only the part of each line matching PATTERN (turns off text highlighting)]:pattern' \
    '--output[output the evaluation of expr for each line (turns off text highlighting)]:expression' \
    '--pager[direct ack'\''s output through program]:pager program:_command_names' \
    '--passthru[prints all lines, whether or not they match the expression]' \
    '--print0[the filenames are output separated with a null byte instead of the usual newline]' \
    '(-Q --literal)'{-Q,--literal}'[quote all metacharacters in the pattern, it is treated as a literal]' \
    '(--no-smart-case)--smart-case[ignore case in the search strings if pattern contains no uppercase characters]' \
    '(--smart-case)--no-smart-case[disable --smart-case option]' \
    '--sort-files[sorts the found files lexically]' \
    '--show-types[outputs the filetypes that ack associates with each file]' \
    '--thpppt[display the all-important Bill The Cat logo]' \
    '*--type[specify the types of files to include or exclude from a search]:type:->types' \
    '*--type-add[files with the given extensions are recognized as being of the given type]:type-def:->type-defs' \
    '*--type-set[files with the given extensions are recognized as being of the given type]:type-def:->type-defs' \
    '(-u --unrestricted -a --all)'{-u,--unrestricted}'[all files and directories (including blib/, core.*, ...) are searched, nothing is skipped]' \
    '(-v --invert-match)'{-v,--invert-match}'[invert match: select non-matching lines]' \
    '(-w --word-regexp)'{-w,--word-regexp}'[force the given pattern to match only whole words]' \
    '-1[stops after reporting first match of any kind]' \
    {'--','--no'}${_ack_raw_types/ ##/\[}']' \
    '1: :->patterns' \
    '*: :_files' \
  && ret=0

  case $state in
    patterns)
      _message -e patterns 'pattern' && ret=0
    ;;
    colors)
      local colors; colors=(
        'black'      'on_black'
        'red'        'on_red'
        'green'      'on_green'
        'yellow'     'on_yellow'
        'blue'       'on_blue'
        'magenta'    'on_magenta'
        'cyan'       'on_cyan'
        'white'      'on_white'
        'clear'
        'reset'
        'dark'
        'bold'
        'underline'
        'underscore'
        'blink'
        'reverse'
        'concealed'
      )
      _describe -t 'colors' 'color' colors && ret=0
    ;;
    type-defs)
      if compset -P '*='; then
        local extensions; extensions=(*.*(:e))
        _values -s ',' 'file extension' '.'$extensions && ret=0
      else
        _message -e type-name 'type name' && ret=0
      fi
    ;;
    types)
      local types; types=({'','no'}${_ack_raw_types/ ##/:})
      _describe -t 'types' 'type' types
    ;;
  esac

  return ret
}

_ack_types_caching_policy() {

  # Rebuild if ackrc more recent than cache.
  [[ -f $HOME/.ackrc && $$HOME/.ackrc -nt "$1" ]] && return 0

  # Rebuild if cache is older than one week.  
  local -a oldp
  oldp=( "$1"(Nmw+1) )
  (( $#oldp )) && return 0

  return 1
}

_ack "$@"
