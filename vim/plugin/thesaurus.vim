" thesaurus.vim - find synonyms for a word
" Author: Viktor Kojouharov
" Version: 0.2
" Uses: Hunspell thesaurus files
" Uses: NLTK python module
" Date: 28.07.09
"
" Usage:
"   Use the Thesaurus normal mode command to find the synonyms for the word
"   under the cursor. The plugin requires vim to be build with python support,
"   and uses the nltk python module for stemming the words. You can obtain
"   nltk from http://www.nltk.org, or via easy_install.
"
"   The plugin defaults to using the en_US thesaurus files, usually installed
"   with Openoffice.org. These files can be located in
"   /usr/share/myspell/dicts. A different thesaurus file can be specified via
"   the g:thesaurus_file variable. It expects the full path to the files,
"   without the extension. If none is specified, a value of
"   '/usr/share/myspell/dicts/th_en_US_v2' is assumed.

if &cp || (exists("g:loaded_thes") && g:loaded_thes)
    finish
endif
let g:loaded_thes = 1

function! s:isPythonInstalled()
    if !has('python')
        echoerr "thesaurus.vim requires vim compiled with +python"
    endif

    return has('python')
endfunction

function! s:DefPython()

    if !s:isPythonInstalled()
        return
    endif

python << PYTHONEOF

import vim
import re

try:
    from nltk.stem.porter import *

    stemmer = PorterStemmer()
except ImportError:
    stemmer = False

def lookup(word, dict):
    f = open(dict + '.idx', 'r')
    length = len(word)
    reg = re.compile('^([\w-]+)\|(\d+)$')
    idx = re.compile('^\w+\|(\d+)$')
    cap = word[0].isupper()
    results = []
    stemmed = []
    lines = -1
    bytes = 0

    if cap:
        word = word.lower()

    if stemmer:
        stem = stemmer.stem(word)
        if word != stem:
            stemmed = lookup(stem, dict)
            if len(stemmed):
                stemmed[0:0] = ["Stem obtained from \"" + word + "\""]

    for line in f:
        line = line.strip()

        if word[0] > line[0]:
            break
        elif word[0] != line[0]:
            continue
        if 1 < length:
            if word[1] > line[1]:
                break
            elif word[1] != line[1]:
                continue
        if 2 < length:
            if word[2] > line[2]:
                break
            elif word[2] != line[2]:
                continue
        if 3 < length:
            if word[3] > line[3]:
                break
            elif word[3] != line[3]:
                continue

        match = idx.match(line)

        if match and match.group(1):
            bytes = int(match.group(1))
            break

    f.close()
    f = open(dict + '.dat', 'r')
    f.seek(bytes)

    for line in f:
        line = line.strip()

        if lines > 0:
            synonyms = line.split('|')
            inner = []
            if cap:
                results.append(word.capitalize() + ' ' + synonyms[0])
            else:
                results.append(word + ' ' + synonyms[0])
            results.append(inner)

            for i in range(1, len(synonyms)):
                if cap:
                    inner.append(synonyms[i].capitalize())
                else:
                    inner.append(synonyms[i])

            lines -= 1
            continue

        elif lines == 0:
            break

        else:
            if word[0] != line[0]:
                continue
            elif 1 < length and word[1] != line[1]:
                continue
            elif 2 < length and word[2] != line[2]:
                continue
            elif 3 < length and word[3] != line[3]:
                continue

            match = reg.match(line)

            if match and match.group(1) == word and match.group(2):
                lines = int(match.group(2))

    f.close()

    if len(stemmed):
        results.extend(stemmed)

    vim.command('silent let g:lookup_meaning = %s' % results)
    return results

PYTHONEOF
endfunction

call s:DefPython()

function! s:Lookup()

    if !s:isPythonInstalled()
        return
    endif

    if exists("g:thesaurus_file") && filereadable(g:thesaurus_file . '.idx') && filereadable(g:thesaurus_file . '.dat')
        let dict = g:thesaurus_file
    else
        let dict = '/usr/share/myspell/dicts/th_en_US_v2'
    endif

    let word = expand("<cword>")
    execute "python lookup('" . word . "', '" . dict . "')"

    if len(g:lookup_meaning) == 0
        unlet g:lookup_meaning
        echo "No synonyms found"
        return
    endif

    let s:b_cur = winbufnr(0)
    silent! split __Synonyms__
    " Mark the buffer as scratch
    call <SID>setupSyntax()
    setlocal buftype=nofile
    setlocal bufhidden=wipe
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted

    nnoremap <buffer> <silent> <ESC> :bwipeout!<CR>
    nnoremap <buffer> <silent> q :bwipeout!<CR>
    nnoremap <buffer> <silent> <CR> :call <SID>UseSynonym(getline('.'))<CR>
    nnoremap <buffer> <silent> <2-LeftMouse> :call <SID>UseSynonym(getline('.'))<CR>

    let index = 0
    let output = "\n"
    while index < len(g:lookup_meaning)
        if type(g:lookup_meaning[index]) == 1 " String
            if g:lookup_meaning[index][0:17] == 'Stem obtained from' && output != "\n"
                let output = output . "\n"
            endif
            let output = output . g:lookup_meaning[index] . "\n"
        elseif type(g:lookup_meaning[index]) == 3 "Array of synonyms
            for j in g:lookup_meaning[index]
                let output = output . ' ' . j . "\n"
            endfor
        endif
        let index += 1
    endwhile

    let output = "<ESC> or 'q' - close, <CR> or <2-LeftMouse>\n" . output
    silent! put =output
    silent! 0,1d
    setlocal nomodifiable
    unlet g:lookup_meaning
endfunction

function! s:UseSynonym(line)
    let line = a:line
    if line =~ '^ \w\+'
        let syn = substitute(line, "^ ", '', '')
        let syn = substitute(syn, "\n$", '', '')
        let syn = substitute(syn, ' ([^)]\+)$', '', '')

        bwipeout!

        let pos = getpos('.')
        let line = getline(pos[1])
        if line[pos[2]] =~ '\w'
            execute "normal edbxi" . syn
        else
            execute "normal bdei " . syn
        endif
    endif
endfunction

function! s:setupSyntax()
  syn clear
  setlocal ft=thesaurus

  syn match     ThesStem        "^Stem obtained from " contains=ThesStemWord
  syn match     ThesStemWord    "\"\w\+\"$" contained
  syn match 	ThesHeader 	display "\w\+\s\+(.\+)$" contains=ThesType
  syn match     ThesType        "(.\+)$" contained
  syn match 	ThesSyn 	"^ [[:alnum:][:space:]-]\+"
  syn match     ThesHelp        "<ESC>.*$"

  hi def link ThesStem  	Title
  hi def link ThesStemWord	String
  hi def link ThesHeader 	Statement
  hi def link ThesType  	Number
  hi def link ThesSyn 		Special
  hi def link ThesHelp 	        Comment	
endfunction

command! Thesaurus call s:Lookup()

