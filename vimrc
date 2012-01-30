" General settings ---
set nocompatible
set ruler
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set ttyfast
set modeline
set noshortname
set shortmess=aI "t

set history=512
set undolevels=512

set cmdheight=1
set shell=/usr/bin/zsh
set encoding=utf8
set clipboard+=unnamedplus "yank to clipboard

set timeoutlen=500  " map sequence timeout 
set ttimeoutlen=50  " make esc work faster
set updatetime=4000 " fire CursorHold after this many ms

set wildmenu
set wildmode=list:longest,full
set wildignore=*.swp,*.bak,*.pyc,*.pyo,*.class

"set autoread
"set autowrite
"set autochdir
"set cursorline
"set scrolloff=2
"set undofile

set virtualedit=block

set splitbelow
set splitright
set hidden

set spellfile=~/.vim/dict.add

" load bundle plugins
source ~/.vim/bundle/pathogen.git/autoload/pathogen.vim

let g:pathogen_disabled = ['pyflakes.git', 'easytags.git', 'ropevim.hg', 'ropemode.hg']
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
call pathogen#infect()

" load user functions
source ~/.vim/functions.vim

" load plugin config
source ~/.vim/plugins.vim

" load menus
source ~/.vim/menu.vim

" load roapvim
let $PYTHONPATH .= expand('$HOME/.vim/bundle/ropevim.hg:$HOME/.vim/bundle/ropemode.hg:')
source ~/.vim/bundle/ropevim.hg/ropevim.vim


" Searching & highlighting ---
set showmatch
set ignorecase 
set smartcase
set hlsearch
set incsearch


" Folding ---
set nofoldenable 
set foldmethod=marker
set foldlevel=100
set foldopen=block,hor,mark,percent,quickfix,tag


" [wip] 
autocmd!
"https://github.com/tpope/tpope/blob/master/.vim/plugin/ztemplate.vim
"flatfoot

" Prettiness ---
syntax on

filetype on
filetype plugin on 
filetype indent on

set number nuw=3
if has('gui_running')
    set guioptions=aic
    set guifont=Monospace\ Bold\ 10
    "set guifont=DejaVu\ Sans\ Mono\ 10
    "set guifont=Mensch\ 9
    "set guifont=Droid\ Sans\ Mono\ 10
    "set guifont=Inconsolata\ 9
    "set guifont=Monaco\ Bold\ 9

    "colorscheme wombat
    colorscheme ir_black
    "colorscheme solarized
    "colorscheme blackboard
    set background=dark

    set ballooneval
else
    set t_Co=256
    colorscheme wombat
    "colorscheme solarized
    "colorscheme ir_black
    set background=dark 
endif


" Backups ---
set backupdir=/var/tmp
set directory=/var/tmp
"set noswapfile
"set nobackup
"set nowriteback


" Command mode ---
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
" @todo: cant'get get alt-[fb] to work
cnoremap <C-b> <C-Left>
cnoremap <C-f> <C-Right>


" Mouse ---
set mouse=v
set selectmode+=mouse
set mousehide
set mousefocus
set mousemodel=popup "todo: preserve search word


" Wrapping ---
set nowrap
"set textwidth=80
"set linebreak
"set showbreak='+++ '
"let &showbreak = '↳ '


" Tabs and Spaces ---
set expandtab
set shiftround
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smartindent
set autoindent
set backspace=start,indent,eol


" Abbreviations ---
iabbrev pdb import pdb ; pdb.set_trace()
iabbrev ipdb import ipdb ; ipdb.set_trace()
iabbrev tpdb import trace ; tracer = trace.Trace(ignoredirs=[sys.prefix, sys.exec_prefix,], trace=0, count=1)
iabbrev isodate <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>


" Global map leader
let mapleader   = ','
let g:mapleader = ','


" Shortcuts (includes plugin shortcuts) ---
map <leader>w :w!<cr>
map <leader>q :q<cr>
map <leader>Q :q!<cr>
map <leader>wq :wq!<cr>

map <leader>s :setlocal spell!<cr>

map <F2> <Esc>:%retab<cr>
map <C-Space> <C-x><C-o>

" scroll the viewport faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>

nnoremap <space>     5<c-e>10j
nnoremap <backspace> 5<c-y>10k

set pastetoggle=<F10>

" toggle hidden characters
map <F12> :set invlist<CR>

" switch to the cwd of the open buffer
map <leader>cd :cd %:p:h<cr>

" tab indent/dedent (though I'm already pretty used to ><)
vmap <tab> >gv
vmap <S-tab> <gv

" quickly reload/edit vimrc
nnoremap <leader>rs :source ~/.vimrc<CR>
nnoremap <leader>rt :tabnew ~/.vimrc<CR>

" quickly clear the current search
nmap <silent> <leader>/ :nohlsearch<CR>

" buffer to html
map <silent> <leader>2h :runtime! syntax/2html.vim<CR>

" join line (opposite of S-j)
nnoremap <silent> <C-J> gEa<CR><ESC>ew 

" taglist
nnoremap <silent> <F8> :TlistToggle<CR>

" nerdtree
map <F3> <Esc>:NERDTreeToggle<cr>

" lusty juggler 
nmap <silent> <Leader>l :LustyJuggler<CR>

" tagbar
nnoremap <silent> <F4> :TagbarToggle<CR>

" yankring
nnoremap <silent> <F11> :YRShow<CR>

" lustyjuggler
nmap <silent> <Leader>j :LustyJuggler<CR>


" fuzzy-finder
nnoremap <silent> sb     :FufBuffer<CR>
nnoremap <silent> sf     :FufFileWithCurrentBufferDir<CR>
nnoremap <silent> sF     :FufFileWithFullCwd<CR>
nnoremap <silent> sc     :FufCoverageFile<CR>
nnoremap <silent> sC     :FufCoverageFileChange<CR>
nnoremap <silent> s<C-c> :FufCoverageFileRegister<CR>
nnoremap <silent> sd     :FufDirWithCurrentBufferDir<CR>
nnoremap <silent> sD     :FufDirWithFullCwd<CR>
nnoremap <silent> s<C-d> :FufDir<CR>
nnoremap <silent> smc    :FufMruCmd<CR>
nnoremap <silent> sm     :FufMruFile<CR>
cnoremap <silent> sM     :FufMruFileInCwd<CR>
nnoremap <silent> su     :FufBookmarkFile<CR>
nnoremap <silent> s<C-u> :FufBookmarkFileAdd<CR>
vnoremap <silent> s<C-u> :FufBookmarkFileCddAsSelectedText<CR>
nnoremap <silent> si     :FufBookmarkDir<CR>
nnoremap <silent> s<C-i> :FufBookmarkDirAdd<CR>
nnoremap <silent> st     :FufTag<CR>
nnoremap <silent> sT     :FufTag!<CR>

" Search for current visual selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>


" Tabs ---
map <leader>tn :tabnew<cr>
map <leader>te :tabedit<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove


" Commands ---
" Better use SudoWrite from vim-eunuch
"comm! W exec 'w !sudo tee % > /dev/null' | e!  


" Status line ---
set laststatus=2
set statusline=%!StatusLine01()


" Viminfo --
" http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='30,\"100,:40,%,n~/.viminfo

autocmd BufWritePost * if getline(1) =~ "^#!" | if getline(1) =~ "/bin/bash" | execute 'silent !chmod u+x <afile>' | endif | end

" Auto commands ---
augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

autocmd InsertLeave * set nocul
autocmd InsertEnter * set cul

autocmd BufNewFile,BufWritePre * call AutoMkDir()

" File templates ---
autocmd BufNewFile **/setup.py  silent! 0r ~/.vim/templates/setup.py
autocmd BufNewFile **/build.xml silent! 0r ~/.vim/templates/build.xml
autocmd BufNewFile **/Makefile  silent! 0r ~/.vim/templates/Makefile

autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set listchars=tab:>.,trail:.,extends:#,nbsp:.

autocmd BufReadPre  *.pdf setlocal binary
autocmd FileReadCmd *.doc execute "read! antiword \"<afile>\""

autocmd BufRead  *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\" 
autocmd BufRead  *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m 
autocmd BufWrite *.py :call DeleteTrailingWS()

autocmd BufNewFile,BufRead *.jinja set syntax=htmljinja
autocmd BufNewFile,BufRead *.jinja2 set syntax=htmljinja
autocmd BufNewFile,BufRead *.mako set ft=mako
autocmd BufNewFile,BufRead *.jelly set ft=xml

