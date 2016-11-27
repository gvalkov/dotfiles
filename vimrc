"-----------------------------------------------------------------------------
" General settings
"-----------------------------------------------------------------------------
set nocompatible
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set ttyfast
set lazyredraw
set modeline
set noshortname
set shortmess=aI

set history=512
set undolevels=512

set showmode
set cmdheight=1
set nrformats-=octal
set shell=/usr/bin/zsh
set encoding=utf8
set clipboard=unnamedplus "yank to clipboard
set display+=lastline

set timeoutlen=500  " map sequence timeout
set ttimeout
set ttimeoutlen=50  " make esc work faster
set updatetime=4000 " fire CursorHold after this many ms

set wildmenu
set wildmode=list:longest,full
set wildignore=*.swp,*.bak,*.pyc,*.pyo,*.class

"set autoread
"set autowrite
"set autochdir
"
set scrolloff=3
set virtualedit=block

set splitbelow
set splitright
set hidden
set noautoread

set spellfile=~/.vim/dict.add

" load user functions
source ~/.vim/functions.vim

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


" Diff mode ---
set diffopt=filler
set diffopt+=context:4
set diffopt+=iwhite
set diffopt+=vertical


"-----------------------------------------------------------------------------
" User-interface
"-----------------------------------------------------------------------------
syntax on

filetype on
filetype plugin on
filetype indent on

set number
set cursorline
set relativenumber
set numberwidth=3

set ruler
set rulerformat=%=%h%m%r%w\ %(%c%V%),%l/%L\ %P

if has('gui_running')
    set guioptions=aic
    set guifont=Inconsolata\ 10
    colors zenburn
    set background=dark
    set ballooneval
else
    set t_Co=256
    colors zenburn
    set background=dark
endif


"-----------------------------------------------------------------------------
" Backups and swap-files
"-----------------------------------------------------------------------------
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
" t: autowrap text using textwidth
" l: long lines are not broken in insert mode
" c: autowrap comments using textwidth, inserting leader
" r: insert comment leader after <CR>
" o: insert comment leader after o or O
set formatoptions-=t
set formatoptions+=lcro
set textwidth=120
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
set smarttab
set backspace=start,indent,eol


" Abbreviations ---
iabbrev pdb import pdb ; pdb.set_trace()
iabbrev ipdb import ipdb ; ipdb.set_trace()
iabbrev ppp import pprint; pprint.pprint(
iabbrev tpdb import trace ; tracer = trace.Trace(ignoredirs=[sys.prefix, sys.exec_prefix,], trace=0, count=1)
iabbrev isodate <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>
"iabbrev vlcnow <C-R>=system('vlc-get-current-position.sh')<CR>
cabbr <expr> %% expand ('%:p:h')


"-----------------------------------------------------------------------------
" Shortcuts (includes plugin shortcuts)
"-----------------------------------------------------------------------------

let mapleader   = ','
let g:mapleader = ','

map <leader>w :w!<cr>
map <leader>q :q<cr>
map <leader>Q :q!<cr>
map <leader>wq :wq!<cr>

map <leader>s :setlocal spell!<cr>

map <F2> <Esc>:%retab<cr>
map <C-Space> <C-x><C-o>

" scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

nnoremap <space>     5<c-e>10j
nnoremap <backspace> 5<c-y>10k

cmap <C-V> <C-R>*
vnoremap <C-C> "*y

set pastetoggle=<F10>

" select previous paste
nnoremap gp `[v`]

" toggle hidden characters
map <F12> :set invlist<CR>

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

" Taglist & Tagbar
nnoremap <silent> <F8> :TlistToggle<CR>
nnoremap <silent> <F4> :TagbarToggle<CR>

" Search for current visual selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" common typos
ca WQ wq
ca Q q

" compatibility with other editors/IDEs with which I'm used to.
inoremap <C-g> <Esc>
vnoremap <C-g> <Esc>
onoremap <C-g> <Esc>
nnoremap <C-g> <Esc>
nnoremap <M-g> n :cnext<CR>
nnoremap <M-;> :TComment<CR>
vnoremap <M-;> :TCommentBlock<CR>
onoremap <M-;> :TComment<CR>
inoremap <M-;> :TComment<CR>
nnoremap <C-/> :TComment<CR>
vnoremap <C-/> :TCommentBlock<CR>
onoremap <C-/> :TComment<CR>
inoremap <C-/> :TComment<CR>

" tabs
map <leader>tn :tabnew<cr>
map <leader>te :tabedit<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove


"-----------------------------------------------------------------------------
" Plugins
"-----------------------------------------------------------------------------
 
call plug#begin('~/.vim/bundles')
Plug 'mileszs/ack.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'vim-scripts/ZoomWin'
Plug 'vim-scripts/tComment'
Plug 'junegunn/vim-easy-align'
call plug#end()


"-----------------------------------------------------------------------------
" Plugin configuration.
"-----------------------------------------------------------------------------

" syntastic
let g:syntastic_enable_signs = 1
let g:syntastic_quiet_warnings = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_auto_jump = 0

" zenburn
let g:zenburn_high_Contrast = 0
let g:zenburn_color_also_Ignore = 1
" let g:zenburn_alternate_Error = 1
" let g:zenburn_unified_CursorColumn = 1

" python syntax
let python_highlight_all = 0


"-----------------------------------------------------------------------------
" Status line
"-----------------------------------------------------------------------------
set laststatus=2
set statusline=%f\ %2*%m\ %1*%h%r%=[%{&encoding}\ %{&fileformat}\ %{strlen(&ft)?&ft:'none'}\ %{getfperm(@%)}]\ 0x%B\ %12.(%c:%l/%L%)
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P


"-----------------------------------------------------------------------------
" Viminfo
"-----------------------------------------------------------------------------
" 'n :  marks will be remembered for up to n previously edited files
" "n :  will save up to n lines for each register
" :n :  up to n lines of command-line history will be remembered
" %  :  saves and restores the buffer list
" n  :  where to save the viminfo files
set viminfo='100,\"100,:100,%,n~/.viminfo


"-----------------------------------------------------------------------------
" Auto commands
"-----------------------------------------------------------------------------
au InsertLeave * set nocul
au InsertEnter * set cul
