" nerdstuff
let NERDSpaceDelims = 1
let NERDTreeMapActivateNode = ''
let NERDTreeChDirMode = 2
let NERDTreeIgnore = ['\.pyc$', '\.pyo$', '\.swp$', '\~$',]
let NERDTreeSortOrder = ['\.py$', '*']

" fuf
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 400
let g:fuf_mrucmd_maxItem = 400

" thesaurus
let g:thesaurus_file = "/usr/share/mythes/th_en_US_v2"

" yankring
let g:yankring_history_file = '.yankring_history'

" lustyjuggler 
let g:LustyJugglerShowKeys = 'a'

" remove fugitive buffers
autocmd BufReadPost fugitive://* set bufhidden=delete

" haskellmode
let g:haddock_browser = '/usr/bin/firefox'
au BufEnter *.hs compiler ghc

" notes
let g:notes_directory = "/tmp/notes"
let g:notes_suffix = '.note'

" python syntax
let python_highlight_all = 0

" pydiction
"let g:pydiction_location = "~/.vim/bundle/pydiction/complete-dict"

" rope
let ropevim_vim_completion = 1
let ropevim_extended_complete = 1
