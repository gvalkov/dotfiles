" nerdstuff
let NERDTreeDirArrows = 1
let NERDTreeMinimalUI = 1
let NERDSpaceDelims = 1
let NERDTreeShowBookmarks = 1
let NERDTreeMapActivateNode = ''
let NERDTreeChDirMode = 2
let NERDTreeIgnore = ['\.pyc$', '\.pyo$', '\.swp$', '\~$',]
let NERDTreeSortOrder = ['\.py$', '*']

" fuf
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 400
let g:fuf_mrucmd_maxItem = 400

" conqueterm
let g:ConqueTerm_CWInsert = 1
let g:ConqueTerm_ReadUnfocused = 1

" thesaurus
let g:thesaurus_file = "/usr/share/mythes/th_en_US_v2"

" easymotion (not sure if I like this yet)
let g:EasyMotion_keys = 'fjdkslewio' 

" yankring
let g:yankring_history_file = '.yankring_history'

" gundo
let g:gundo_right = 1
let g:gundo_width = 60

" lustyjuggler 
let g:LustyJugglerShowKeys = 'a'

" syntastic
let g:syntastic_enable_signs = 1
let g:syntastic_quiet_warnings = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_auto_jump = 0 

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
