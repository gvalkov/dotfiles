if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis
endif


func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc


func! VisualSearch(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunc
