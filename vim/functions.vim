func! StatusLine01 () 
    let status_line = '%m %F%h%w'
    let cwd = getcwd()
    
    if expand('%:p:h') != cwd
        let cwd_rel = substitute(cwd, $HOME.'/', '~/', 'g')
        "let status_line .= ' (cwd: ' . cwd_rel . ')'
        let status_line .= ' (' . cwd_rel . ')'
    endif

    if exists('g:loaded_fugitive') 
        "let fugitive_branch = substitute(fugitive#statusline(), '.*(\(.*\)).*', '\1', 'g')
        let status_line .= ' (%{fugitive#branchname()})'
    endif

    let status_line .= '%=%04l %04L %3c %3p%%  '
    return status_line
endfunc


function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

" 
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis
endif

func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc


" Search for current visual selection ---
" Taken from http://amix.dk/blog/post/19334
func! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction


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


" From github.com/majutsushi's vimrc
" Automatically create dir to write file to if it doesn't exist
function! AutoMkDir()
    let required_dir = expand("<afile>:p:h")
    if !isdirectory(required_dir)
        if confirm("Directory '" . required_dir . "' doesn't exist.", "&Abort\n&Create it") != 2
            bdelete
            return
        endif

        try
            call mkdir(required_dir, 'p')
        catch
            if confirm("Can't create '" . required_dir . "'", "&Abort\n&Continue anyway") != 2
                bdelete
                return
            endif
        endtry
    endif
endfunction
