" File type overrides ---
au BufReadPre  *.pdf setlocal binary
au FileReadCmd *.doc execute "read! antiword \"<afile>\""

au BufRead  *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\" 
au BufRead  *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m 
au BufWrite *.py :call DeleteTrailingWS()

au FileType notes let b:delimitMate_autoclose = 0
" au FileType notes nmap <buffer>  <leader><Space> :NoteToggleCheckbox<CR>

au BufNewFile,BufRead *.jinja set syntax=htmljinja
au BufNewFile,BufRead *.jinja2 set syntax=htmljinja
au BufNewFile,BufRead *.mako set ft=mako
au BufRead,BufNewFile nginx.conf set ft=nginx

autocmd BufNewFile,BufRead *.jelly set ft=xml
au BufNewFile,BufRead *.ini,*/.hgrc,*/.hg/hgrc setf ini 
au BufRead,BufNewFile nginx.conf set ft=nginx 
au BufRead,BufNewFile haproxy.conf set ft=haproxy
 
