set foldmethod=marker
set foldlevelstart=0

" Set a nicer foldtext function
set foldtext=MyFoldText()
function! MyFoldText()
  let line = getline(v:foldstart)
  let line = substitute(line, '{{{', "", "")
  let line = substitute(line, '"', "", "g")
  let line = substitute(line, '^\s\=\s\=\(\s*\)\(.\{-}\)\s*$', '\1--\2', '')
  return line
endfunction



