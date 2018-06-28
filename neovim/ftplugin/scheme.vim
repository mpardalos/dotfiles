command! -nargs=1 RunSExpr call VimuxRunCommand(<f-args>)
nnoremap <leader>r "eya(:RunSExpr <C-r>e<CR>
vnoremap <leader>r "ey:RunSExpr <C-r>e<CR>

