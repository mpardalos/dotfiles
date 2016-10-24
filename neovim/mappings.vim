let mapleader = ","

" Prevents me from having to press Shift when entering a command
nmap ; :

nnoremap <leader>t :NERDTreeToggle<Return>

" Alt-hjkl to move. 
" Alt seems more convenient than Ctrl (which most people use) since I can hit
" it with my thumb instead of my pinky
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" Similar as above, but for rearranging splits
nnoremap <A-H> <C-w>H
nnoremap <A-J> <C-w>J
nnoremap <A-K> <C-w>K
nnoremap <A-L> <C-w>L

" And for moving between tabs
" u and i are right above hjkl
nnoremap <A-u> gT
nnoremap <A-i> gt

" folding
nnoremap <space> za
nnoremap <leader>F zM

" For the terminal
" These 2 allow for toggling the terminal with <A-t>
let g:term_buf = 0
function! Term_toggle()
  1wincmd w
  if g:term_buf == bufnr("")
    setlocal bufhidden=hide
    close
  else
    topleft vnew
    try
      exec "buffer ".g:term_buf
    catch
      call termopen("zsh", {"detach": 0})
      let g:term_buf = bufnr("")
    endtry
    startinsert!
  endif
endfunction
nnoremap <A-t> :call Term_toggle()<cr>
tnoremap <A-t> <C-\><C-n>:call Term_toggle()<cr>

tnoremap <Esc> <C-\><C-n>
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
tnoremap <A-H> <C-\><C-n><C-w>H
tnoremap <A-J> <C-\><C-n><C-w>J
tnoremap <A-K> <C-\><C-n><C-w>K
tnoremap <A-L> <C-\><C-n><C-w>L

" Stop highlighting search findings
nnoremap <leader><space> :nohlsearch<CR>

" FZF.vim
nnoremap <C-f> :Files<CR>
nnoremap <C-g><C-f> :GFiles<CR>

" Edit config files quickly with F2
nnoremap <F2> :FZF ~/.config/dotfiles<CR>

" Toggle background
map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>


