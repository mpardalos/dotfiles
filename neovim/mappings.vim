let mapleader = ","

" Prevents me from having to press Shift when entering a command
nmap ; :

" Save with zz
nnoremap zz :w<CR>

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

" And, for changing buffers, n and m which are below hjkl
nnoremap <A-n> :bprevious<CR>
nnoremap <A-m> :bnext<CR>

" folding
nnoremap <space> za
nnoremap <leader>F zM

" NERDTree
nnoremap <leader>t :NERDTreeToggle<Return>
let NERDTreeMapActivateNode='<space>'

" For the terminal
" These 2 allow for toggling the terminal with <A-t>
let g:term_buf = 0
function! Term_toggle()
  1wincmd w
  if g:term_buf == bufnr("")
    setlocal bufhidden=hide
    close
  else
    belowright
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

" FZF.vim
nnoremap <C-g><F1> :GFiles<CR>
nnoremap <F1> :Files<CR>
nnoremap <F2> :FZF ~/Documents<CR>
nnoremap <F3> :Files ~/.config/dotfiles<CR>

" Toggle background
map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" Mappings for ditto, a plugin to highlight overused words
nmap <leader>di <Plug>ToggleDitto      " Turn it on and off

nmap ]d <Plug>DittoNext                
nmap [d <Plug>DittoPrev                
nmap dg <Plug>DittoGood                
nmap db <Plug>DittoBad                 
nmap =d <Plug>DittoMore                
nmap -d <Plug>DittoLess                

