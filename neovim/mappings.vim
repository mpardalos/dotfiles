let mapleader = ","


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
nnoremap <A-n> gT
nnoremap <A-m> gt

"Space to toggle folds
nnoremap <Space> za

"For the terminal
nnoremap <C-t> :split term://zsh<Return>
tnoremap <Esc> <C-\><C-n>

" Stop highlighting search findings
nnoremap <leader><space> :nohlsearch<CR>

" FZF.vim
noremap <leader>f :Files<CR>
nnoremap <leader>gf :GFiles<CR>

" Toggle background
map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>


