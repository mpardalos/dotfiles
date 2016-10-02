let mapleader = ","

nnoremap <leader>t :NERDTreeToggle<Return>
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

nnoremap <A-H> <C-w>H
nnoremap <A-J> <C-w>J
nnoremap <A-K> <C-w>K
nnoremap <A-L> <C-w>L

nnoremap <A-n> gT
nnoremap <A-m> gt

"Space to toggle folds
nnoremap <Space> za

"For the terminal
nnoremap <C-t> :split term://zsh<Return>
tnoremap <Esc> <C-\><C-n>

" Stop highlighting search findings
nnoremap <leader><space> :nohlsearch<CR>

" B and E instead of ^ and $
onoremap B ^
onoremap E $
nnoremap ^ <nop>
nnoremap $ <nop>

" FZF.vim
noremap <leader>f :Files<CR>
nnoremap <leader>gf :GFiles<CR>


