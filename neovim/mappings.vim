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

" F4 to toggle highlighting of search results
:noremap <F4> :set hlsearch! hlsearch?<CR>

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

" Moving around with (<>) as guides (Used by other mappings)
inoremap <leader>m <Esc>/<++><Enter>"_c4l
vnoremap <leader>m <Esc>/<++><Enter>"_c4l
nnoremap <leader>m /<++><Enter>"_c4l
