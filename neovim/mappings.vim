let mapleader = ","
let maplocalleader = "\\"

" Prevents me from having to press Shift when entering a command
nmap ; :

" Save with <C-s>
nnoremap <C-s> :w<CR>

" Use the macro in the q register with @@
nnoremap @@ @q

" Text manipulation {{{

" Moving lines up and down
nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv

" {un}indenting visual blocks
nnoremap <C-h> <<
nnoremap <C-l> >>
vnoremap <C-h> <gv
vnoremap <C-l> >gv

" }}}

" Moving around buffers {{{
" Alt-hjkl to move between splits
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

" And for moving between tabs u and i are right above hjkl
nnoremap <A-u> gT
nnoremap <A-i> gt

" And, for changing buffers, n and m which are below hjkl
nnoremap <A-n> :bprevious<CR>
nnoremap <A-m> :bnext<CR>
" }}}

" Folding {{{
nnoremap <space> za
" }}}

" NERDTree {{{
nnoremap <leader>/ :NERDTreeToggle<Return>
let NERDTreeMapActivateNode='<space>'
" }}}

" Terminal mode {{{
tnoremap <Esc> <C-\><C-n>
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
tnoremap <A-H> <C-\><C-n><C-w>H
tnoremap <A-J> <C-\><C-n><C-w>J
tnoremap <A-K> <C-\><C-n><C-w>K
tnoremap <A-L> <C-\><C-n><C-w>L
" }}}

" FZF.vim {{{
nnoremap <leader>fd :Files ~/Documents<CR>
nnoremap <leader>fc :Files ~/.config/dotfiles<CR>
nnoremap <leader>fg :GFiles<CR>
" }}}

" Git {{{
nnoremap ,ga :GitGutterStageHunk<CR>
nnoremap ,gu :GitGutterUndoHunk<CR>
nnoremap ,g] :GitGutterNextHunk<CR>
nnoremap ,g[ :GitGutterPrevHunk<CR>
nnoremap ,gs :Gstatus<CR>
nnoremap ,gc :Gcommit<CR>
" }}}

" Toggles  {{{
" Toggle background
map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>
" // to toggle highlighting of search results
nnoremap // :set hlsearch! hlsearch?<CR>
" }}}

" Guides  {{{
" Moving around with <++> as guides (Used by other mappings)
inoremap <leader>m <Esc>/<++><Enter>"_c4l
vnoremap <leader>m <Esc>/<++><Enter>"_c4l
nnoremap <leader>m /<++><Enter>"_c4l
" }}}

" Easy align {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" }}}
