let mapleader = "\<space>"
let maplocalleader = "\<space><space>"

" Prevents me from having to press Shift when entering a command
nmap ; :

" Use the macro in the q register with @@
nnoremap Q @q
vnoremap Q :norm @q<CR>

" <C-Space> omni completes
imap <C-space> <C-x><C-o>

" Text manipulation {{{

" Moving lines up and down
" nnoremap <C-j> :m .+1<CR>==
" nnoremap <C-k> :m .-2<CR>==
" vnoremap <C-j> :m '>+1<CR>gv=gv
" vnoremap <C-k> :m '<-2<CR>gv=gv

" {un}indenting visual blocks
vnoremap < <gv
vnoremap > >gv

" }}}

" Moving around {{{

" Tmux-navigator {{{

" Alt-hjkl to move between splits
" Alt seems more convenient than Ctrl (which most people use) since I can hit
" it with my thumb instead of my pinky
nnoremap <silent> <A-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <A-j> :TmuxNavigateDown<CR>
nnoremap <silent> <A-k> :TmuxNavigateUp<CR>
nnoremap <silent> <A-l> :TmuxNavigateRight<CR>

let g:tmux_navigator_no_mappings = 1
" Write all buffers before navigating from Vim to tmux pane
let g:tmux_navigator_save_on_switch = 1
" Disable tmux navigator when zooming the Vim pane
let g:tmux_navigator_disable_when_zoomed = 1

" }}}

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
nnoremap <leader>fb :Buffers<CR>
" }}}

" Toggles  {{{
" Toggle background
map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>
" // to toggle highlighting of search results
nnoremap // :set hlsearch! hlsearch?<CR>
" }}}

" Easy align {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" }}}

" Marks {{{
function! GoToMark()
    execute "normal! /<++>/\<CR>ca<"
endfunction
nnoremap <leader>m :call GoToMark()<CR>a
inoremap <leader>m <Esc>:call GoToMark()<CR>a
" }}}
