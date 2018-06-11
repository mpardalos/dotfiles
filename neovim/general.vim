" Options {{{
"
" Case insensitive search...
set ignorecase
" ...Except when using capital letters
set smartcase

" Line numbers
set relativenumber
set number

"4 spaces for indentation
set expandtab
set tabstop=4
set shiftwidth=4

"Automatically change working directory to that of current file
set autochdir

" Disable the conceal feature
set conceallevel=0
" 92 character column
set colorcolumn=92
set textwidth=92

" Saner splits
set splitright
set splitbelow

" highligh cursor line & column
set cursorline
set cursorcolumn

"no visual wrap
set nowrap

set mouse=a
" }}}

" Autocmds {{{
" Automatically reload init.vim when it is changed. Either in the dotfiles
" directory or in the nvim config directory
if !exists("autocommands_loaded")
    let autocommands_loaded = 1
    autocmd bufwritepost $HOME/.config/nvim/* source $HOME/.config/nvim/init.vim
    autocmd bufwritepost $HOME/.config/dotfiles/neovim/* source $HOME/.config/nvim/init.vim
    au BufNewFile,BufRead *.es6 set filetype=javascript
    " Auto insert mode in the terminal
    autocmd BufEnter * if &buftype == "terminal" | startinsert | endif
endif
" }}}
