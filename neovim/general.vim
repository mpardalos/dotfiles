" Highlight search findings
set hlsearch
" Case insensitive search...
set ignorecase
" ...Except when using capital letters
set smartcase

" Line numbers
set number
" relative to the current line
set relativenumber

"4 spaces for indentation
set expandtab
set tabstop=4
set shiftwidth=4

"Automatically change working directory to that of current file
set autochdir

" Open 2 levels of folds by default
set foldlevelstart=2

" Disable conceal on current line
set concealcursor=""

" 80 character column
set colorcolumn=80
set textwidth=80

" Saner splits
set splitright
set splitbelow

" highligh cursor line & column
set cursorline
set cursorcolumn

" Automatically reload init.vim when it is changed. Either in the dotfiles
" directory or in the nvim config directory
autocmd! bufwritepost $HOME/.config/nvim/* source $HOME/.config/nvim/init.vim
autocmd! bufwritepost $HOME/.config/dotfiles/neovim/* source $HOME/.config/nvim/init.vim

au BufNewFile,BufRead *.es6 set filetype=javascript

" Auto insert mode in the terminal
autocmd BufEnter * if &buftype == "terminal" | startinsert | endif

