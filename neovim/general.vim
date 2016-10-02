" Highlight search findings
set hlsearch
" Case insensitive search...
set ignorecase
" ...Except when using capital letters
set smartcase

"4 spaces for indentation
set expandtab
set tabstop=4
set shiftwidth=4

"Automatically change working directory to that of current file
set autochdir

" Open 2 levels of folds by default
set foldlevelstart=2

" Automatically reload init.vim when it is changed
autocmd! bufwritepost $HOME/.config/nvim/* source $HOME/.config/nvim/init.vim

au BufNewFile,BufRead *.es6 set filetype=javascript
