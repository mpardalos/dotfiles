call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'ervandew/supertab'

"-------------------git-----------------------
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
"-------------------git-----------------------

"-------------------FZF-----------------------
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
"-------------------FZF-----------------------

"--------------------Python--------------------
Plug 'jmcantrell/vim-virtualenv'
Plug 'tmhedberg/SimpylFold'
let g:SimpylFold_fold_docstring = 0 " Do not fold docstrings
"--------------------Python--------------------

"--------------------LaTeX--------------------
Plug 'reedes/vim-pencil'
Plug 'junegunn/goyo.vim'
Plug 'lervag/vimtex'
let g:tex_flavor='latex'
let g:vimtex_fold_enabled='1'
"--------------------LaTeX--------------------

"--------------------Themes---------------------
Plug 'altercation/vim-colors-solarized'
"--------------------Themes---------------------

"--------------------airline--------------------
Plug 'vim-airline/vim-airline'
let g:airline_powerline_fonts = 1
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='wombat'
"--------------------airline--------------------

"--------------------webdev---------------------
Plug 'mattn/emmet-vim'
"--------------------webdev---------------------
call plug#end()




