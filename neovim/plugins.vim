call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

Plug 'Yggdroot/indentLine'
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

Plug 'zchee/deoplete-jedi'
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python2'

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
Plug 'morhetz/gruvbox'
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




