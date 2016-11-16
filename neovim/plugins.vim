call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'ervandew/supertab'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1
Plug 'peterhoeg/vim-qml'


Plug 'Yggdroot/indentLine'
"-------------------git-----------------------
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'airblade/vim-gitgutter'
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
Plug 'LaTeX-Box-Team/LaTeX-Box'
let g:tex_flavor='latex'
let g:LatexBox_Folding='1'
let g:LatexBox_quickfix=3

" deoplete LaTeX completion
if !exists('g:deoplete#omni#input_patterns')
      let g:deoplete#omni#input_patterns = {}
  endif
  let g:deoplete#omni#input_patterns.tex = '\\(?:'
        \ .  '\w*cite\w*(?:\s*\[[^]]*\]){0,2}\s*{[^}]*'
        \ . '|\w*ref(?:\s*\{[^}]*|range\s*\{[^,}]*(?:}{)?)'
        \ . '|hyperref\s*\[[^]]*'
        \ . '|includegraphics\*?(?:\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|(?:include(?:only)?|input)\s*\{[^}]*'
        \ . '|\w*(gls|Gls|GLS)(pl)?\w*(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|includepdf(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . '|includestandalone(\s*\[[^]]*\])?\s*\{[^}]*'
        \ .')'
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




