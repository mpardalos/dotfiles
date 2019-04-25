call plug#begin('~/.config/nvim/plugged')

" General {{{
    Plug 'pseewald/vim-anyfold'
    Plug 'ervandew/supertab'
    Plug 'easymotion/vim-easymotion'
    Plug 'tpope/vim-surround'
    Plug 'junegunn/vim-easy-align'
    Plug 'scrooloose/nerdtree'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-repeat'
" }}}

" Tmux {{{
    Plug 'benmills/vimux'
    Plug 'christoomey/vim-tmux-navigator'
" }}}

" FZF {{{
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
    Plug 'junegunn/fzf.vim'
" }}}

" Visuals {{{
    Plug 'chriskempson/base16-vim'
    Plug 'ryanoasis/vim-devicons'
    Plug 'dylanaraps/wal.vim'
    Plug 'miyakogi/seiya.vim' " {{{
    let g:seiya_target_groups = has('nvim') ? ['guibg'] : ['ctermbg']
    " }}}
" }}}

" Airline {{{
    Plug 'vim-airline/vim-airline'
    let g:airline_powerline_fonts = 1
    Plug 'vim-airline/vim-airline-themes'
    let g:airline_theme='wombat'
    " Plug 'edkolev/tmuxline.vim'
" }}}

" Language-specific {{{
    Plug 'dag/vim-fish'
    Plug 'harenome/vim-mipssyntax'
" }}}
call plug#end()


