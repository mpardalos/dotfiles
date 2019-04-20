call plug#begin('~/.config/nvim/plugged')

" General {{{
    Plug 'pseewald/vim-anyfold'
    Plug 'ervandew/supertab'
    Plug 'easymotion/vim-easymotion'
    Plug 'mhinz/vim-startify'
    Plug 'tpope/vim-surround'
    Plug 'junegunn/vim-easy-align'
    Plug 'scrooloose/nerdtree'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } " {{{
    let g:deoplete#enable_at_startup = 1
    "}}}
    Plug 'neomake/neomake' 
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-unimpaired'
" }}}

" Tmux {{{
    Plug 'benmills/vimux'
    Plug 'christoomey/vim-tmux-navigator'
" }}}

" Git {{{
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-git'
    Plug 'airblade/vim-gitgutter'
" }}}

" FZF {{{
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
    Plug 'junegunn/fzf.vim'
" }}}

" Prose {{{
    Plug 'dbmrq/vim-ditto'
    Plug 'junegunn/goyo.vim'
" }}}

" Visuals {{{
    Plug 'chriskempson/base16-vim'
    Plug 'ryanoasis/vim-devicons'
    Plug 'dylanaraps/wal.vim'
    Plug 'miyakogi/seiya.vim' " {{{
    let g:seiya_target_groups = has('nvim') ? ['guibg'] : ['ctermbg']
    " }}}
    Plug 'dbarsam/vim-rainbow-parentheses' " {{{
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
    Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

    let g:LanguageClient_serverCommands = {
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'python': ['pyls']
    \ }

    Plug 'dag/vim-fish'
    Plug 'harenome/vim-mipssyntax'

" }}}
call plug#end()


