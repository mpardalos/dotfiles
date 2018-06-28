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

" Organisation {{{
    Plug 'freitass/todo.txt-vim'
    Plug 'vitalk/vim-simple-todo', {'for': 'list'}
    let g:simple_todo_map_keys = 0
" }}}

" Prose {{{
    Plug 'dbmrq/vim-ditto'
    Plug 'reedes/vim-pencil'
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

    " Python {{{
    Plug 'tmhedberg/SimpylFold'
    " }}}

    " LaTeX {{{
        Plug 'LaTeX-Box-Team/LaTeX-Box'
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
    " }}}

    " Markdown {{{
        Plug 'godlygeek/tabular'
        " Plug 'vim-pandoc/vim-pandoc'
        Plug 'vim-pandoc/vim-pandoc-syntax'

        let g:pandoc#formatting#mode = "h"
        let g:pandoc#folding#fold_yaml = "1"
        let g:pandoc#folding#fold_fenced_codeblocks = "1"
        let g:pandoc#spell#default_langs = ["en", "de"]
        let g:pandoc#formatting#textwidth = "92"
    " }}}

    " Webdev {{{
        Plug 'mattn/emmet-vim'
        Plug 'posva/vim-vue'
        Plug 'lepture/vim-jinja'
    " }}}
    
    " Fish shell {{{
    Plug 'dag/vim-fish'
    " }}}
" }}}
call plug#end()


