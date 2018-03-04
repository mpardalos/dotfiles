source $HOME/.config/nvim/plugins.vim
source $HOME/.config/nvim/mappings.vim
source $HOME/.config/nvim/visuals.vim
source $HOME/.config/nvim/general.vim
if filereadable("./.vimlocal")
    source ./.vimlocal
endif
