#!/bin/sh

MAKEDEPENDS="curl"
DEPENDS="neovim python-neovim"

module_install() {
    curl --silent -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    nvim +PlugInstall +qa!
}

