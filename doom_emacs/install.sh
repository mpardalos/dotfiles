DEPENDS="emacs fd aspell pandoc editorconfig-core-c"

module_install() {
    printf "\n==> Cloning doom-emacs\n"
    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d

    printf "\n==> Refreshing doom-emacs\n"
    ~/.emacs.d/bin/doom refresh
}
