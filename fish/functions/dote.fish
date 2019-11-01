function dote -d "Edit dotfiles"
    # Do the pushd/popd thing so that the paths shown are relative to
    # ~/.config/dotfiles, not absolute
    pushd ~/.config/dotfiles
    fzf-dir .
    popd
end
