fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.elan/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.config/emacs/bin"
fish_add_path "$HOME/.ghcup/bin"
fish_add_path "$HOME/.cabal/bin"
fish_add_path "$HOME/Documents/go/bin"
fish_add_path "$HOME/.local/share/npm/bin"
fish_add_path "$HOME/.config/dotfiles/bin"

set -gx GOPATH "$HOME/Documents/go"
set -gx VISUAL "/usr/bin/nvim"
set -gx EDITOR "/usr/bin/nvim"
set -gx OSTYPE "linux"
set -gx VAGRANT_DEFAULT_PROVIDER "libvirt"

# Aliases
alias ll="exa -lg --group-directories-first --git"
alias csvcol="column -s, -t"
alias open-rust-doc="open /usr/share/doc/rust/html/index.html"

abbr --add ec "emacsclient -n"
abbr --add g git
abbr --add v nvim
abbr --add tree "ll --tree"

set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

eval (direnv hook fish)

function tmux-set-pane-title 
    if test -n $TMUX
        printf "\033]2;%s\033\ \n" "$argv" 
    end
end

function ssh
  tmux-set-pane-title "$argv"
  command ssh $argv 
  tmux-set-pane-title (hostname)
end
