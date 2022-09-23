# Aliases
alias ll="exa -lg --group-directories-first --git"
alias csvcol="column -s, -t"
alias open-rust-doc="open /usr/share/doc/rust/html/index.html"

abbr --add g git
abbr --add v nvim
abbr --add tree "ll --tree"

set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

set -p PATH ~/.cargo/bin
set -p PATH ~/.local/bin
set -p PATH ~/.emacs.d/bin
set -p PATH ~/.ghcup/bin
set -p PATH ~/.cabal/bin
set -p PATH ~/Documents/go/bin
set -p PATH ~/.local/share/npm/bin

set -x GOPATH "$HOME/Documents/go"

set -x VISUAL /usr/bin/nvim
set -x EDITOR /usr/bin/nvim

set -x OSTYPE linux

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
