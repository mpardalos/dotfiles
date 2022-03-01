# Aliases
alias ll="exa -lg --group-directories-first"
alias csvcol="column -s, -t"

abbr --add g git
abbr --add v nvim
abbr --add tree "ll --tree"

set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

set -a PATH ~/.local/bin
set -a PATH ~/.emacs.d/bin
set -a PATH ~/.ghcup/bin
set -a PATH ~/.cabal/bin
set -a PATH ~/Documents/go/bin
set -a PATH ~/.local/share/npm/bin

set -x GOPATH "$HOME/Documents/go"

set -x VISUAL /usr/bin/nvim
set -x EDITOR /usr/bin/nvim

set -x OSTYPE linux

eval (direnv hook fish)
