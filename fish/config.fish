# Aliases
alias ll="exa -lg --group-directories-first"

abbr --add g git
abbr --add v nvim
abbr --add tree "ll --tree"

set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

eval (direnv hook fish)
