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
alias ll="eza -lg --group-directories-first --git"
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

# Fix for remote emacs connecting to this machine
# https://github.com/oh-my-fish/theme-bobthefish/issues/148
if test "$TERM" = "dumb"
  function fish_prompt
    echo "\$ "
  end

  function fish_right_prompt; end
  function fish_greeting; end
  function fish_title; end
end

# Plugin: THEME PURE
set -p fish_function_path $__fish_config_dir/plugins/pure/functions/
source $__fish_config_dir/plugins/pure/conf.d/pure.fish;

# Plugin: fzf.fish
set -p fish_function_path $__fish_config_dir/plugins/fzf/functions/
source $__fish_config_dir/plugins/fzf/conf.d/fzf.fish;

# Plugin: bass
set -p fish_function_path $__fish_config_dir/plugins/bass/functions/
