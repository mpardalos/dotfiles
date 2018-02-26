# Unbind some stuff for vim/tmux
set -x EDITOR /usr/bin/nvim
bind -e \eh
bind -e \ej
bind -e \ek
bind -e \el

# Add . to PATH
set -gx PATH . $PATH

