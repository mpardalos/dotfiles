#!/bin/bash
~/.bin/set_bspwm_colors
pkill polybar; ~/.bin/start_polybar

cat ~/.config/alacritty/alacritty-config.yml ~/.cache/wal/alacritty-colors.yml > ~/.config/alacritty.yml

cat ~/.config/mako/base-config ~/.cache/wal/colors-mako > ~/.config/mako/config
makoctl reload
