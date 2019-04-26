#!/bin/bash

cp ~/.cache/wal/*.rasi ~/.config/rofi

cp ~/.cache/wal/colors.sh ~/.bin/colors.sh
cp ~/.cache/wal/colors-rgba.sh ~/.bin/colors-rgba.sh

cp ~/.cache/wal/lemonsqueezer.json ~/.config/dotfiles/lemonbar/colors.json
kill -10 $(cat /tmp/lemonsqueezer.lock)

cp ~/.cache/wal/zathuracolors ~/.config/zathura/

~/.bin/set_bspwm_colors

