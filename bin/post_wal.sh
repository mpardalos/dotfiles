#!/bin/bash

cp ~/.cache/wal/rofi_flat.rasi       ~/.config/rofi/flat.rasi
cp ~/.cache/wal/rofi_fullscreen.rasi ~/.config/rofi/fullscreen.rasi

cp ~/.cache/wal/colors.sh ~/.bin/colors.sh
cp ~/.cache/wal/colors-rgba.sh ~/.bin/colors-rgba.sh

cp ~/.cache/wal/lemonsqueezer.json ~/.config/dotfiles/lemonbar/colors.json
kill -10 $(cat /tmp/lemonsqueezer.lock)

~/.bin/set_bspwm_colors

