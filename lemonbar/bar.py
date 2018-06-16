#!/bin/env python3

from logging import DEBUG, WARN, INFO

import sh

from pylemonbar import Bar, Align, BSPWMDesktops, BarModule
import pylemonbar

pylemonbar.log.setLevel(WARN)

bar = Bar(update_interval=0.1,
        fonts=["Font Awesome 5 Free:style=Solid", "UbuntuMono Nerd Font Mono:size=12"], 
        geometry=(1896, 30, 12, 10),
        bg_color="#DD6a1931", 
        fg_color="#FFFFFF", 
        padding=(20, 20), 
        spacing=10,
        separator='│')

bar.register_module(BSPWMDesktops(0.5))

@bar.module(2, align=Align.CENTER)
def volume():
    status = sh.amixer.sget.Master().splitlines()
    vol_line = next(l.split() for l in status if '%' in l)
    vol, mute = int(vol_line[4][1:-2]), vol_line[5][1:-1] == 'off'

    if mute:
        return ' -'
    elif vol == 0:
        return ' 0%'
    elif vol < 30:
        return f' {vol}%'
    else:
        return f' {vol}%'


play_button = bar.button('', 1, sh.playerctl.play)
pause_button = bar.button('', 1, sh.playerctl.pause)
previous_button = bar.button('', 1, sh.playerctl.previous)
next_button = bar.button('', 1, sh.playerctl.next)

@bar.module(2, align=Align.CENTER)
def music():
    try:
        playing = sh.playerctl.status().strip() == 'Playing'
    except sh.ErrorReturnCode_1:
        return ' -'

    play_pause_button = pause_button if playing else play_button

    now_playing = '-'
    if playing:
        artist = sh.playerctl.metadata.artist()
        track = sh.playerctl.metadata.title()
        now_playing = f'{artist} - {track}'
    
    return ' '.join(map(str, ['', now_playing, previous_button, play_pause_button, next_button]))

    

@bar.module(10, align=Align.RIGHT)
def battery():
    status = list(map(lambda s: s.strip(), sh.acpi('-b').split(',')))
    battery_level = int(status[1][:-1])
    if battery_level < 20:
        icon = ''
    elif battery_level < 40:
        icon = ''
    elif battery_level < 60:
        icon = ''
    elif battery_level < 80:
        icon = ''
    else:
        icon = ''

    return icon + ' ' + str(battery_level) + '%'


nmtui_button = bar.button('', 1, sh.alacritty.bake('-e', 'nmtui-connect'))
@bar.module(20, align=Align.RIGHT)
def wifi():
    nmtui_button.text = ' ' + str(sh.nmcli(
        '--terse', '--fields', 'name', '--colors=no', 'connection', 'show', '--active')
        ).strip()

    return nmtui_button

@bar.module(20, align=Align.RIGHT)
def clock():
    return ' ' + sh.date("+%a %d %b - %H:%M").strip()

bar.run()

