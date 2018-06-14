#!/bin/env python

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

@bar.module(10, align=Align.RIGHT)
def battery():
    status = list(map(lambda s: s.strip(), sh.acpi('-b').split(',')))
    print(status)
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


@bar.module(60, align=Align.RIGHT)
def fa():
    return ' ' + str(sh.nmcli(
        '--terse', '--fields', 'name', '--colors=no', 'connection', 'show', '--active')
        ).strip()

@bar.module(20, align=Align.RIGHT)
def clock():
    return ' ' + sh.date("+%a %d %b - %H:%M").strip()

bar.run()

