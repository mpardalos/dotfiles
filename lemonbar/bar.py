#!/bin/env python3
from pathlib import Path
import os

from lemonsqueezer.modules import Clock
from modules import BSPWMDesktops, Battery, CurrentWifi, MediaControls, LayoutIndicator
from lemonsqueezer import Align
import lemonsqueezer as lsq

lockfile = Path('/tmp/lemonsqueezer.lock')
lockfile.write_text(str(os.getpid()))


bar = lsq.Bar(
    config_file=str(Path.home() / '.config/lemonbar/colors.json'),
    fonts=[
        "FontAwesome:style=Regular", "Noto Sans Display", "Sans"
    ],
    screen_bottom=True,
    geometry=(1920, 30, 0, 0),
    bg_color="#DD6a1931",
    fg_color="#FFFFFF",
    padding=(20, 20),
    spacing=10,
    offset=-5,
    separator='│')

bar.register_module(
    MediaControls(
        '{artist} - {title}', ('  ', '  ', '', ''),
        icon='',
        align=Align.LEFT))
bar.register_module(
    BSPWMDesktops(
        monitor_sel='primary',
        highlight_color='#4788ef',
        align=Align.CENTER))
bar.register_module(LayoutIndicator(align=Align.RIGHT))
bar.register_module(Battery(
    plugged_in_icon='',
    battery_icon='',
    align=Align.RIGHT))
bar.register_module(CurrentWifi('wlp58s0', icon='', align=Align.RIGHT))
bar.register_module(
    Clock("+%a %d %b - %H:%M", 20, icon='', align=Align.RIGHT))

try:
    bar.run()
finally:
    os.remove(lockfile)
