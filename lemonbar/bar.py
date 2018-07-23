#!/bin/env python3
from pathlib import Path
import os

from lemonsqueezer.modules import BSPWMDesktops, Battery, CurrentWifi, Clock,\
        CommandMonitorModule, MediaControls
from lemonsqueezer import Align
import lemonsqueezer as lsq

pid_file = Path('/tmp/lemonsqueezer_pid')
pid_file.write_text(str(os.getpid()))


class LayoutIndicator(CommandMonitorModule):
    def __init__(self, *args, **kwargs):
        super().__init__(['skb'], *args, **kwargs)

    def first_output(self, bar):
        return

    def on_line_received(self, process_output):
        return process_output


bar = lsq.Bar(
    config_file=str(Path.home() / '.config/lemonbar/colors.json'),
    fonts=[
        "Font Awesome 5 Free:style=Solid", "Noto Sans Display"
    ],
    geometry=(1900, 30, 10, 10),
    bg_color="#DD6a1931",
    fg_color="#FFFFFF",
    padding=(20, 20),
    spacing=10,
    offset=-2,
    separator='│')

bar.register_module(
    BSPWMDesktops(monitor_sel='focused', highlight_color='#4788ef'))
bar.register_module(
    MediaControls(
        '{artist} - {title}', ('', '', '', ''),
        icon='',
        align=Align.CENTER))
bar.register_module(LayoutIndicator(align=Align.RIGHT))
bar.register_module(Battery(['', '', '', '', ''], align=Align.RIGHT))
bar.register_module(CurrentWifi('wlp58s0', icon='', align=Align.RIGHT))
bar.register_module(
    Clock("+%a %d %b - %H:%M", 20, icon='', align=Align.RIGHT))

try:
    bar.run()
finally:
    os.remove(pid_file)
