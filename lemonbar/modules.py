import subprocess
from subprocess import PIPE
import fcntl
import os
from logging import DEBUG, WARN, INFO

import sh

from pylemonbar import Bar, Align, BarModule, colored, BSPWMDesktops
import pylemonbar

pylemonbar.log.setLevel(INFO)

bar = Bar(font="UbuntuMono Nerd Font Mono:size=18", 
        geometry=(1896, 30, 12, 10),
        bg_color="#D7D787", 
        fg_color="#121212", padding=(20, 20))

bar.register_module(BSPWMDesktops(1))

@bar.module(20, align=Align.RIGHT)
def clock():
    return sh.date("+%a %d %b - %H:%M").strip()

bar.run()

