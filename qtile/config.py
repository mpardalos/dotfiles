import os
import re

from libqtile import bar, widget, hook, layout
from libqtile.config import Key, Group, Screen, Drag, Click, Match
from libqtile.command import lazy, Client
import sh

client = Client()

mod = 'mod4'

cursor_warp = True

keys = [
    Key([mod], 'j', lazy.layout.down()),
    Key([mod], 'k', lazy.layout.up()),
    Key([mod], 'h', lazy.layout.left()),
    Key([mod], 'l', lazy.layout.right()),
    Key([mod, 'shift'], 'k', lazy.layout.shuffle_up()),
    Key([mod, 'shift'], 'j', lazy.layout.shuffle_down()),
    
    Key([mod], 'space', 
        lazy.spawn('rofi -combi-modi window,drun -show combi')),
    Key([mod, 'shift'], 'q', lazy.window.kill()),
    Key([mod], 'f', lazy.window.toggle_floating()),
    Key([mod], 'Tab', lazy.next_layout()),

    # Application shortcuts
    Key([mod], 'Return', lazy.spawn('konsole')),
    Key([mod, 'shift'], 'Return', lazy.spawn('dolphin')),

    # Screen locker
    Key(['control', 'mod1'], 'l',
        lazy.spawn('i3lock --tiling --image {}/Pictures/wallpapers/arc_dark_default_blurred.png'.format(os.path.expanduser('~')))),

    # Live reload
    Key([mod, 'shift'], 'r', lazy.restart())
]

mouse = [
    Drag([mod], 'Button1', lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], 'Button3', lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], 'Button2', lazy.window.bring_to_front())
]

layouts = [
    layout.Columns(margin=5),
    layout.Matrix(margin=5),
    layout.Max(margin=5)
]


groups = [
    Group('', screen_affinity=0),
    Group('', matches=[Match(wm_class=['konsole'])], screen_affinity=0),
    Group('', 
        matches=[Match(wm_class=[re.compile('google play music desktop player', flags=re.I)])],
        screen_affinity=1
    ),
    Group('', 
        matches=[Match(wm_class=['MessengerForDesktop', 'Skype'])]
    )
]

screens = [
    Screen(bottom=bar.Bar([
        widget.GroupBox(),
        widget.WindowName(width=bar.CALCULATED),

        widget.Spacer(),

        widget.Sep(size_percent=60),        
        widget.TextBox(text=''),
        widget.Pacman(),
        widget.Sep(size_percent=60),        
        widget.Systray(),
        widget.Sep(size_percent=60),        
        widget.Clock()
    ], 30, background='#31363b')),

    Screen(bottom=bar.Bar([
        widget.GroupBox(),
        widget.WindowName(width=bar.CALCULATED)
        ], 30, background='#31363b'))
]

keys += [Key([mod], str(i+1), lazy.group[group.name].toscreen()) for i, group in enumerate(groups)]
keys += [Key([mod, 'shift'], str(i+1), lazy.window.togroup(group.name)) for i, group in enumerate(groups)]

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~')
    sh.compton('-f', _bg=True)
    sh.feh(os.path.join(home, 'Pictures', 'wallpapers', 'arc_dark_default.png'), bg_scale=True, _bg=True)
    sh.konsole(_bg=True)

@hook.subscribe.startup
def autostart_always():
    subprocess.run(['setxkbmap', '-model', 'pc105', '-layout', 'gb,gr', '-option',
        'grp:alt_space_toggle,caps:escape'])




