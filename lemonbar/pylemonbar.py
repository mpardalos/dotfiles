from subprocess import PIPE
from time import sleep, time
from typing import Callable
import fcntl
import logging 
import os
import subprocess
import uuid

import sh

log = logging.getLogger(__name__)
log.setLevel(logging.FATAL)
log.addHandler(logging.StreamHandler())

def non_block_read(output):
    """
    No clue how this works. hulp.
    """
    fd = output.fileno()
    fl = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
    try:
        return output.read() or ""
    except:
        return ""

def colored(text, color=None, reset_color=None):
    """
    Colorize text for showing on the bar

    :param text: The text to colorize
    :param color: The color in "#RGB", "#RRGGBB" or "#AARRGGBB" format. Defaults to the
    default color of the bar
    :param reset_color: The color to reset to after this text. Defaults to the default color
    of the bar
    """
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{F{color}}}{text!s}%{{F{reset_color}}}'

def backgrounded(text, color=None, reset_color=None):
    """
    Set background color of text for showing on the bar

    :param text: The text to colorize
    :param color: The background color in "#RGB", "#RRGGBB" or "#AARRGGBB" format. Defaults
    to the default background color of the bar
    :param reset_color: The background color to reset to after this text. Defaults to the
    default background color of the bar
    """
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{B{color}}}{text!s}%{{B{reset_color}}}'

def underlined(text, color=None, reset_color=None):
    """
    Set background color of text for showing on the bar

    :param text: The text to colorize
    :param color: The underline color in "#RGB", "#RRGGBB" or "#AARRGGBB" format. Defaults
    to the default underline color of the bar
    :param reset_color: The underline color to reset to after this text. Defaults to the
    default underline color of the bar
    """
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{U{color}}}%{{+u}}{text!s}%{{-u}}%{{U{reset_color}}}'


class Bar:
    """
    Class representing an instance of lemonbar. The output on the lemonbar is defined by
    registering modules (instances of `BarModule`) on an instance of this class. See also
    the `Bar.module` helper method. 

    Usage:
    >>> bar = Bar(font="NotoSans", update_interval=1, bg_color="#FDFDFD")
    >>> @bar.module(1)
    >>> def const_text():
    >>>     return "This will appear on the bar"
    >>> bar.run()
    """
    def __init__(self,
            fonts=None,
            update_interval=0.5,
            bg_color='#000000',
            fg_color='#FFFFFF',
            u_color='#FF0000',
            geometry=None, 
            padding=(0, 0), 
            spacing=0,
            separator=''):
        """
        :param font: The default font of the bar, as would be passed to fc-match
        
        :param update_interval: How often to update the bar. Note that each module also has
        its own update interval. This only controls how often the bar checks if a module
        should be updated. It should be at least equal to the minimum update interval of a
        module in this bar.

        :param bg_color: Default background color for the bar
        :param fg_color: Default foreground color for the bar
        :param u_color: Default underline color for the bar

        :param geometry: Determines the size and positioning of the bar. 4-tuple containing
        (width, height, x-offset, y-offset). This is passed directly to lemonbar's -g
        parameter

        :param padding: The spacing before and after the first and last elements of the
        bar respectively.

        :param spacing: The spacing around each element
        :param separator: The character separating each element. It is inserted in the
        middle of the spacing
        """

        self.update_interval = update_interval
        self.bg_color = bg_color
        self.fg_color = fg_color
        self.u_color = u_color
        self.fonts = fonts or []
        self.geometry = geometry
        self.padding = padding
        self.spacing = spacing
        self.separator = separator

        self.left_modules = []
        self.center_modules = []
        self.right_modules = []

        self.button_callbacks = {}

        self._start_bar()

    def register_button(self, button, callback):
        self.button_callbacks[button.id] = callback

    def button(self, text, button, callback):
        """
        A button on the lemonbar. To create a button which responds to different mouse
        buttons, nest calls to these method as follows:

        >>> bar = Bar(interval=1)
        >>> bar.button(bar.button("Click me", 1, lmb_callback), 2, rmb_callback)

        :param text: The text on the button
        :param button: Which mouse button to respond to
        :param callback: A callback which will be called when this button is activated
        """
        button_obj = Button(text, button)
        self.register_button(button_obj, callback)
        return button_obj

    def module(self, *args, **kwargs):
        """
        Construct a BarModule using the decorated function as its update method and add it
        to the bar's module list
        """
        def decorator(func):
            module_obj = BarModule(*args, **kwargs)
            module_obj.get_output = func
            self.register_module(module_obj)
            return func
            
        return decorator

    def register_module(self, module):
        """
        Register an instance of `BarModule` on this bar
        """
        if module.align == Align.LEFT:
            self.left_modules.append(module)
        elif module.align == Align.CENTER:
            self.center_modules.append(module)
        elif module.align == Align.RIGHT:
            self.right_modules.append(module)
        
        module.on_attach(self)

        log.info("module registered")


    def _update(self):
        """
        Update the bar as needed. Updates any modules which should be updated and calls the
        callbacks of any buttons that have been clicked
        """
        for callback in non_block_read(self._bar.stdout).splitlines():
            self.button_callbacks[callback]()
        
        now = time()
        log.debug('updating')

        spacing = f'%{{O{self.spacing/2}}}{self.separator}%{{O{self.spacing/2}}}'
        output = (
              f'%{{l}}%{{O{self.padding[0]}}}' 
            + spacing.join(module._update(now) for module in self.left_modules)
            + '%{c}' 
            + spacing.join(module._update(now) for module in self.center_modules)
            + '%{r}'
            + spacing.join(module._update(now) for module in self.right_modules)
            + f'%{{O{self.padding[1]}}}'
            )

        log.debug(f'TEXT: {output}')

        self._bar.stdin.write(output + '\n')
        self._bar.stdin.flush()

    def _start_bar(self):
        """
        Start the bar process, passing the arguments as specified in __init__. Stores 
        """
        args = ["/usr/bin/lemonbar", '-p', 
                    '-B', self.bg_color,
                    '-F', self.fg_color, 
                    '-U', self.u_color]

        if self.geometry:
            w, h, x, y = self.geometry
            args += ['-g', f'{w}x{h}+{x}+{y}']
        
        for font in self.fonts:
            args += ['-f', font]

        self._bar = subprocess.Popen(
                args,
                stdin=PIPE, 
                stdout=PIPE,
                universal_newlines=True)

    def run(self):
        """
        Run the bar until an exception (such as Ctrl-c) is raised
        """
        try: 
            while True:
                self._update()
                sleep(self.update_interval)
        finally:
            log.error('terminating bar')
            self._bar.terminate()


class Button:
    """
    Represents a lemonbar button. Must be attached to a bar (directly or through a module)
    for its action to be available

    :param button: Which mouse button this button responds to (see lemonbar docs)
    :param text: The text on the button
    :param id: The id of the button. If None, uses a random uuid.
    """
    def __init__(self, text, button, id=None):
        self._id = id or uuid.uuid4().hex
        self.button = button
        self.text = text

    # No setter
    @property
    def id(self):
        return self._id

    def __str__(self):
        return f'%{{A{self.button}:{self.id}:}}{self.text}%{{A}}'


class Align:
    """
    Represents the different alignments a module can have
    """
    LEFT = 'l'
    CENTER = 'c'
    RIGHT = 'r'

class BarModule:
    """
    A lemonbar module. When attached to a Bar instance, the output of "output" is printed to
    the bar every :param interval: seconds, along with formatting as specified in init
    """
    def __init__(self, 
            interval,
            align=Align.LEFT,
            bg_color=None,
            fg_color=None, 
            u_color=None):

        self.align = align
        self.interval = interval
        self.bg_color = bg_color or '-'
        self.fg_color = fg_color or '-'
        self.u_color = u_color or '-'
        self._last_update_time = -1
        self._last_output = ''

        self.button_callbacks = {}

    def get_output(self):
        """override this"""
        return ''

    def on_attach(self, bar):
        """
        Called when attached to a `Bar` instance. 
        The default implementation attaches the modules buttons to the bar
        """
        bar.button_callbacks = {**bar.button_callbacks, **self.button_callbacks}

    def _update(self, now):
        time_since_last_update = now - self._last_update_time

        if time_since_last_update >= self.interval:
            self._last_update_time = now
            plain_output = self.get_output()
            formatted_output = f'%{{F{self.fg_color}}}%{{B{self.bg_color}}}{plain_output}%{{B-}}%{{F-}}'
            self._last_output = formatted_output

        return self._last_output

    
    def register_button(self, button, callback):
        self.button_callbacks[button.id] = callback

    def button(self, text, button, callback):
        """
        Create a button and register and its callback. When this modules is attached to a
        bar, all its callbacks are registered on the bar. See `Bar.button` for more details

        :param text: The text on the button
        :param button: Which mouse button to respond to
        :param callback: A callback which will be called when this button is activated
        """
        button_obj = Button(text, button)
        self.register_button(button_obj, callback)
        return button_obj


class BSPWMDesktops(BarModule):
    """
    A module for showing desktops for bspwm. Shows every desktop by name and highlights the
    current one.
    """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.events = subprocess.Popen(['bspc', 'subscribe', 'desktop'], stdout=PIPE)

        desktop_ids = sh.bspc.query('-D').splitlines()
        desktop_names = sh.bspc.query('-D', '--names').splitlines()
        desktop_buttons = [
            self.button(name, 1, sh.bspc.desktop.bake('-f', name))
            for name in desktop_names
        ]
        # map desktop ids to desktop names and buttons
        self.desktops = dict(
            zip(desktop_ids, zip(desktop_names, desktop_buttons))
        )
        
        self.current_desktop_id = sh.bspc.query('-D', '-d').strip()

    def get_output(self):
        event_strings = (non_block_read(self.events.stdout)).splitlines()

        # Find the last events of each type and put them in current_state_events
        current_state_events = {}
        for event in map(lambda s: s.split(), reversed(event_strings)):
            if event[0] not in current_state_events:
                current_state_events[event[0]] = event[1:]

        for event_type, args in current_state_events.items():
            if event_type == b'desktop_focus':
                self.current_desktop_id = str(args[1], 'utf-8') 
        
        formatted_desktops = [
            colored(button, "#FF0000") if id == self.current_desktop_id else str(button)
            for id, (name, button) in self.desktops.items()
        ]

        return ' | '.join(formatted_desktops)

