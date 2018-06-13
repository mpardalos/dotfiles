import subprocess
from subprocess import PIPE
from time import sleep, time
import logging 
import fcntl
import os
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
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{F{color}}}{text}%{{F{reset_color}}}'

def backgrounded(text, color=None, reset_color=None):
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{B{color}}}{text}%{{B{reset_color}}}'

def underlined(text, color=None, reset_color=None):
    reset_color = reset_color or '-'
    color = color or '-'
    return f'%{{U{color}}}%{{+u}}{text}%{{-u}}%{{U{reset_color}}}'


class Bar:
    def __init__(self,
            font=None,
            update_interval=0.5,
            bg_color='#000000',
            fg_color='#FFFFFF',
            u_color='#FF0000',
            geometry=None, 
            padding=(0, 0)):

        self.update_interval = update_interval
        self.bg_color = bg_color
        self.fg_color = fg_color
        self.u_color = u_color
        self.font = font
        self.geometry = geometry
        self.padding = padding

        self.left_modules = []
        self.center_modules = []
        self.right_modules = []

        self.button_callbacks = {}

        self._start_bar()

    def button(self, text, button, callback):
        id = uuid.uuid4().hex
        self.button_callbacks[id] = callback
        return f'%{{A{button}:{id}:}}{text}%{{A}}'

    def module(self, *args, **kwargs):
        """
        Construct a BarModule using the decorated function as its update method and add it
        to the bar's module list
        """
        def decorator(func):
            module_obj = BarModule(*args, **kwargs)
            module_obj.output = func
            self.register_module(module_obj)
            return func
            
        return decorator

    def register_module(self, module):
        if module.align == Align.LEFT:
            self.left_modules.append(module)
        elif module.align == Align.CENTER:
            self.center.append(module)
        elif module.align == Align.RIGHT:
            self.right_modules.append(module)

        log.info("module registered")


    def _update(self):
        for callback in non_block_read(self._bar.stdout).splitlines():
            self.button_callbacks[callback]()
        
        now = time()
        log.debug('updating')
        output = (
              f'%{{l}}%{{O{self.padding[0]}}}' 
            + ''.join(module.update(now) for module in self.left_modules)
            + '%{c}' 
            + ''.join(module.update(now) for module in self.center_modules)
            + '%{r}'
            + ''.join(module.update(now) for module in self.right_modules)
            + f'%{{O{self.padding[1]}}}'
            )

        log.info(f'TEXT: {output}')

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
        if self.font:
            args += ['-f', self.font]

        if self.geometry:
            w, h, x, y = self.geometry
            args += ['-g', f'{w}x{h}+{x}+{y}']
        
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

class Align:
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

    def output(self):
        """override this"""
        return ''

    def _formatted_output(self):
        plain_output = self.output()
        formatted_output = f'%{{F{self.fg_color}}}%{{B{self.bg_color}}}{plain_output}%{{B-}}%{{F-}}'

        return formatted_output

    def update(self, now):
        time_since_last_update = now - self._last_update_time

        if time_since_last_update >= self.interval:
            self._last_update_time = now
            self._last_output = self._formatted_output()

        return self._last_output
        

class BSPWMDesktops(BarModule):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.events = subprocess.Popen(['bspc', 'subscribe', 'desktop'], stdout=PIPE)

        desktop_ids = sh.bspc.query('-D').splitlines()
        desktop_names = sh.bspc.query('-D', '--names').splitlines()
        # map desktop ids to desktop names
        self.desktops = dict(zip(desktop_ids, desktop_names))
        
        self.current_desktop_id = sh.bspc.query('-D', '-d').strip()

    def output(self):
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
            colored(name, "#FF0000") if id == self.current_desktop_id else name
            for id, name in self.desktops.items()
        ]
        return ' | '.join(formatted_desktops)

