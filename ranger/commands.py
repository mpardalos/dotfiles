from __future__ import (absolute_import, division, print_function)

import os
from pathlib import Path

from ranger.api.commands import Command

class create(Command):
    """
    :create <name>[/] 
    Create a file or directory. If the name is followed by a slash creates a directory
    """
    def execute(self):
        name = self.arg(1)
        if name == '':
            self.fm.notify('Enter a name', bad=True)
            return

        isdir = name[-1] == '/'
        requested_path = Path(str(self.fm.thisdir)) / name
        if requested_path.exists():
            self.fm.notify('Path exists', bad=True)
        
        if isdir:
            os.mkdir(requested_path)
        else:
            requested_path.touch()
        





