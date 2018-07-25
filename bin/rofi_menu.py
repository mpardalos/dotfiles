from typing import Dict, Callable
from rofi import Rofi

def menu(r: Rofi, prompt: str, options: Dict[str, Callable], *args, **kwargs):
    """
    Create a menu using rofi to execute on of some options, all args not documented
        are passed directly into Rofi.select
    :param options: A dict of strings to show on the menu the action they execute
    :param rofi: the rofi instance to use

    :returns: The name of the option selected and the value returned by the 
        function executed, or, if none is selected, None
    """
    index, key = r.select(prompt, options.keys(), *args, **kwargs)
    if key == -1: return None

    name, action = list(options.items())[index]
    result = action()
    return name, action

    
