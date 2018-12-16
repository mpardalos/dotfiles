from typing import Callable, Union
from typing import Dict, Callable
from rofi import Rofi

def menu(r: Rofi, prompt: str, options: Dict[Union[str, Callable[[], str]], Callable], *args, **kwargs):
    """
    Create a menu using rofi to execute on of some options, all args not documented
        are passed directly into Rofi.select
    :param prompt: The prompt to show, can be given as a function to call.
    :param options: A dict of strings to show on the menu the action they execute
    :param rofi: the rofi instance to use

    :returns: The name of the option selected and the value returned by the 
        function executed, or, if none is selected, None
    """
    index, key = r.select(
        prompt, 
        [name() if callable(name) else name for name in options.keys()], # If a name is a callable, call it
        *args, **kwargs)
    
    if key == -1: return None

    name, action = list(options.items())[index]
    action()
    return name, action

    
