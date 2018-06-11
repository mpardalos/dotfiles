__all__ = ('configure')

def configure(repl):    
    repl.vi_mode = True

    repl.show_signature = True
    repl.show_meta_enter_message = True
    repl.show_line_numbers = True
    repl.enable_open_in_editor = True


    repl.true_color = True
    repl.use_code_colorscheme('monokai')

    
