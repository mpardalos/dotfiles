function pman --description "Show a man page as a pdf document"
    man -Tpdf $argv[1] | zathura -
end
