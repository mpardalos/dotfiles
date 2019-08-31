function pman --description "Show a man page as a pdf document"
    man -w $argv[1] 2>&1 >/dev/null && begin;
        man -t $argv[1] | ps2pdf - | zathura -
    end
end
