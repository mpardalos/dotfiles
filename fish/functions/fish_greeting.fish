function fish_greeting
    if ! [ "$INSIDE_EMACS" ]
        neofetch
        # ~/.motd -t 0.001
    end
end
