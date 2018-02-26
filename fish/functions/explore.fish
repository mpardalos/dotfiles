function explore
    if count $argv >/dev/null
        dolphin $argv >/dev/null 2>&1 &
    else
        dolphin . >/dev/null 2>&1 &
    end

    disown %dolphin
end
