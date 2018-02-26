function explore
    dolphin $argv >/dev/null 2>&1 &
    disown %dolphin
end
