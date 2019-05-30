#!/usr/bin/fish

if ! omf -v 
    curl -L https://get.oh-my.fish | fish
end

omf install agnoster
omf theme agnoster
