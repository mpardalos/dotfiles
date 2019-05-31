#!/bin/sh

DEPENDS="fish exa" 
MAKEDEPENDS="curl"

module_install() {
    if fish -c "! type omf 2>&1 >/dev/null"; then
        curl -L https://get.oh-my.fish | fish
    fi

    fish -c "omf install agnoster"
    fish -c "omf theme agnoster"
}
