#!/bin/sh

DEPENDS="fish exa" 
MAKEDEPENDS="curl"

module_install() {
    if fish -c "! type omf >/dev/null 2>&1 "; then
        echo
        echo "==> Downloading omf installer"
        curl -L https://get.oh-my.fish > /tmp/fish-install-omf 2>/dev/null

        echo
        echo "==> Running omf installer"
        chmod u+x /tmp/fish-install-omf
        /tmp/fish-install-omf --noninteractive
    fi

    echo "==> Setting omf theme"
    fish -c "omf install agnoster"
    fish -c "omf theme agnoster"
}
