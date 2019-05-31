#!/bin/sh

DEPENDS="fish exa" 
MAKEDEPENDS="curl"

module_install() {
    fish <<-HERE
    if ! type omf 2>&1 >/dev/null
        curl -L https://get.oh-my.fish | fish
    end

    omf install agnoster
    omf theme agnoster
HERE
}
