#!/bin/bash

# This script assumes it is positioned in dotfiles directory

DOT_DIR=$PWD/$(dirname $0) # absolute path to dotfiles directory

# Takes two params, source and target, and creates link if it does not exist
install_link() {
    local s=$1
    local t=$2

    # if correct link exists
    [[ -L $t && $(readlink -f $t) == $(readlink -f $s) ]] && return 0

    ln --s --no-target-directory --interactive --verbose $s $t
}

# emacs
install_link $DOT_DIR/emacs.d $HOME/.emacs.d
