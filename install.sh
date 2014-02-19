#!/bin/bash

# This script assumes it is positioned in dotfiles directory

DOT_DIR=$PWD/$(dirname $0) # absolute path to dotfiles directory

# Takes source and target and creates link if it does not exist
install_link() {
    local s=$1
    local t=$2

    # if correct link exists do nothing
    [[ -L $t && $(readlink -f $t) == $(readlink -f $s) ]] && return 0

    ln --symbolic --no-target-directory --interactive --verbose $s $t
}

# emacs
install_link $DOT_DIR/emacs.d $HOME/.emacs.d

# bash
install_link $DOT_DIR/bashrc $HOME/.bashrc
install_link $DOT_DIR/bash_aliases $HOME/.bash_aliases
if [ ! -e $HOME/.bash_local ]; then 
	echo "# Put settings specific for this machine here. Loaded by .bashrc" \
		> $HOME/.bash_local
	echo "$HOME/.bash_local created" 
fi
