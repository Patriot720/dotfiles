#!/usr/bin/env bash

install_doom_emacs(){
    if [ ! -d "$home_dir/.doom.d" ]
    then
        git clone --depth 1 https://github.com/hlissner/doom-emacs $home_dir/.emacs.d
        git clone https://github.com/Patriot720/.doom.d $home_dir/.doom.d
        $home_dir/.emacs.d/bin/doom install
    fi
}

if prompt "Install Emacs+Doom?"; then
    yay -Sy emacs-gcc-wayland-devel-bin
    install_doom_emacs
fi
