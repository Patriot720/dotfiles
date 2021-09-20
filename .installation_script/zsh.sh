#!/usr/bin/env bash

install_zsh(){
    yay -Sy zsh
    chsh -s /bin/zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
}

if prompt "Install zsh? (Should be run last)"; then
   install_zsh
fi
