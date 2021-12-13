#!/usr/bin/env bash
install_fish(){
    yay -Sy fish
    chsh -s /bin/fish
    curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh
}

if prompt "Install fish? (Should be run last)"; then
    install_fish
fi
