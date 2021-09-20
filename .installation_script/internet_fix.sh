#!/usr/bin/env bash

internet_fix(){
    pacman -Sy dhcpcd;
    sudo systemctl enable dhcpcd;
    sudo systemctl start dhcpcd;
}

if prompt_default_yes "Install dhcpcd?"; then
    internet_fix
fi
