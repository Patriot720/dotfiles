#!/usr/bin/env bash

install_lightdm_webkit_greeter(){
    if ! cat /etc/lightdm/lightdm.conf | grep lightdm-webkit2-greeter
    then
        sudo sed -i '/^\[Seat:/a greeter-session = lightdm-webkit2-greeter' /etc/lightdm/lightdm.conf
    fi
}

install_webkit_theme(){
    if ! cat /etc/lightdm/lightdm-webkit2-greeter.conf | grep glorious
    then
        sudo sed -i 's/webkit_theme.*/webkit_theme = glorious/g' /etc/lightdm/lightdm-webkit2-greeter.conf
    fi
}

enable_lightdm_service(){
    sudo systemctl enable lightdm.service;
}

if prompt "Install lightdm + webkit greeter + glorious theme"; then
    yay -Sy lightdm lightdm-webkit2-greeter lightdm-webkit2-theme-glorious;
    install_lightdm_webkit_greeter;
    install_webkit_theme;
    enable_lightdm_service;
fi
