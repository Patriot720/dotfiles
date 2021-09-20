#!/usr/bin/env bash

if prompt "Install i3wm and xorg packages?"; then
    yay -Sy i3lock polybar i3-gaps i3lock-color flameshot pasystray xorg-server xorg-xinput feh
fi
