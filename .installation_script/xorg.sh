#!/usr/bin/env bash

if prompt_default_no "Install i3wm and xorg packages?"; then
    yay -Sy polybar i3-gaps i3lock-color flameshot pasystray xorg-xinput feh
fi
