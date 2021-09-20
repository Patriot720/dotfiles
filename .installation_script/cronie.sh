#!/usr/bin/env bash

if prompt "Install Cronie?"; then
    yay -Sy cronie
    sudo systemctl enable cronie.service
fi
