#!/usr/bin/env bash

if prompt_default_yes "Install Cronie?"; then
    yay -Sy cronie
    sudo systemctl enable cronie.service
fi
