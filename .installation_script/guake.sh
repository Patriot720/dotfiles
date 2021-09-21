#!/usr/bin/env bash

if prompt_default_yes "Install Guake?"; then
    yay -Sy guake
    guake --restore-preferences $HOME/.config/guake/guake_prefs
fi
