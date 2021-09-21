#!/usr/bin/env bash

if prompt_default_yes "Install Linux Zen kernel?"; then
    yay -Sy linux-zen
fi
