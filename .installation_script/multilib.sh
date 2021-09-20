#!/usr/bin/env bash

add_multilib(){
    sudo sed -ie '92,94 s/#\s?//' /etc/pacman.conf
}

if prompt_default_yes "Add multilib?"; then
    add_multilib
fi
