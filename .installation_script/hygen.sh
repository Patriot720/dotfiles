#!/usr/bin/env bash

install_hygen(){
    sudo npm i -g hygen
    git clone https://github.com/Patriot720/my-hygen-templates.git
}

if prompt_default_yes "Install Hygen?"; then
    install_hygen;
fi
