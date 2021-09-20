#!/usr/bin/env bash

install_hygen(){
    npm i -g hygen
    git clone https://github.com/Patriot720/my-hygen-templates.git
}

if prompt "Install Hygen?"; then
    install_hygen;
fi
