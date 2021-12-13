#!/usr/bin/env bash

autologin(){
    sudo mkdir -p /etc/systemd/system/getty@tty1.service.d/
    sudo echo '
[Service]
ExecStart=
ExecStart=-/usr/bin/agetty --autologin andrew --noclear %I $TERM
' > /etc/systemd/system/getty@tty1.service.d/override.conf
}

if prompt_default_yes "Setup autologin for user andrew"; then
    autologin;
fi
