#!/usr/bin/env bash

enable_caps_hjkl(){
    sudo cp $home_dir/altgr_vim /usr/share/X11/xkb/symbols/altgr_vim
    sudo sed -i '3 a include "altgr_vim(altgr-vim)"'  /usr/share/X11/xkb/symbols/us
}

if prompt "Enable Caps HJKL?"; then
    enable_caps_hjkl
fi
