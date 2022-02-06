#!/usr/bin/env bash
export QT_STYLE_OVERRIDE="breezedark"
export QT_QPA_PLATFORMTHEME="qt5ct"
# export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# export QT_QPA_PLATFORM=gtk2
# export XDG_CURRENT_DESKTOP=i3
# Japanese input
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
PATH="$HOME/.node_modules/bin:$PATH"
export npm_config_prefix=~/.node_modules
export _JAVA_AWT_WM_NONREPARENTING=1
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
