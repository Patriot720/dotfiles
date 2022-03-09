if status is-interactive
    # Commands to run in interactive sessions can go here
    # set -g QT_QPA_PLATFORM wayland
    # set -g XDG_CURRENT_DESKTOP sway
end

function fish_user_key_bindings
    bind -M normal -m insert \cr 'peco_select_history (commandline -b)'
    bind -M insert \cr 'peco_select_history (commandline -b)'
end

fish_add_path ~/.ghcup/bin/
fish_add_path ~/.cabal/bin/

set -g XDG_CONFIG_HOME ~/.config/
