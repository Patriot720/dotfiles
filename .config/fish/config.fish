if status is-interactive
    # Commands to run in interactive sessions can go here
    # set -g QT_QPA_PLATFORM wayland
    # set -g XDG_CURRENT_DESKTOP sway
    # bash -c "eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh)"
    # for env_var in (gnome-keyring-daemon --start);
    #     set -x (echo $env_var | string split "=")
    # end
end

function fish_user_key_bindings
    bind -M normal -m insert \cr 'peco_select_history (commandline -b)'
    bind -M insert \cr 'peco_select_history (commandline -b)'
end

fish_add_path ~/.ghcup/bin/
fish_add_path ~/.cabal/bin/
fish_add_path ~/.pythonbin

set -g XDG_CONFIG_HOME ~/.config/
set -g BROWSER brave
