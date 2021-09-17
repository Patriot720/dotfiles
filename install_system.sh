
if ! command -v yay &> /dev/null
then
    sudo pacman -S --needed base-devel
    mkdir build
    cd build
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
fi

home_dir=/home/$USER

install_packages(){
    yay -Sy dmenu rofi-lbonn-wayland-git rofi-calc spotify-adblock-git telegram-desktop ttf-font-awesome feh redshift-wayland-git emacs-gcc-wayland-devel-bin  \
        i3lock swaylock zramd guake npm gnome-terminal paprefs pavucontrol yad openresolv mako nautilus gnome-disk-utility polkit-gnome flameshot pasystray pulseaudio \
        peco-bin cronie linux-zen sway waybar \
        polybar \
        bluez-utils pulseaudio-bluetooth breeze breeze-gtk panther-launcher-git fcitx5 fcitx5-mozc fcitx5-gtk \
        i3-gaps lightdm-webkit2-greeter lightdm-webkit2-theme-glorious slurp xorg-xwayland python-pywal i3lock-color \
        qt5ct qbittorrent wgcf wireguard-tools lxappearance gimp discord docker docker-compose ttf-fira-code \
        ttf-weather-icons ttf-comfortaa gnu-free-fonts ttf-arphic-uming ttf-baekmuk \
        ttf-nerd-fonts-symbols-mono kdeconnect xorg-server brave adobe-source-han-sans-jp-fonts adobe-source-han-serif-jp-fonts \
        ttf-hanazono otf-ipafont gucharmap \
        xorg-xinput noto-fonts ntfs-3g openssh fcitx5-configtool
}

install_zsh(){
    yay -Sy zsh
    chsh -s /bin/zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
}


add_gpg_key(){
    if ! cat /etc/pacman.d/gnupg/gpg.conf | grep hkp://keys.gnupg
    then
        sudo sh -c "echo 'keyserver hkp://keys.gnupg.net' >> /etc/pacman.d/gnupg/gpg.conf"
        sudo cp /etc/pacman.d/gnupg/gpg.conf $home_dir/.gnupg/gpg.conf
    fi
}

install_lightdm_webkit_greeter(){
    if ! cat /etc/lightdm/lightdm.conf | grep lightdm-webkit2-greeter
    then
        sudo sed -i '/^\[Seat:/a greeter-session = lightdm-webkit2-greeter' /etc/lightdm/lightdm.conf
    fi
}

install_webkit_theme(){
    if ! cat /etc/lightdm/lightdm-webkit2-greeter.conf | grep glorious
    then
        sudo sed -i 's/webkit_theme.*/webkit_theme = glorious/g' /etc/lightdm/lightdm-webkit2-greeter.conf
    fi
}

install_doom_emacs(){
    if [ ! -d "$home_dir/.doom.d" ]
    then
        git clone --depth 1 https://github.com/hlissner/doom-emacs $home_dir/.emacs.d
        git clone https://github.com/Patriot720/.doom.d $home_dir/.doom.d
        $home_dir/.emacs.d/bin/doom install
    fi
}

install_org(){
    if [ ! -d "$home_dir/.doom.d" ]
    then
        git clone https://Patriot720@bitbucket.org/Patriot720/org.git
    fi
}

docker_setup(){
    sudo systemctl enable docker.service
    sudo systemctl start docker.service
    sudo usermod -aG docker $USER
}

internet_fix(){
    pacman -Sy dhcpcd;
    sudo systemctl enable dhcpcd;
    sudo systemctl start dhcpcd;
}

add_multilib(){
    sudo sed -ie '92,94 s/#\s?//' /etc/pacman.conf
}

enable_caps_hjkl(){
    sudo cp $home_dir/altgr_vim /usr/share/X11/xkb/symbols/altgr_vim
    sudo sed -i '3 a include "altgr_vim(altgr-vim)"'  /usr/share/X11/xkb/symbols/us
}

redirect_github_https_to_ssh(){
    git config --global url.ssh://git@github.com/.insteadOf https://github.com/
}

enable_systemd_oomd_service(){
    sudo systemctl enable systemd-oomd.service;
}

enable_lightdm_service(){
    sudo systemctl enable lightdm.service;
}

configure_git_credentials(){
    echo "Input git Email:";
    read email;
    echo "Input git Username:"
    read username;
    git config --global user.email "$email";
    git config --global user.name "$username";
}

install_weather_config(){
    echo "Input OpenWeatherApiPolybar (https://home.openweathermap.org/api_keys) key:"
    read api_key
    touch $home_dir/.config/polybar/config.toml;
    echo "
    # If you don't want to write your key here, you can delete this line and use the OWM_API_KEY environment variable instead
    api_key = \"$api_key\"

    # This is for Montreal, find your city at https://openweathermap.org
    # The id will be the last part of the URL
    city_id = \"524901\"

    # Output format, using Handlebars syntax, meaning variables should be used like {{ this }}
    # Available tokens are:
    # - temp_celcius
    # - temp_kelvin
    # - temp_fahrenheit
    # - temp_icon
    # - trend
    # - forecast_celcius
    # - forecast_kelvin
    # - forecast_fahrenheit
    # - forecast_icon
    display = \"{{ temp_icon }} {{ temp_celcius }}°C {{ trend }} {{ forecast_icon }} {{ forecast_celcius }}°C\"
    " >> $home_dir/.config/polybar/config.toml;
}

install_shure_font(){
    wget -O shure_mono.ttf https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/ShareTechMono/complete/Shure%20Tech%20Mono%20Nerd%20Font%20Complete.ttf?raw=true
    mv shure_mono.ttf $home_dir/.local/share/fonts/
    fc-cache
}

install_hygen(){
    npm i -g hygen
    git clone https://github.com/Patriot720/my-hygen-templates.git
}

enable_services(){
    sudo systemctl enable cronie.service
}

add_git_aliases(){
    git config --global alias.coa '!git add -A && git commit -m'
}


case $1 in
    "-h" | "help" | "")
        echo -e "Arguments:\ninstall_weather_config\ninstall\nwebkit-theme\ninstall_packages\ninstall_zsh\ninternet_fix\nenable_caps_hjkl\ninstall_doom_emacs";;
    install_webkit_theme)
        install_webkit_theme;;
    install_weather_config)
        install_weather_config
        ;;
    install_packages)
        install_packages;;
    internet_fix)
        internet_fix;;
    install_zsh)
        install_zsh;;
    enable_caps_hjkl)
        enable_caps_hjkl
        ;;
    install_doom_emacs)
        install_doom_emacs
        ;;
    configure_git_credentials)
        configure_git_credentials
        ;;
    install)
        add_multilib
        install_packages
        # add_gpg_key
        install_zsh
        install_lightdm_webkit_greeter
        install_webkit_theme
        install_doom_emacs
        docker_setup
        enable_caps_hjkl
        redirect_github_https_to_ssh
        enable_systemd_oomd_service
        enable_lightdm_service
        configure_git_credentials
        install_shure_font
        enable_services
        add_git_aliases
        ;;
esac
