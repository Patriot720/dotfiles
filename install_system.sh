
function prompt(){
    while true; do
        read -n 1 -p "$1 [Y/y/N/n] " yn
        case $yn in
            [Yy]* ) echo;return 0;;
            [Nn]* ) echo;return -1;;
            * ) echo "Please answer [Y/y/N/n]"
        esac
    done
}

function prompt_default_yes(){
    while true; do
        read -r -n 1 -p "$1 [Y/n] " yn
        case $yn in
            "" ) echo;return 0;;
            [Yy]* ) echo;return 0;;
            [Nn]* ) echo;return -1;;
            * ) echo "Please answer [Y/n]"
        esac
    done
}

function prompt_default_no(){
    while true; do
        read -n 1 -p "$1 [y/N] " yn
        case $yn in
            "" ) echo;return -1;;
            [Yy]* ) echo;return 0;;
            [Nn\r]* ) echo;return -1;;
            * ) echo "Please answer [y/N]"
        esac
    done
}

if ! command -v yay &> /dev/null
then
    sudo pacman -S --needed base-devel
    mkdir build
    cd build
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
    cd ..
    rm -rf build
fi

home_dir=/home/$USER

install_packages(){
    yay -Sy rofi-lbonn-wayland-git xorg-server rofi-calc spotify-adblock-git telegram-desktop wget redshift-wayland-git   \
        swaylock zramd guake npm paprefs pavucontrol yad openresolv mako nautilus gnome-disk-utility polkit-gnome pulseaudio \
        peco linux-zen sway waybar \
        bluez-utils pulseaudio-bluetooth breeze breeze-gtk panther-launcher-git fcitx5 fcitx5-mozc fcitx5-gtk \
        dropbox  slurp xorg-xwayland python-pywal \
        qt5ct qbittorrent wgcf wireguard-tools lxappearance gimp discord \
        kdeconnect  brave gucharmap \
        ntfs-3g openssh fcitx5-configtool
}

add_git_aliases(){
    git config --global alias.coa '!git add -A && git commit -m'
}


case $1 in
    "-h" | "help" | "")
        echo -e "Arguments:\ninstall\ninstall_lightdm\ninstall_packages\ninstall_zsh\ninternet_fix\nenable_caps_hjkl";;
    install_lightdm)
        source ./.installation_script/lightdm.sh
        ;;
    internet_fix)
        source ./.installation_script/internet_fix.sh
        ;;
    install_zsh)
        source ./.installation_script/zsh.sh
        ;;
    enable_caps_hjkl)
        source ./.installation_script/caps_hjkl.sh
        ;;
    configure_git_credentials)
        source ./.installation_script/git.sh
        ;;
    install)
        source ./.installation_script/multilib.sh
        install_packages
        source ./.installation_script/lightdm.sh
        source ./.installation_script/docker.sh
        source ./.installation_script/caps_hjkl.sh
        source ./.installation_script/git.sh
        source ./.installation_script/systemd.sh
        source ./.installation_script/fonts.sh
        source ./.installation_script/xorg.sh
        source ./.installation_script/hygen.sh
        source ./.installation_script/emacs.sh
        source ./.installation_script/internet_fix.sh
        source ./.installation_script/cronie.sh
        source ./.installation_script/zsh.sh
        enable_services
        add_git_aliases
        ;;
esac
