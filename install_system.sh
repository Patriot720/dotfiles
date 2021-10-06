source ./.installation_script/util.sh;

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
    yay --save --answerclean None --answerdiff None
fi

home_dir=/home/$USER

install_packages(){
    yay --noconfirm -Sy rofi-lbonn-wayland-git xorg-server rofi-calc spotify-adblock-git telegram-desktop wget redshift-wayland-git   \
        swaylock zramd variety npm paprefs pavucontrol yad openresolv mako nautilus gnome-disk-utility polkit-gnome pulseaudio \
        peco vim vi sway waybar \
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
        source ./.installation_script/guake.sh
        source ./.installation_script/zsh.sh
        enable_services
        add_git_aliases
        ;;
esac
