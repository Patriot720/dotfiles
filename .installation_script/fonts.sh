#!/usr/bin/env bash

install_shure_font(){
    wget -O shure_mono.ttf https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/ShareTechMono/complete/Shure%20Tech%20Mono%20Nerd%20Font%20Complete.ttf?raw=true
    mv shure_mono.ttf $home_dir/.local/share/fonts/
    fc-cache
}

if prompt_default_yes "Install fonts?"; then
    yay -Sy ttf-font-awesome gnu-free-fonts ttf-nerd-fonts-symbols-mono ttf-weather-icons ttf-comfortaa ttf-arphic-uming \
        adobe-source-han-sans-jp-fonts adobe-source-han-serif-jp-fonts ttf-baekmuk otf-ipafont noto-fonts ttf-fira-code \
        ttf-hanazono;
    mkdir -p .local/share/fonts/
    install_shure_font;
fi
