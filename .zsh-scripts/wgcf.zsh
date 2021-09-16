#!/usr/bin/env zsh

alias wg-generate="cd $HOME;wgcf register --accept-tos; wgcf generate;"
alias wg-start="cd $HOME;sudo wg-quick up wgcf-profile.conf"
alias wg-stop="cd $HOME;sudo wg-quick down wgcf-profile.conf"
