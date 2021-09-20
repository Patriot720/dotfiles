#!/usr/bin/env bash

enable_systemd_oomd_service(){
    sudo systemctl enable systemd-oomd.service;
}

if prompt_default_yes "Enable Systemd oomd service"; then
    enable_systemd_oomd_service
fi
