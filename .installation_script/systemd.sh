#!/usr/bin/env bash

enable_systemd_oomd_service(){
    sudo systemctl enable systemd-oomd.service;
}

if prompt "Enable Systemd oomd service"; then
    enable_systemd_oomd_service
fi
