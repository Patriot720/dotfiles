#!/usr/bin/env bash

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
