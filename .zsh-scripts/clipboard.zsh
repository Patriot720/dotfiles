#!/usr/bin/env zsh

alias clip="wl-copy"

fp () {
    case "$1" in
        /*) printf '%s\n' "$1";;
        *) printf '%s\n' "file://$PWD/$1";;
    esac
}

clip-file(){
    fp $1 | clip -t text/uri-list
}

clip-files(){
    files=("${(@f)$(fd -aI $1)}")
    arr=();
    for file in $files; do
        arr+=("file:/$file\n");
    done
    echo $arr | wl-copy clipboard -t text/uri-list
}
