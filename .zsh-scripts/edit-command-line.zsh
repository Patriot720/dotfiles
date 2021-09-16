#!/usr/bin/env zsh

autoload -z edit-command-line
autoload zmv
zle -N edit-command-line
bindkey -M vicmd "^X^E" edit-command-line
