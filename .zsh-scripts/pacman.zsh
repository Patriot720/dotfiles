#!/usr/bin/env zsh

alias list-packages='expac -H M "%011m\t%-20n\t%10d" $(comm -23 <(yay -Qqen | sort) <({ yay -Qqe; expac -l "\n" "%E" base; } | sort | uniq)) | sort -n | less'
