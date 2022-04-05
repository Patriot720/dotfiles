#!/bin/sh
KEYMAP=$1
( echo "$KEYMAP"                                   \
    ; cat ~/.xmonad/lib/AndrewConfig.hs                          \
    | awk '/^'"$KEYMAP"'/,/]$/'                      \
    | sed -r '1d;$d;s/.*xK_(.)\), (.*)\)$/\1 -> \2/' \
    | column | expand ; cat ) | dzen2 -l 5 -e onstart=uncollapse
