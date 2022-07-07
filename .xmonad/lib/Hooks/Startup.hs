-- |
module Hooks.Startup where

import Data.Foldable
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

myStartupHook =
  spawnAllOnce
    [ "pasystray",
      "redshift-gtk",
      "/usr/lib/kdeconnectd",
      "~/.dropbox-dist/dropboxd",
      "dunst",
      "flameshot",
      -- "emacs -nw --daemon",
      "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
      "blueman-applet",
      "picom --experimental-backends",
      -- "xcompmgr",
      "guake",
      "fcitx -d"
    ]
    <+> spawnAndDoOnce (doShift "1_10") "telegram-desktop"
    <+> spawnAndDoOnce (doShift "1_10") "sleep 5;/mnt/md126/Downloads/OpenHab-linux/OpenHab"

spawnAllOnce xs =
  forM_ xs spawnOnce
