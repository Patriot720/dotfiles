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
      "emacs -nw --daemon",
      "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
      "blueman-applet",
      "xcompmgr",
      "guake",
      "fcitx -d"
    ]
    <+> spawnAndDo (doShift "1_10") "telegram-desktop"
    <+> spawnAndDo (doShift "1_10") "sleep 1;/mnt/md126/Downloads/OpenHab-linux/OpenHab"
    <+> spawnAndDo (doShift "1_10") "youtube-music"

spawnAllOnce xs =
  forM_ xs spawnOnce
