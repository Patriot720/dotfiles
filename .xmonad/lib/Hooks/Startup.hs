-- |
module Hooks.Startup where
import Data.Foldable
import XMonad.Util.SpawnOnce

myStartupHook =
  spawnAllOnce
    [ "pasystray",
      "redshift-gtk",
      "/usr/lib/kdeconnectd",
      "~/.dropbox-dist/dropboxd",
      "dunst",
      "telegram-desktop",
      "youtube-music",
      "flameshot",
      "emacs -nw --daemon",
      "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
      "blueman-applet",
      "xcompmgr",
      "guake",
      "fcitx -d"
    ]

spawnAllOnce xs =
  forM_ xs spawnOnce
