module Keys where
import qualified XMonad.Config.Prime as Xmonad.Config.Prime
import XMonad
import XMonad.Actions.Minimize
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens
import XMonad.Config.Dmwit

toggleMaximization :: Maybe Xmonad.Config.Prime.Window -> X ()
toggleMaximization window =
  case window of
    Nothing -> withFocused minimizeWindow
    Just w -> maximizeWindowAndFocus w


myKeysP =
  [ ("M-d", spawn "~/.config/rofi/launchers/colorful/launcher.sh"),
    ("M-c", spawn "~/.config/rofi/launchers/text/calc.sh"),
    ("M-x", kill),
    ("F12", spawn "guake-toggle"),
    -- ("M-0", windows $ W.view "1_10"),
    -- ("M-M1-0", windows $ W.shift "1_10"),
    ("M-M1-h", sendMessage Shrink),
    ("M-M1-l", sendMessage Expand),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),
    ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%"),
    ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10%-"),
    ("<XF86AudioPlay>", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"),
    ("<XF86AudioPause>", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause"),
    ("<XF86AudioNext>", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"),
    ("<XF86AudioPrev>", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"),
    ("<Print>", spawn "flameshot gui"),
    ("M-z", withLastMinimized' toggleMaximization)
  ]

shiftThenView i = W.view i . W.shift i

screenShiftThenView i = focusScreen (unmarshallS i) . shiftThenView i

screenView i = focusScreen (unmarshallS i) . W.view i

myKeys =
  [ ( (m .|. mod4Mask, k),
      windows $ onCurrentScreen f i
    )
    | (i, k) <- zip (workspaces' myConfig) ([xK_1 .. xK_9] ++ [xK_0]),
      (f, m) <- [(screenView, 0), (screenShiftThenView, altMask)]
  ]
    ++ [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_h, xK_l] [0 ..],
           (f, m) <- [(W.view, 0), (shiftThenView, shiftMask)]
       ]
    ++ [ ((mod4Mask .|. altMask, xK_j), windows W.swapDown),
         ((mod4Mask .|. altMask, xK_k), windows W.swapUp)
       ]
