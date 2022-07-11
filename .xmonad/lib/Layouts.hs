-- |
module Layouts where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

myTabConfig =
  def
    { activeColor = "#556064",
      inactiveColor = "#2F3D44",
      urgentColor = "#FDF6E3",
      activeBorderColor = "#454948",
      inactiveBorderColor = "#454948",
      urgentBorderColor = "#268BD2",
      activeTextColor = "#80FFF9",
      inactiveTextColor = "#1ABC9C",
      urgentTextColor = "#1ABC9C",
      fontName = "xft:Noto Sans CJK:size=10:antialias=true"
    }

myLayout =
  onWorkspace "1_10" (stackTile ||| Full) $
    lessBorders Screen $
      avoidStrutsOn
        [D]
        (minimize $
            boringWindows $ -- drawer `onLeft`
              tiled ||| tabbedBottom shrinkText myTabConfig)
        ||| Full
  where
    -- drawer = Layout.drawer 0.0 0.4 (ClassName "Spotify" `Or` ClassName "Telegram" `Or` ClassName "Org.gnome.Nautilus") Full
    stackTile = minimize $ boringWindows $ avoidStruts $ reflectHoriz (Tall 1 0.03 0.5)

    columns = multiCol [1] 1 0.01 0.5
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta 0.6
    nmaster = 1
    ratio = 0.7
    delta = 3 / 100
