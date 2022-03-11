{-# LANGUAGE FlexibleInstances #-}

module AndrewConfig where

import Control.Monad (forM_, join)
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Traversable (for)
import System.Taffybar.Support.PagerHints
import XMonad
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.Minimize (maximizeWindow, maximizeWindowAndFocus, minimizeWindow, withLastMinimized, withLastMinimized')
import XMonad.Actions.OnScreen
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceCursors (workspaceCursors)
import XMonad.Config.Dmwit (altMask, floatAll, ppWorkspaces)
import qualified XMonad.Config.Prime as Xmonad.Config.Prime
import XMonad.Hooks.DynamicLog (xmobarPP)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Hooks.Focus as W
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDebug
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.StatusBar.PP (PP (ppExtras))
import XMonad.Layout.BoringWindows
import XMonad.Layout.Drawer (Property (ClassName, Or), onBottom, onLeft, onRight, onTop, simpleDrawer)
import qualified XMonad.Layout.Drawer as Layout
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import qualified XMonad.Layout.IndependentScreens as W
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.StackTile (StackTile (StackTile))
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

instance Show (X ()) where
  show f = "Kekw"

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      ),
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
      ((modm, button4), \w -> focus w >> windows W.focusDown),
      ((modm, button5), \w -> focus w >> windows W.focusUp)
    ]

myConfig =
  def
    { modMask = mod4Mask,
      mouseBindings = myMouseBindings,
      layoutHook = layout,
      terminal = "alacritty",
      workspaces =
        withScreen 0 ["1", "2", "3", "4", "5"]
          ++ withScreen 1 ["6", "7", "8", "9", "10"],
      logHook = updatePointer (0.5, 0.5) (0, 0),
      startupHook =
        -- spawn "killall my-taffybar;my-taffybar" >>
        spawnAllOnce
            ["pasystray",
              "redshift-gtk",
              "/usr/lib/kdeconnectd",
              "~/.dropbox-dist/dropboxd",
              "dunst",
              "env LD_PRELOAD=/usr/lib/spotify-adblock.so spotify %U",
              "telegram-desktop",
              "flameshot",
              "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
              "blueman-applet",
              "guake",
              "fcitx -d"
            ],
      -- spawn "~/.config/polybar/launch.sh",
      handleEventHook =
        dynamicPropertyChange
          "WM_CLASS"
          ( composeAll
              [title =? "Spotify" --> doShift "1_10",
               insertPosition Master Newer
              ]
          )
          <+> minimizeEventHook,
      manageHook =
        composeAll
          [ floatAll
              [ "Guake",
                "Pavucontrol"
              ],
            className =? "TelegramDesktop" --> doShift "1_10",
            className =? "discord" --> doShift "1_8",
            className =? "qBittorrent" --> doShift "1_8",
            className =? "Guake" --> hasBorder False,
            className =? "Guake" --> doFloat,
            className =? "Org.gnome.Nautilus" --> doFloat,
            className =? "Steam" --> doFloat
            -- insertPosition End Newer
          ]
    }

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

layout =
  onWorkspace "1_10" stackTile $
    lessBorders Screen $
      avoidStrutsOn [D] $
        minimize $
          boringWindows $ -- drawer `onLeft`
            tiled ||| tabbedBottom shrinkText myTabConfig ||| Full
  where
    -- drawer = Layout.drawer 0.0 0.4 (ClassName "Spotify" `Or` ClassName "Telegram" `Or` ClassName "Org.gnome.Nautilus") Full
    stackTile = minimize $ boringWindows $ avoidStruts $ TwoPane (3 / 100) (1 / 2)
    tiled = ResizableTall nmaster delta ratio []
    threeCol = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio = 3 / 5
    delta = 3 / 100

spawnAllOnce xs =
  forM_ xs spawnOnce

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
    ("M-z", withLastMinimized' toggleMaximization),
    ("M-S-t", spawn "killall my-taffybar;my-taffybar")
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
           (f, m) <- [(W.view, 0), (shiftThenView, altMask)]
       ]
    ++ [ ((mod4Mask .|. altMask, xK_j), windows W.swapDown),
         ((mod4Mask .|. altMask, xK_k), windows W.swapUp)
       ]

-- viewWorkspace :: PhysicalWorkspace -> X ()
-- viewWorkspace phyWs = do
--   let (screen, _virtWs) = unmarshall phyWs
--   maybeScreenWorkspace <- screenWorkspace screen
--   case maybeScreenWorkspace of
--     Nothing -> mempty -- screen not found, don't do anything
--     Just screenWs ->
--       windows
--         ( W.view phyWs -- change to the workspace we want
--             . W.view screenWs -- move focus to the appropriate screen
--         )

mySort = getSortByXineramaRule

myRename :: String -> WindowSpace -> String
myRename s _w = unmarshallW s

-- xmobarProp config =
--   withEasySB (statusBarProp "xmobar" (pure xmobarPP)) toggleStrutsKey config
myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused :: String -> String
    formatFocused x = ""
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

mainConfig = do
  xmonad $
    docks $
      ewmhFullscreen $
        ewmh $
          addEwmhWorkspaceRename (pure myRename) $
            pagerHints $
              myConfig `additionalKeysP` myKeysP
                `additionalKeys` myKeys
