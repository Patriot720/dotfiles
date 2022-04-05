{-# LANGUAGE FlexibleInstances #-}

module AndrewConfig where

import Control.Monad (forM_, join)
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Traversable (for)
import qualified Debug.Trace
import System.Taffybar.Support.PagerHints
import XMonad
import XMonad.Actions.AfterDrag
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.FloatSnap
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
import XMonad.Hooks.ManageHelpers
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
import XMonad.Layout.SimpleFloat
import XMonad.Layout.StackTile (StackTile (StackTile))
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.WorkspaceCompare (filterOutWs, getSortByXineramaRule)
import XMonad.Actions.Submap

instance Show (X ()) where
  show f = "Kekw"

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicMove (Just 50) (Just 50) w)),
      ((modm .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)),
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> ifClick (snapMagicResize [R, D] (Just 50) (Just 50) w)),
      -- ( (modm, button1),
      --   \w ->
      --     focus w >> mouseMoveWindow w
      --       >> windows W.shiftMaster
      -- ),
      -- -- mod-button2, Raise the window to the top of the stack
      -- ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- -- mod-button3, Set the window to floating mode and resize by dragging
      -- ( (modm, button3),
      --   \w ->
      --     focus w >> mouseResizeWindow w
      --       >> windows W.shiftMaster
      -- ),
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
      ((modm, button4), \w -> focus w >> windows W.focusDown),
      ((modm .|. altMask, button4), \w -> focus w >> windows W.swapDown),
      ((modm .|. altMask, button5), \w -> focus w >> windows W.swapUp),
      ((modm, button5), \w -> focus w >> windows W.focusUp)
    ]

contains :: (Eq a, Functor m, Show a) => m [a] -> [a] -> m Bool
q `contains` x = fmap (\s -> Debug.Trace.trace (show x ++ " isInfixOf " ++ show s ++ " " ++ show (x `isInfixOf` s)) (x `isInfixOf` s)) q

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
        spawnAllOnce
          [ "pasystray",
            "redshift-gtk",
            "/usr/lib/kdeconnectd",
            "~/.dropbox-dist/dropboxd",
            "dunst",
            "youtube-music",
            "telegram-desktop",
            "flameshot",
            "emacs -nw --daemon",
            "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
            "blueman-applet",
            "xcompmgr",
            "guake",
            "fcitx -d"
          ],
      handleEventHook =
        minimizeEventHook,
      manageHook =
        composeAll
          [ namedScratchpadManageHook scratchpads,
            insertPosition Below Newer,
            floatAll
              [ "Guake",
                "Pavucontrol"
              ],
            className =? "TelegramDesktop" --> doShift "1_10",
            className =? "discord" --> doShift "1_8",
            className =? "qBittorrent" --> doShift "1_8",
            className =? "Guake" --> hasBorder False,
            className =? "Guake" --> doFloat,
            title =? "emacs-everywhere" --> doFloat,
            title =? "org-roam-everywhere" --> doFloat,
            -- title `contains` "org-capture" --> doFloat,
            -- "Emacs Everywhere" `isInProperty` "WM_NAME" --> doFloat,
            -- title ^? "Emacs Everywhere" --> doFloat,
            -- className =? "Org.gnome.Nautilus" --> doFloat,
            -- className =? "Gnome-calculator" --> doCenterFloat,
            className =? "Steam" --> doFloat,
            className =? "steam" --> doFullFloat,
            className =? "YouTube Music" --> doShift "1_10",
            isDialog --> doF W.shiftMaster <+> doF W.swapDown
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
  onWorkspace "1_10" (stackTile ||| Full) $
    lessBorders Screen $
      avoidStrutsOn [D] $
        minimize $
          boringWindows $ -- drawer `onLeft`
            tiled ||| tabbedBottom shrinkText myTabConfig ||| Full
  where
    -- drawer = Layout.drawer 0.0 0.4 (ClassName "Spotify" `Or` ClassName "Telegram" `Or` ClassName "Org.gnome.Nautilus") Full
    stackTile = minimize $ boringWindows $ avoidStruts $ TwoPane (3 / 100) (1 / 2)
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta 0.6
    nmaster = 1
    ratio = 0.7
    delta = 3 / 100

spawnAllOnce xs =
  forM_ xs spawnOnce

toggleMaximization :: Maybe Xmonad.Config.Prime.Window -> X ()
toggleMaximization window =
  case window of
    Nothing -> withFocused minimizeWindow
    Just w -> maximizeWindowAndFocus w

killAllOtherCopiesForWindowSet ss =
  whenJust (W.peek ss) $ \w ->
    windows $
      W.view (W.currentTag ss)
        . delFromAllButCurrent w
  where
    delFromAllButCurrent w ss =
      foldr
        (delWinFromWorkspace w . W.tag)
        ss
        (W.hidden ss ++ map W.workspace (W.visible ss))
    delWinFromWorkspace w wid = viewing wid $ W.modify Nothing (W.filter (/= w))

    viewing wis f ss = W.view (W.currentTag ss) $ f $ W.view wis ss

-- copyToSecondScreen ::  a -> a
copyToSecondScreen s =
  foldr copy s (withScreen 1 ["6", "7", "8", "9", "10"])

-- if null (copiesOfOn (W.peek s) (taggedWindows $ W.hidden s))
-- then
-- else s
-- TODO toggle

scratchpads =
  [ -- NS "calc" "gnome-calculator" (className =? "Gnome-calculator") defaultFloating
    NS "calc" "speedcrunch" (className =? "SpeedCrunch") doCenterFloat
  ]

myKeysP =
  [ ("M-d", spawn "~/.config/rofi/launchers/colorful/launcher.sh"),
    ("M-c", namedScratchpadAction scratchpads "calc"),
    -- spawn "~/.config/rofi/launchers/text/calc.sh"
    ("M-x", kill),
    ("F12", spawn "guake-toggle"),
    -- ("M-0", windows $ W.view "1_10"),
    -- ("M-M1-0", windows $ W.shift "1_10"),
    ("M-S-h", sendMessage Shrink),
    ("M-S-l", sendMessage Expand),
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
    ("<Print>", spawn "flameshot gui --delay=1000"),
    ("C-<Print>", spawn "fish -c 'flameshot full -c'"),
    ("<Pause>", spawn "fish -c 'record_region'"),
    ("C-<Pause>", spawn "fish -c 'record_screen'"),
    ("M-z", withLastMinimized' toggleMaximization),
    ("M-t", withFocused toggleFloat),
    ("M-e", spawn "emacsclient --eval \"(emacs-everywhere)\""),
    ("M-a", windows copyToSecondScreen),
    ("M-S-a", killAllOtherCopies),
    ("M-S-t", spawn "killall my-taffybar;my-taffybar")
  ]

shiftThenView i = W.view i . W.shift i

screenShiftThenView i = focusScreen (unmarshallS i) . shiftThenView i

screenView i = focusScreen (unmarshallS i) . W.view i

myActivateHook :: ManageHook
myActivateHook =
  className /=? "YouTube Music" --> doFocus

centerWindow :: Window -> X ()
centerWindow win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
  return ()

toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect 0.15 0.15 0.65 0.65) s
    )

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
         ((mod4Mask .|. altMask, xK_k), windows W.swapUp),
         ((mod4Mask, xK_Tab), toggleRecentWS),
         ((mod4Mask, xK_f), submap . M.fromList $
         [ ((mod4Mask, xK_l), withFocused $ snapMove R Nothing),
           ((mod4Mask, xK_h), withFocused $ snapMove L Nothing),
           ((mod4Mask, xK_k), withFocused $ snapMove U Nothing),
           ((mod4Mask, xK_j), withFocused $ snapMove D Nothing)
         ]),
         ((mod4Mask, xK_Left), withFocused $ snapMove L Nothing),
         ((mod4Mask, xK_Right), withFocused $ snapMove R Nothing),
         ((mod4Mask, xK_Up), withFocused $ snapMove U Nothing),
         ((mod4Mask, xK_Down), withFocused $ snapMove D Nothing)
       ]

mySort = getSortByXineramaRule

myRename :: String -> WindowSpace -> String
myRename s _w = unmarshallW s

myFilter = filterOutWs [scratchpadWorkspaceTag]

mainConfig = do
  xmonad $
    docks $
      ewmhFullscreen $
        ewmh $
          setEwmhActivateHook myActivateHook $
            addEwmhWorkspaceSort (pure myFilter) $
              addEwmhWorkspaceRename (pure myRename) $
                pagerHints $
                  myConfig `additionalKeysP` myKeysP
                    `additionalKeys` myKeys
