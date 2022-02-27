import Control.Monad (forM_)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Traversable (for)
import System.Taffybar.Support.PagerHints
import XMonad
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceCursors (workspaceCursors)
import XMonad.Config.Dmwit (floatAll)
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Hooks.Focus as W
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.AutoMaster as XLA
import XMonad.Layout.Drawer (Property (ClassName, Or), drawer, onRight, onTop, simpleDrawer)
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import qualified XMonad.Layout.IndependentScreens as W
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.NoBorders
import XMonad.Layout.StackTile (StackTile (StackTile))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = layout,
      focusFollowsMouse = False,
      terminal = "alacritty",
      workspaces =
        withScreen 0 ["1", "2", "3", "4", "5"]
          ++ withScreen 1 ["6", "7", "8", "9", "10"],
      logHook = updatePointer (0.5, 0.5) (0, 0),
      startupHook =
        composeAll
          [ spawnAllOnce
              [ "status-notifier-watcher"
              -- "redshift-gtk",
              -- "/usr/lib/kdeconnectd",
              -- "~/.dropbox-dist/dropboxd",
              -- "dunst",
              -- "env LD_PRELOAD=/usr/lib/spotify-adblock.so spotify %U",
              -- "telegram-desktop",
              -- "flameshot",
              -- "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
              -- "blueman-applet",
              -- "fcitx -d"
              ],
            spawn "pkill taffybar && taffybar &"
          ],
      manageHook =
        composeAll
          [ floatAll
              [ "Guake",
                "Pavucontrol"
              ],
            className =? "Telegram" --> doShift "1_9",
            className =? "discord" --> doShift "1_8",
            className =? "qBittorrent" --> doShift "1_8",
            className =? "Spotify" --> doShift "1_10",
            insertPosition End Newer
          ]
    }

layout = lessBorders Screen (avoidStruts (tiled ||| Full))
  where
    consoleOn i = simpleDrawer 0 0.5 (ClassName "Nautilus" `Or` ClassName "Xchat") `onRight` i
    testLayout = consoleOn (Tall 1 0.03 0.5)
    stackTile = StackTile 1 (3 / 100) (1 / 2)
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio = 3 / 4
    delta = 3 / 100

spawnAllOnce xs =
  forM_ xs spawnOnce

myKeysP =
  [ ("M-d", spawn "~/.config/rofi/launchers/colorful/launcher.sh"),
    ("M-c", spawn "~/.config/rofi/launchers/text/calc.sh"),
    ("M-x", kill),
    ("F12", spawn "guake-toggle"),
    ("M-0", windows $ W.view "10"),
    ("M-M1-h", sendMessage Shrink),
    ("M-M1-l", sendMessage Expand),
    ("XF86AudioRaiseVolume", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status"),
    ("XF86AudioLowerVolume", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status"),
    ("XF86AudioMute", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status"),
    ("XF86AudioMicMute", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status"),
    ("XF86MonBrightnessUp", spawn "brightnessctl s +10%"),
    ("XF86MonBrightnessDown", spawn "brightnessctl s 10%-"),
    ("XF86AudioPlay", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"),
    ("XF86AudioPause", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause"),
    ("XF86AudioNext", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"),
    ("XF86AudioPrev", spawn "--no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
  ]

shiftThenView i = W.view i . W.shift i

myKeys =
  [ ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' myConfig) [xK_1 .. xK_9],
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
    ++ [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_h, xK_l] [0 ..],
           (f, m) <- [(W.view, 0), (shiftThenView, shiftMask)]
       ]

mySort = getSortByXineramaRule

myRename :: String -> WindowSpace -> String
myRename s _w = last $ splitOn "_" s

main :: IO ()
main = do
  xmonad $
    docks $
      ewmhFullscreen $
        addEwmhWorkspaceRename (pure myRename) $
          ewmh $
            pagerHints $
              myConfig `additionalKeysP` myKeysP
                `additionalKeys` myKeys
