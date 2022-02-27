import Control.Monad (forM_)
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
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import qualified XMonad.Layout.IndependentScreens as W
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

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

layout = lessBorders Screen (avoidStruts tiled)
  where
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
    ("M-M1-l", sendMessage Expand)
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

main :: IO ()
main = do
  xmonad $
    docks $
      ewmhFullscreen $
        ewmh $
          pagerHints $
            myConfig `additionalKeysP` myKeysP
              `additionalKeys` myKeys
