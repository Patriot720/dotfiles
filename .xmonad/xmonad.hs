import Control.Monad (forM_)
import Data.Traversable (for)
import System.Taffybar.Support.PagerHints
import XMonad
import XMonad.Actions.WorkspaceCursors (workspaceCursors)
import XMonad.Config.Dmwit (floatAll)
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Hooks.Focus as W
import XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.IndependentScreens as W
import XMonad.Layout.Magnifier (magnifier)
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

layout = avoidStruts (tiled ||| Mirror tiled ||| Full ||| magnifier threeCol)
  where
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

spawnAllOnce xs =
  forM_ xs spawnOnce

main :: IO ()
main = do
  xmonad $
    docks $
      ewmhFullscreen $
        ewmh $
          pagerHints $
            def
              { modMask = mod4Mask,
                layoutHook = layout,
                focusFollowsMouse = False,
                terminal = "alacritty",
                workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"],
                startupHook =
                  spawnAllOnce
                    [ "status-notifier-watcher",
                      "redshift-gtk",
                      "/usr/lib/kdeconnectd",
                      "~/.dropbox-dist/dropboxd",
                      "dunst",
                      "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
                      "blueman-applet",
                      "fcitx -d"
                    ]
                    >> spawn "pkill taffybar && taffybar",
                manageHook =
                  composeAll
                    [ floatAll ["Guake"],
                      className =? "Telegram" --> doShift "9"
                    ]
              }
              `additionalKeysP` [ ("M-d", spawn "~/.config/rofi/launchers/colorful/launcher.sh"),
                                  ("M-c", spawn "~/.config/rofi/launchers/text/calc.sh"),
                                  ("M-x", kill),
                                  ("F12", spawn "guake-toggle"),
                                  ("M-0", windows $ W.greedyView "10")
                                ]
