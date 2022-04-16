-- |
module Hooks.ManageHook where
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import Scratchpads
import XMonad.Hooks.InsertPosition
import XMonad.Config.Dmwit (floatAll)
import XMonad.Layout.NoBorders (hasBorder)
import qualified Debug.Trace
import Data.List

contains :: (Eq a, Functor m, Show a) => m [a] -> [a] -> m Bool
q `contains` x = fmap (\s -> Debug.Trace.trace (show x ++ " isInfixOf " ++ show s ++ " " ++ show (x `isInfixOf` s)) (x `isInfixOf` s)) q

myManageHook =
  composeAll
    [ namedScratchpadManageHook myScratchpads,
      insertPosition Below Newer,
      floatAll
        [ "Guake",
          "Pavucontrol"
        ],
      -- https://stackoverflow.com/questions/26028146/xmonad-open-a-window-into-a-particular-tile
      className =? "TelegramDesktop" --> doShift "1_10",
      className =? "discord" --> doShift "1_8",
      className =? "qBittorrent" --> doShift "1_8",
      className =? "Guake" --> hasBorder False,
      className =? "Guake" --> doFloat,
      className =? "Gnome-boxes" --> doFloat,
      className =? "gnome-boxes" --> doFloat,
      className =? "Boxes" --> doFloat,
      title =? "emacs-everywhere" --> doFloat,
      title =? "org-roam-everywhere" --> doFloat,
      className =? "Blueman-manager" --> doCenterFloat,
      className =? "Steam" --> doFloat,
      className =? "steam" --> doFullFloat,
      className =? "YouTube Music" --> doShift "1_10" <+> doF W.swapUp,
      className =? "openhab-nativefier-9825c2" --> doShift "1_10",
      isDialog --> doF W.shiftMaster <+> doF W.swapDown,
      isFullscreen --> doFullFloat
    ]
