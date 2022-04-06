{-# LANGUAGE FlexibleInstances #-}

module AndrewConfig where
import XMonad
import Layouts (myLayout)
import Workspaces (myWorkspaces)
import XMonad.Actions.UpdatePointer (updatePointer)
import Hooks.Startup (myStartupHook)
import XMonad.Hooks.Minimize (minimizeEventHook)
import Hooks.ManageHook (myManageHook)
import XMonad.Hooks.ManageDocks (docks)
import Ewmh (myEwmh)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import Keys (myKeys)

instance Show (X ()) where
  show f = "Kekw"

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = myLayout,
      terminal = "alacritty",
      workspaces = myWorkspaces,
      logHook = updatePointer (0.5, 0.5) (0, 0),
      startupHook = myStartupHook,
      handleEventHook = minimizeEventHook,
      manageHook = myManageHook
    }

mainConfig = do
  xmonad $
    docks $
      myEwmh $
        pagerHints $
          myKeys myConfig
