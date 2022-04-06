-- |
module Ewmh where

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

myActivateHook :: ManageHook
myActivateHook =
  className /=? "YouTube Music" --> doFocus

mySort = getSortByXineramaRule

myRename :: String -> WindowSpace -> String
myRename s _w = unmarshallW s

myFilter = filterOutWs [scratchpadWorkspaceTag]

myEwmh :: XConfig a -> XConfig a
myEwmh c =
  ewmhFullscreen $
    ewmh $
      setEwmhActivateHook myActivateHook $
        addEwmhWorkspaceSort (pure myFilter) $
          addEwmhWorkspaceRename (pure myRename) c
