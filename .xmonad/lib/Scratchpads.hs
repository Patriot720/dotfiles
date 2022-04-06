-- |

module Scratchpads where
import XMonad.Util.NamedScratchpad
import XMonad
import XMonad.Hooks.ManageHelpers

myScratchpads =
  [ -- NS "calc" "gnome-calculator" (className =? "Gnome-calculator") defaultFloating
    NS "calc" "speedcrunch" (className =? "SpeedCrunch") doCenterFloat
  ]
