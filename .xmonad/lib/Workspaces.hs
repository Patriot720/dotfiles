-- |

module Workspaces where
import XMonad.Layout.IndependentScreens
import Data.List (nub)

myWorkspaces :: [PhysicalWorkspace]
myWorkspaces =
  withScreen 0 ["1", "2", "3", "4", "5"]
    ++ withScreen 1 ["6", "7", "8", "9", "10"]

myWorkspaces' :: [VirtualWorkspace]
myWorkspaces' = nub $ map unmarshallW myWorkspaces
