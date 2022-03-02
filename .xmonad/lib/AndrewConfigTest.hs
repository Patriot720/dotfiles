module AndrewConfigTest where
import Test.HUnit
import XMonad (screenWorkspace)

tests = TestList [
  TestCase $ do
      (Just workspaceId) <- screenWorkspace 0
      assertEqual ""  workspaceId 1
  ]
