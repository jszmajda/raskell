module Raskell.MainSpec where

import qualified Main as M
import SpecHelper

-- I have no idea what I'm doing
commandLineSpec :: Spec
commandLineSpec =
  describe "running" $ do
    it "works" $ do
      M.runRuby "puts \"Hello World\"" `shouldReturn` "Hello World\n"

main :: IO ()
main = hspec spec
spec :: Spec
spec = commandLineSpec
