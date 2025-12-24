module Test.Directory.Internal.ActionSpec where

import Test.Prelude

import Test.Spec (around_)
import Webb.Directory.Data.Absolute as Abs
import Webb.Directory.Internal.Action as Action


spec :: Spec Unit
spec = describe "Test internal directory actions" do 
  around_ resetTestDir do 
    run "can detect existing dir" do
      pure unit

  where
  run msg prog = do 
    it msg do
      Action.eval {} do 
        prog
        pure unit

  testDir = Abs.new [] "test-dirs"
  path string = Abs.new ["test-dirs"] string
  
  resetTestDir runTest = do
    Action.eval {} $ Action.resetDir testDir