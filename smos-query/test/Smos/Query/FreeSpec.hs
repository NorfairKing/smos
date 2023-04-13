module Smos.Query.FreeSpec (spec) where

import Smos.Query.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = sequential $
  modifyMaxSuccess (`div` 25) $ -- The first test will be empty, the second will not
    describe "Work" $ do
      it "'just works' for any InterestingStore" $
        forAllValid $ \is ->
          testSmosQuery is ["free"]
