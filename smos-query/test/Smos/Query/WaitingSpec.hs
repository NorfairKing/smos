module Smos.Query.WaitingSpec (spec) where

import Smos.Query.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = sequential $
  modifyMaxSuccess (`div` 50) $ -- The first test will be empty, the second will not
    describe "Waiting" $
      it "'just works' for any InterestingStore" $
        forAllValid $
          \is -> testSmosQuery is ["waiting"]
