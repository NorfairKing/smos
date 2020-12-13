module Smos.Query.AgendaSpec (spec) where

import Smos.Query.TestUtils
import Test.Syd

import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $ -- The first test will be empty, the second will not
  describe "Agenda" $
    it "'just works' for any InterestingStore" $
      forAllValid $
        \is -> testSmosQuery is ["agenda", "--all-time"]
