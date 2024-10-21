{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.WorkSpec (spec) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smos.Query.OptParse as Query
import Smos.Query.TestUtils
import Smos.Report.Filter
import Smos.Report.OptParse (ContextName (..))
import qualified Smos.Report.OptParse as Report
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = sequential $
  modifyMaxSuccess (`div` 25) $ -- The first test will be empty, the second will not
    describe "Work" $ do
      it "'just works' for any InterestingStore" $
        forAllValid $ \is ->
          testSmosQuery is ["work"]
