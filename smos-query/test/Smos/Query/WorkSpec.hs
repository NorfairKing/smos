{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.WorkSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import Smos.Query.Config
import Smos.Query.Default
import Smos.Query.TestUtils
import Smos.Report.Filter
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = sequential $
  modifyMaxSuccess (`div` 25) $ -- The first test will be empty, the second will not
    describe "Work" $ do
      it "'just works' for any InterestingStore" $
        forAllValid $ \is -> testSmosQuery is ["work"]
      it "'just works' for any InterestingStore and simple context" $
        forAllValid $ \is -> do
          let onlineFilterString = "(tag:online or not:tag:offline)"
          let offlineFilterString = "(not:tag:online or tag:offline)"
          case (,) <$> parseEntryFilterRel onlineFilterString <*> parseEntryFilterRel offlineFilterString of
            Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
            Right (onlineFilter, offlineFilter) -> do
              let wc =
                    defaultWorkReportConfig
                      { workReportConfigContexts =
                          M.fromList
                            [ (ContextName "online", onlineFilter),
                              (ContextName "offline", offlineFilter)
                            ]
                      }
                  rc = defaultReportConfig {smosReportConfigWorkConfig = wc}
                  sqc = defaultSmosQueryConfig {smosQueryConfigReportConfig = rc}
              testSmosQueryWithConfig sqc is ["work", "online"]
