{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.WorkSpec (spec) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smos.Query.OptParse.Types as Query
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
      it "'just works' for any InterestingStore and simple context" $
        forAllValid $ \is -> do
          let onlineFilterString = "(tag:online or not:tag:offline)"
          let offlineFilterString = "(not:tag:online or tag:offline)"
          case (,) <$> parseEntryFilter onlineFilterString <*> parseEntryFilter offlineFilterString of
            Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
            Right (onlineFilter, offlineFilter) -> do
              let wc =
                    Report.defaultWorkReportConfiguration
                      { Report.workReportConfContexts =
                          Just $
                            M.fromList
                              [ (ContextName "online", onlineFilter),
                                (ContextName "offline", offlineFilter)
                              ]
                      }
                  rc = Report.defaultConfiguration {Report.confWorkReportConf = Just wc}
                  c = Query.defaultConfiguration {confReportConf = rc}
              testSmosQueryWithConfig c is ["work", "online"]
      it "'just works' for any InterestingStore and a check but no contexts" $
        forAllValid $ \is -> do
          let checkString = "property:timewindow"
          case parseEntryFilter checkString of
            Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
            Right checkFilter -> do
              let wc =
                    Report.defaultWorkReportConfiguration
                      { Report.workReportConfChecks = Just $ S.singleton checkFilter
                      }
                  rc = Report.defaultConfiguration {Report.confWorkReportConf = Just wc}
                  c = Query.defaultConfiguration {confReportConf = rc}
              testSmosQueryWithConfig c is ["work"]
