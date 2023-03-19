{-# LANGUAGE OverloadedStrings #-}

module Smos.JobHunt.Command.InitSpec (spec) where

import Control.Monad.Logger
import Data.Maybe
import Data.Time
import Path
import Path.IO
import Smos.Data
import Smos.Data.TestUtils
import Smos.JobHunt.Command.Init
import Smos.JobHunt.OptParse.Types
import Test.Syd

spec :: Spec
spec = do
  describe "initSmosFilePath" $
    it "makes a filesystem-friendly path" $
      initSmosFilePath "CS SYD" `shouldBe` parseRelFile "cs-syd.smos"
  describe "initSmosFile" $
    it "makes the same smos file as before" $
      goldenSmosFile "test_resources/init.smos" $
        case initSmosFile (UTCTime (fromGregorian 2023 03 20) (timeOfDayToTime (TimeOfDay 07 14 00))) "CS SYD" (Just "https://cs-syd.eu") (Just "syd@cs-syd.eu") Nothing of
          Nothing -> expectationFailure "Failed to make the init smos file."
          Just smosFile -> pure smosFile
  describe "smosJobHuntInit" $
    it "can initialise an application project file" $
      withSystemTempDir "smos-jobhunt-test" $ \td -> do
        let initSettings =
              InitSettings
                { initSettingCompany = "CS SYD",
                  initSettingContactName = Just "Tom Sydney Kerckhove",
                  initSettingContactEmail = Just "syd@cs-syd.eu",
                  initSettingUrl = Just "https://cs-syd.eu"
                }
        pf <- runNoLoggingT $ initApplicationProject td Nothing initSettings
        mSf <- readSmosFile pf
        mSf `shouldSatisfy` isJust
