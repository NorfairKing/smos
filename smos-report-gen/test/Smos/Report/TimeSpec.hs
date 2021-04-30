{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.TimeSpec
  ( spec,
  )
where

import Cursor.Forest.Gen ()
import Data.Text (Text)
import Smos.Report.Time
import Smos.Report.Time.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Megaparsec

spec :: Spec
spec = do
  eqSpecOnValid @Time
  ordSpecOnValid @Time
  genValidSpec @Time
  jsonSpecOnValid @Time
  describe "timeP" $ do
    parsesValidSpec timeP
    it "parses 2s correctly" $ parseJust timeP "2s" (Seconds 2)
    it "parses 2 sec correctly" $ parseJust timeP "2 sec" (Seconds 2)
    it "parses 2 secs correctly" $ parseJust timeP "2 secs" (Seconds 2)
    it "parses 2 second correctly" $ parseJust timeP "2 second" (Seconds 2)
    it "parses 2 seconds correctly" $ parseJust timeP "2 seconds" (Seconds 2)
    it "parses 3w correctly" $ parseJust timeP "3w" (Weeks 3)
    it "parses 3 wk correctly" $ parseJust timeP "3 wk" (Weeks 3)
    it "parses 3 wks correctly" $ parseJust timeP "3 wks" (Weeks 3)
    it "parses 3 week correctly" $ parseJust timeP "3 week" (Weeks 3)
    it "parses 3 weeks correctly" $ parseJust timeP "3 weeks" (Weeks 3)
    it "parses 4d correctly" $ parseJust timeP "4d" (Days 4)
    it "parses 4 day correctly" $ parseJust timeP "4 day" (Days 4)
    it "parses 4 days correctly" $ parseJust timeP "4 days" (Days 4)
    it "parses 5h correctly" $ parseJust timeP "5h" (Hours 5)
    it "parses 5 hr correctly" $ parseJust timeP "5 hr" (Hours 5)
    it "parses 5 hrs correctly" $ parseJust timeP "5 hrs" (Hours 5)
    it "parses 5 hours correctly" $ parseJust timeP "5 hours" (Hours 5)
    it "parses 6m correctly" $ parseJust timeP "6m" (Minutes 6)
    it "parses 6 min correctly" $ parseJust timeP "6 min" (Minutes 6)
    it "parses 6 mins correctly" $ parseJust timeP "6 mins" (Minutes 6)
    it "parses 6 minute correctly" $ parseJust timeP "6 minute" (Minutes 6)
    it "parses 6 minutes correctly" $ parseJust timeP "6 minutes" (Minutes 6)
  describe "renderTime" $ do
    it "produces valid texts" $ producesValidsOnValids renderTime
    it "renders bys that parse to the same" $ forAllValid $ \s -> parseJust timeP (renderTime s) s

parsesValidSpec :: (Show a, Validity a) => P a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
        unlines ["P failed on input", show s, "with error", errorBundlePretty err]
    Right out -> out `shouldBe` res

parsesValid :: (Show a, Validity a) => P a -> Text -> Expectation
parsesValid p s =
  case parse (p <* eof) "test input" s of
    Left _ -> pure ()
    Right out -> shouldBeValid out
