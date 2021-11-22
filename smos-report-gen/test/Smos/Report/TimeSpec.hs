{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.TimeSpec
  ( spec,
  )
where

import Cursor.Forest.Gen ()
import Data.Text (Text, pack)
import Smos.Report.Time
import Smos.Report.Time.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Megaparsec

spec :: Spec
spec = do
  eqSpec @Time
  ordSpec @Time
  genValidSpec @Time
  jsonSpec @Time
  describe "timeP" $ do
    parsesValidSpec timeP
    it "parses is correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "s") (Seconds i)
    it "parses i sec correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "sec") (Seconds i)
    it "parses i secs correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "secs") (Seconds i)
    it "parses i second correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "second") (Seconds i)
    it "parses i seconds correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "seconds") (Seconds i)
    it "parses im correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "m") (Minutes i)
    it "parses i min correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "min") (Minutes i)
    it "parses i mins correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "mins") (Minutes i)
    it "parses i minute correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "minute") (Minutes i)
    it "parses i minutes correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "minutes") (Minutes i)
    it "parses ih correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "h") (Hours i)
    it "parses i hr correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "hr") (Hours i)
    it "parses i hrs correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "hrs") (Hours i)
    it "parses i hour correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "hour") (Hours i)
    it "parses i hours correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "hours") (Hours i)
    it "parses id correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "d") (Days i)
    it "parses i day correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "day") (Days i)
    it "parses i days correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "days") (Days i)
    it "parses iw correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "w") (Weeks i)
    it "parses i wk correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "wk") (Weeks i)
    it "parses i wks correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "wks") (Weeks i)
    it "parses i week correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "week") (Weeks i)
    it "parses i weeks correctly" $
      forAllValid $ \i -> parseJust timeP (pack $ show i ++ "weeks") (Weeks i)
  describe "renderTime" $ do
    it "produces valid texts" $ producesValid renderTime
    it "renders times that parse to the same" $ forAllValid $ \s -> parseJust timeP (renderTime s) s

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
