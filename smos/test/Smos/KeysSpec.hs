{-# LANGUAGE TypeApplications #-}

module Smos.KeysSpec where

import TestImport

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Smos.Keys
import Smos.Keys.Gen ()
import Smos.OptParse.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Key
  genValidSpec @Key
  jsonSpecOnValid @Key
  eqSpecOnValid @Modifier
  genValidSpec @Modifier
  jsonSpecOnValid @Modifier
  eqSpecOnValid @KeyPress
  genValidSpec @KeyPress
  jsonSpecOnValid @KeyPress
  describe "keyP" $ parsesValidSpec keyP genValid
  describe "modifierP" $ parsesValidSpec modifierP genValid
  describe "keyPressP" $ parsesValidSpec keyPressP genValid

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseNothingSpec :: (Show a, Eq a) => P a -> Text -> Spec
parseNothingSpec p s = it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Gen Text -> Spec
parsesValidSpec p gen = it "only parses valid values" $ forAll gen $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $ unlines ["P failed on input", show s, "with error", parseErrorPretty err]
    Right out -> out `shouldBe` res

parseNothing :: (Show a, Eq a) => P a -> Text -> Expectation
parseNothing p s =
  case parse (p <* eof) "test input" s of
    Right v ->
      expectationFailure $
      unlines ["P succeeded on input", show s, "at parsing", show v, "but it should have failed."]
    Left _ -> pure ()

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Property
parsesValid p s =
  let (useful, ass) =
        case parse (p <* eof) "test input" s of
          Left _ -> (False, (pure () :: IO ()))
          Right out -> (True, shouldBeValid out)
   in cover useful 10 "useful" $ property ass
