{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Archive.Prompt
  ( YesNo (..),
    promptYesNo,
    yesNoPromptString,
    parseYesNo,
    promptRawString,
  )
where

import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)

promptYesNo :: YesNo -> String -> IO YesNo
promptYesNo def p = do
  rs <- promptRawString $ p ++ " " ++ yesNoPromptString def
  pure $ fromMaybe def $ parseYesNo rs

data YesNo
  = Yes
  | No
  deriving (Show, Eq, Generic)

instance Validity YesNo

yesNoPromptString :: YesNo -> String
yesNoPromptString =
  \case
    Yes -> "[Y/n]"
    No -> "[y/N]"

parseYesNo :: String -> Maybe YesNo
parseYesNo =
  \case
    "yes" -> pure Yes
    "y" -> pure Yes
    "no" -> pure No
    "n" -> pure No
    _ -> Nothing

promptRawString :: String -> IO String
promptRawString s = do
  putStr $ s ++ " > "
  hFlush stdout
  getLine
