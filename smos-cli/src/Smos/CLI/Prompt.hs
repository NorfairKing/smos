{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.CLI.Prompt
  ( YesNo (..),
    promptYesNo,
    promptUntil,
    promptSecret,
    promptSecretUntil,
    prompt,
    -- Internals
    yesNoPromptText,
    parseYesNo,
  )
where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Text
import qualified Data.Text.IO as T
import Data.Validity
import GHC.Generics (Generic)
import System.IO (hFlush, hGetEcho, hSetEcho, stdin, stdout)

promptYesNo :: YesNo -> Text -> IO YesNo
promptYesNo def p = do
  rs <- prompt $ p <> " " <> yesNoPromptText def
  pure $ fromMaybe def $ parseYesNo rs

data YesNo
  = Yes
  | No
  deriving (Show, Eq, Generic)

instance Validity YesNo

yesNoPromptText :: YesNo -> Text
yesNoPromptText =
  \case
    Yes -> "[Y/n]"
    No -> "[y/N]"

parseYesNo :: Text -> Maybe YesNo
parseYesNo =
  \case
    "yes" -> pure Yes
    "y" -> pure Yes
    "no" -> pure No
    "n" -> pure No
    _ -> Nothing

promptUntil :: Text -> (Text -> Maybe a) -> IO a
promptUntil p = promptRawUntil p prompt

promptSecretUntil :: Text -> (Text -> Maybe a) -> IO a
promptSecretUntil p = promptRawUntil p promptSecret

promptRawUntil :: Text -> (Text -> IO Text) -> (Text -> Maybe a) -> IO a
promptRawUntil p pf func = do
  s <- pf p
  case func s of
    Nothing -> promptUntil p func
    Just a -> pure a

prompt :: Text -> IO Text
prompt = promptRaw True

promptSecret :: Text -> IO Text
promptSecret = promptRaw False

promptRaw :: Bool -> Text -> IO Text
promptRaw b p = do
  T.putStr p
  T.putStr " > "
  hFlush stdout
  pass <- withEcho b T.getLine
  unless b $ putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
