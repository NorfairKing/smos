{-# LANGUAGE LambdaCase #-}

module Smos.Report.ShouldPrint where

import Control.Monad.IO.Class
import System.IO

data ShouldPrint
  = PrintError
  | PrintWarning Handle
  | DontPrint
  deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint "error" = Just PrintError
parseShouldPrint "warning" = Just (PrintWarning stderr)
parseShouldPrint "nothing" = Just DontPrint
parseShouldPrint _ = Nothing

printErrorMessage :: MonadIO m => ShouldPrint -> String -> m ()
printErrorMessage = \case
  DontPrint -> const $ pure ()
  PrintWarning h -> liftIO . hPutStrLn h
  PrintError -> error
