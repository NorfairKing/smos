{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ShouldPrint where

import Data.Configurator.Types

import qualified Data.Text as T

data ShouldPrint
    = PrintError
    | PrintWarning
    | DontPrint
    deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint "error" = Just PrintError
parseShouldPrint "warning" = Just PrintWarning
parseShouldPrint "nothing" = Just DontPrint
parseShouldPrint _ = Nothing

instance Configured ShouldPrint where
    convert (String t) = parseShouldPrint $ T.unpack t
    convert _ = Nothing

printErrorMessage :: ShouldPrint -> String -> IO ()
printErrorMessage DontPrint = const $ pure ()
printErrorMessage PrintWarning = putStrLn
printErrorMessage PrintError = error
