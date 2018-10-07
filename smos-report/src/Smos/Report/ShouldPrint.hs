{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ShouldPrint where

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

printErrorMessage :: ShouldPrint -> String -> IO ()
printErrorMessage DontPrint = const $ pure ()
printErrorMessage PrintWarning = putStrLn
printErrorMessage PrintError = error
