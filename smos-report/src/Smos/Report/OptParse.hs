{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse where

import System.Environment

import Control.Monad

import Path.IO

import Smos.Report.Config

import Options.Applicative

import Smos.Report.OptParse.Types

combineToConfig ::
       SmosReportConfig
    -> Flags
    -> Environment
    -> Maybe Configuration
    -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
    pure $
        case msum [flagWorkflowDir, envWorkflowDir, mc >>= confWorkflowDir] of
            Nothing -> src
            Just wd ->
                src
                    { smosReportConfigAgendaFileSpec =
                          AgendaFileSpec $ resolveDir' wd
                    }

parseFlags :: Parser Flags
parseFlags = Flags <$> parseWorkflowDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILEPATH"
             , help "The workflow directory to use"
             , value Nothing
             ])

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    let getSmosEnv :: String -> Maybe String
        getSmosEnv key = ("SMOS_" ++ key) `lookup` env
    pure
        Environment
            { envWorkflowDir =
                  getSmosEnv "WORKFLOW_DIRECTORY" <|> getSmosEnv "WORKFLOW_DIR"
            }
