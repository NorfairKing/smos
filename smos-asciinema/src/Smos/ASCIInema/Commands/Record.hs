{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Commands.Record
  ( record,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Yaml
import GHC.IO.Handle
import Path
import Path.IO
import System.Exit
import System.Process.Typed
import YamlParse.Applicative

record :: Path Abs File -> IO ()
record f = do
  mSpec <- readConfigFile f
  case mSpec of
    Nothing -> die $ "File does not exist: " <> fromAbsFile f
    Just s -> runASCIInema s

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaOutput :: FilePath,
        asciinemaInput :: [ASCIInemaCommand]
      }
  deriving (Show, Eq)

instance FromJSON ASCIInemaSpec where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaSpec where
  yamlSchema =
    objectParser "ASCIInemaSpec" $
      ASCIInemaSpec
        <$> optionalField "command" "The command to show off. Leave this to just run a shell"
        <*> requiredField "output" "The path to the cast file"
        <*> optionalFieldWithDefault "input" [] "The inputs to send to the command"

runASCIInema :: ASCIInemaSpec -> IO ()
runASCIInema ASCIInemaSpec {..} = do
  outFile <- resolveFile' asciinemaOutput
  let apc =
        setStdin createPipe $ proc "asciinema" $
          concat
            [ ["rec", "--yes", "--quiet", "--overwrite", fromAbsFile outFile],
              maybe [] (\c -> ["--command", c]) asciinemaCommand
            ]
  ensureDir $ parent outFile
  withProcessWait apc $ \p -> do
    let h = getStdin p
    hSetBuffering h NoBuffering
    sendAsciinemaCommand h $ Wait 1000
    mapM_ (sendAsciinemaCommand h) asciinemaInput
    when (isNothing asciinemaCommand) $ sendAsciinemaCommand h $ SendInput "exit\n"

data ASCIInemaCommand
  = Wait Int -- Milliseconds
  | SendInput String
  deriving (Show, Eq)

instance FromJSON ASCIInemaCommand where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaCommand where
  yamlSchema =
    alternatives
      [ objectParser "Wait" $ Wait <$> requiredField "wait" "How long to wait (in milliseconds)",
        objectParser "SendInput" $ SendInput <$> requiredField "send" "The input to send"
      ]

sendAsciinemaCommand :: Handle -> ASCIInemaCommand -> IO ()
sendAsciinemaCommand h = \case
  Wait i -> threadDelay $ i * 1000
  SendInput s -> do
    hPutStr h s
    hFlush h
