{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Commands.Record
  ( record,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Yaml
import GHC.IO.Handle
import Path
import Path.IO
import Smos.ASCIInema.OptParse.Types
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import System.Timeout
import YamlParse.Applicative

record :: RecordSettings -> IO ()
record rs@RecordSettings {..} = do
  mSpec <- readConfigFile recordSetSpecFile
  case mSpec of
    Nothing -> die $ "File does not exist: " <> fromAbsFile recordSetSpecFile
    Just s -> do
      withCurrentDir (parent recordSetSpecFile)
        $ bracket
          (getFileStati (asciinemaFiles s))
          restoreFiles
        $ \_ -> runASCIInema rs s

data FileStatus
  = FileDoesNotExist
  | FileWithContents ByteString
  deriving (Show, Eq)

getFileStati :: [FilePath] -> IO [(Path Abs File, FileStatus)]
getFileStati = mapM $ \f -> do
  p <- resolveFile' f
  s <- getFileStatus p
  pure (p, s)

getFileStatus :: Path Abs File -> IO FileStatus
getFileStatus p = maybe FileDoesNotExist FileWithContents <$> forgivingAbsence (SB.readFile (fromAbsFile p))

restoreFiles :: [(Path Abs File, FileStatus)] -> IO ()
restoreFiles = mapM_ (uncurry restoreFile)

restoreFile :: Path Abs File -> FileStatus -> IO ()
restoreFile p = \case
  FileDoesNotExist -> ignoringAbsence $ removeFile p
  FileWithContents bs -> do
    ensureDir $ parent p
    SB.writeFile (fromAbsFile p) bs

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaTimeout :: Int, -- Seconds
        asciinemaFiles :: [FilePath],
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
        <*> optionalFieldWithDefault "timeout" 60 "How long to allow the recording to run before timing out, in seconds"
        <*> alternatives
          [ (: []) <$> requiredField "file" "The file that is being touched. It will be brought back in order afterwards.",
            optionalFieldWithDefault "files" [] "The files that are being touched. These will be brought back in order afterwards."
          ]
        <*> optionalFieldWithDefault "input" [] "The inputs to send to the command"

runASCIInema :: RecordSettings -> ASCIInemaSpec -> IO ()
runASCIInema RecordSettings {..} ASCIInemaSpec {..} = do
  -- Get the output file's parent directory ready
  ensureDir $ parent recordSetOutputFile
  env <- getEnvironment
  let env' =
        env
          ++ [ ("ASCIINEMA_CONFIG_HOME", ".config")
             ]
  let apc =
        setEnv env'
          $ setStdin createPipe
          $ proc "asciinema"
          $ concat
            [ ["rec", "--stdin", "--yes", "--quiet", "--overwrite", fromAbsFile recordSetOutputFile],
              maybe [] (\c -> ["--command", c]) asciinemaCommand
            ]
  withProcessWait apc $ \p -> do
    mExitedNormally <- timeout (asciinemaTimeout * 1000 * 1000) $ do
      let h = getStdin p
      hSetBuffering h NoBuffering
      sendAsciinemaCommand recordSetWait h $ Wait 1
      mapM_ (sendAsciinemaCommand recordSetWait h) asciinemaInput
      when (isNothing asciinemaCommand) $ hPutStr h "exit\n"
    case mExitedNormally of
      Nothing -> do
        stopProcess p
        die "Asciinema got stuck for 60 seconds"
      Just () -> pure ()

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

sendAsciinemaCommand :: Double -> Handle -> ASCIInemaCommand -> IO ()
sendAsciinemaCommand d h = \case
  Wait i -> threadDelay $ round $ fromIntegral (i * 1000) * d
  SendInput s -> do
    hPutStr h s
    hFlush h
