{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Commands.Record where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.Maybe
import Data.Random.Normal
import Data.Set (Set)
import qualified Data.Set as S
import Data.Yaml
import GHC.IO.Handle
import Path
import Path.IO
import Smos.ASCIInema.OptParse.Types
import qualified System.Directory as FP
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
    Just s -> runASCIInema rs recordSetSpecFile s

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaTimeout :: Int, -- Seconds
        asciinemaFiles :: [FilePath],
        asciinemaWorkingDir :: Maybe FilePath,
        asciinemaWorkflowDir :: Maybe FilePath,
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
        <*> optionalField "working-dir" "The working directory directory"
        <*> optionalField "workflow-dir" "The workflow directory to set via an environment variable"
        <*> optionalFieldWithDefault "input" [] "The inputs to send to the command"

withRestoredFiles :: [FilePath] -> IO a -> IO a
withRestoredFiles fs func =
  bracket
    (getFileStati fs)
    restoreFiles
    $ const func

data FileStatus
  = DoesNotExist FilePath
  | FileWithContents (Path Abs File) ByteString
  | DirWithContents (Path Abs Dir) (DirForest ByteString)
  deriving (Show, Eq, Ord)

getFileStati :: [FilePath] -> IO (Set FileStatus)
getFileStati fs = S.fromList <$> mapM getFileStatus fs

getFileStatus :: FilePath -> IO FileStatus
getFileStatus p = do
  fileExists <- FP.doesFileExist p
  if fileExists
    then do
      fp <- resolveFile' p
      FileWithContents fp <$> SB.readFile (fromAbsFile fp)
    else do
      dirExists <- FP.doesDirectoryExist p
      if dirExists
        then do
          dp <- resolveDir' p
          DirWithContents dp <$> DF.read dp (SB.readFile . fromAbsFile)
        else pure (DoesNotExist p)

-- maybe FileDoesNotExist FileWithContents <$> forgivingAbsence (SB.readFile (fromAbsFile p))

restoreFiles :: Set FileStatus -> IO ()
restoreFiles = mapM_ restoreFile . S.toList

restoreFile :: FileStatus -> IO ()
restoreFile = \case
  DoesNotExist p -> do
    ignoringAbsence $ FP.removePathForcibly p
  FileWithContents p bs -> do
    ensureDir $ parent p
    SB.writeFile (fromAbsFile p) bs
  DirWithContents p df -> do
    ensureDir p
    DF.write p df (\p_ bs -> SB.writeFile (fromAbsFile p_) bs)

runASCIInema :: RecordSettings -> Path Abs File -> ASCIInemaSpec -> IO ()
runASCIInema RecordSettings {..} specFilePath ASCIInemaSpec {..} = do
  let parentDir = parent specFilePath
  mWorkingDir <- mapM (resolveDir parentDir) asciinemaWorkingDir
  let dirToResolveFiles = fromMaybe parentDir mWorkingDir
  withCurrentDir dirToResolveFiles
    $ withRestoredFiles asciinemaFiles
    $ do
      -- Get the output file's parent directory ready
      env <- getEnvironment
      mWorkflowDir <- mapM (resolveDir parentDir) asciinemaWorkflowDir
      let env' =
            concat
              [ env,
                [ ("ASCIINEMA_CONFIG_HOME", maybe ".config" fromAbsDir recordSetAsciinemaConfigDir)
                ],
                [("SMOS_WORKFLOW_DIR", fromAbsDir p) | p <- maybeToList mWorkflowDir]
              ]
      let apc =
            maybe id (setWorkingDir . fromAbsDir) mWorkingDir
              $ setEnv env'
              $ setStdin createPipe
              $ proc "asciinema"
              $ concat
                [ [ "rec",
                    "--stdin",
                    "--yes",
                    "--quiet",
                    "--overwrite",
                    fromAbsFile recordSetOutputFile,
                    "--env=SMOS_WORKFLOW_DIR",
                    "--env=TERM"
                  ],
                  maybe [] (\c -> ["--command", c]) asciinemaCommand
                ]
      -- Make sure the output file can be created nicely
      ensureDir $ parent recordSetOutputFile
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
  | Type String Int -- Milliseconds
  deriving (Show, Eq)

instance FromJSON ASCIInemaCommand where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaCommand where
  yamlSchema =
    alternatives
      [ objectParser "Wait" $ Wait <$> requiredField "wait" "How long to wait (in milliseconds)",
        objectParser "SendInput" $ SendInput <$> requiredField "send" "The input to send",
        objectParser "Type" $
          Type
            <$> requiredField "type" "The input to send"
            <*> optionalFieldWithDefault "delay" 100 "How long to wait between keystrokes (in milliseconds)"
      ]

sendAsciinemaCommand :: Double -> Handle -> ASCIInemaCommand -> IO ()
sendAsciinemaCommand d h = go
  where
    go = \case
      Wait i -> threadDelay $ round $ fromIntegral (i * 1000) * d
      SendInput s -> do
        hPutStr h s
        hFlush h
      Type s i ->
        forM_ s $ \c -> do
          randomDelay <- normalIO' (0, 15) -- Add some random delay to make the typing feel more natural
          let delay = round (fromIntegral i * charSpeed c + randomDelay :: Double)
          go $ Wait delay
          go $ SendInput [c]
    -- Add a delay multiplier based on what kind of character it is to make the typing feel more natural.
    charSpeed ' ' = 1.25
    charSpeed c
      | c `elem` ['a' .. 'z'] = 0.75
      | c `elem` ['A' .. 'Z'] = 1.5 -- Because you have to press 'shift'
      | otherwise = 2 -- Special characters take even longer
