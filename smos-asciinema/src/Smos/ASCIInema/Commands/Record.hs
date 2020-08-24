{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Commands.Record where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Yaml
import GHC.IO.Handle
import Path
import Path.IO
import Smos.ASCIInema.Cast
import Smos.ASCIInema.Input
import Smos.ASCIInema.OptParse.Types
import Smos.ASCIInema.Output
import Smos.ASCIInema.Spec
import Smos.ASCIInema.Terminal
import Smos.ASCIInema.WindowSize
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
    Just s -> do
      cast <- runASCIInema rs recordSetSpecFile s
      LB.writeFile (fromAbsFile recordSetOutputFile) (renderCast cast)

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaTimeout :: Int, -- Seconds
        asciinemaFiles :: [FilePath],
        asciinemaWorkingDir :: Maybe FilePath,
        asciinemaWorkflowDir :: Maybe FilePath,
        asciinemaConfigFile :: Maybe FilePath,
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
        <*> optionalField "config-file" "The config file to set via an environment variable"
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

runASCIInema :: RecordSettings -> Path Abs File -> ASCIInemaSpec -> IO Cast
runASCIInema rs@RecordSettings {..} specFilePath spec@ASCIInemaSpec {..} = do
  let parentDir = parent specFilePath
  mWorkingDir <- mapM (resolveDir parentDir) asciinemaWorkingDir
  let dirToResolveFiles = fromMaybe parentDir mWorkingDir
  withCurrentDir dirToResolveFiles
    $ withRestoredFiles asciinemaFiles
    $ do
      -- Get the output file's parent directory ready
      env <- getEnvironment
      mWorkflowDir <- mapM (resolveDir parentDir) asciinemaWorkflowDir
      mConfigFile <- mapM (resolveFile parentDir) asciinemaConfigFile
      let env' =
            concat
              [ env,
                [("SMOS_WORKFLOW_DIR", fromAbsDir p) | p <- maybeToList mWorkflowDir],
                [("SMOS_CONFIG_FILE", fromAbsFile p) | p <- maybeToList mConfigFile]
              ]
      pc <- case asciinemaCommand of
        Nothing ->
          case lookup "SHELL" env of
            Nothing -> die "No shell configured"
            Just s -> pure $ shell s
        Just c -> pure $ shell c
      -- Make sure the output file can be created nicely
      ensureDir $ parent recordSetOutputFile
      withPseudoTerminal $ \Terminal {..} -> do
        let apc =
              maybe id (setWorkingDir . fromAbsDir) mWorkingDir
                $ setEnv env'
                $ setCreateGroup True
                $ setNewSession True
                $ setStdin (useHandleClose tSlaveHandle)
                $ setStdout (useHandleClose tSlaveHandle)
                $ setStderr (useHandleClose tSlaveHandle) pc
        hSetBuffering tMasterHandle NoBuffering
        hSetBuffering tSlaveHandle NoBuffering
        setWindowSize tFd (recordSetColumns, recordSetRows)
        withProcessWait apc $ \p -> do
          start <- getCurrentTime
          outVar <- newTVarIO []
          mExitedNormally <- timeout (asciinemaTimeout * 1000 * 1000) $ do
            let commands =
                  concat
                    [ [Wait 500],
                      asciinemaInput,
                      [SendInput "exit\r" | isNothing asciinemaCommand],
                      [Wait 500]
                    ]
            race -- For some reason the output conduit never finishes, so this works.
              (runConduit $ inputWriter recordSetOutputView recordSetSpeed recordSetMistakes tAttributes tMasterHandle commands)
              (runConduit $ outputConduit recordSetOutputView outVar tMasterHandle)
          case mExitedNormally of
            Nothing -> do
              stopProcess p
              die $ unwords ["the recording got stuck for", show asciinemaTimeout, "seconds."]
            Just (Right _) -> die "Should not happen: The outputter finished before the inputter"
            Just (Left inputEvents) -> do
              outputEvents <- readTVarIO outVar
              pure $ completeCast rs spec recordSetSpeed start inputEvents outputEvents

completeCast :: RecordSettings -> ASCIInemaSpec -> Speed -> UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> Cast
completeCast RecordSettings {..} ASCIInemaSpec {..} speed start inputs outputs =
  let castHeader =
        Header
          { headerWidth = recordSetColumns,
            headerHeight = recordSetRows,
            headerStartTimestamp = Just start,
            headerDuration = Nothing,
            headerIdleTimeLimit = Nothing,
            headerCommand = asciinemaCommand,
            headerTitle = Nothing,
            headerEnv = Nothing
          }
      castEvents = map (eventSpeedUp speed) $ interleaveEvents start inputs outputs
   in Cast {..}
