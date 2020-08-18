{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Commands.Record where

import Conduit
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Random.Normal
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time
import Data.Yaml
import GHC.IO.Handle
import Path
import Path.IO
import Smos.ASCIInema.Cast
import Smos.ASCIInema.OptParse.Types
import Smos.ASCIInema.Spec
import Smos.ASCIInema.Terminal
import Smos.ASCIInema.WindowSize
import qualified System.Directory as FP
import System.Environment (getEnvironment)
import System.Exit
import System.IO
import System.Posix.IO (handleToFd, stdOutput)
import System.Process.Typed
import System.Random
import System.Timeout
import Text.Show.Pretty
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
      let env' =
            concat
              [ env,
                [("SMOS_WORKFLOW_DIR", fromAbsDir p) | p <- maybeToList mWorkflowDir]
              ]
      pc <- case asciinemaCommand of
        Nothing ->
          case lookup "SHELL" env of
            Nothing -> die "No shell configured"
            Just s -> pure $ proc s []
        Just c -> pure $ shell c
      -- Make sure the output file can be created nicely
      ensureDir $ parent recordSetOutputFile
      putStrLn "Starting terminal"
      withPseudoTerminal $ \Terminal {..} -> do
        let apc =
              maybe id (setWorkingDir . fromAbsDir) mWorkingDir
                $ setEnv env'
                $ setStdin (useHandleClose tSlaveHandle)
                $ setStdout (useHandleClose tSlaveHandle)
                $ setStderr (useHandleClose tSlaveHandle) pc
        hSetBuffering tMasterHandle NoBuffering
        hSetBuffering tSlaveHandle NoBuffering
        setWindowSize tFd (recordSetColumns, recordSetRows)
        putStrLn "Starting process"
        withProcessWait apc $ \p -> do
          start <- getCurrentTime
          putStrLn "Sending commands"
          mExitedNormally <- timeout (asciinemaTimeout * 1000 * 1000) $ do
            let commands =
                  concat
                    [ [Wait 500],
                      asciinemaInput,
                      [SendInput "exit\n" | isNothing asciinemaCommand],
                      [Wait 500]
                    ]
            concurrently
              ( runConduit $ do
                  inputs <- inputWriter recordSetWait recordSetMistakes tMasterHandle commands
                  liftIO $ putStrLn "Done with input"
                  pure inputs
              )
              ( runConduit $ do
                  outputs <- outputConduit tMasterHandle
                  liftIO $ print "Done with output"
                  pure outputs
              )
          putStrLn "Process done."
          case mExitedNormally of
            Nothing -> do
              stopProcess p
              die $ unwords ["the recording got stuck for", show asciinemaTimeout, "seconds."]
            Just (inputEvents, outputEvents) -> do
              putStrLn "Waiting for outputs"
              putStrLn "got inputs:"
              pPrint inputEvents
              putStrLn "got outputs:"
              pPrint outputEvents
              pure $ completeCast rs spec start inputEvents outputEvents

completeCast :: RecordSettings -> ASCIInemaSpec -> UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> Cast
completeCast RecordSettings {..} ASCIInemaSpec {..} start inputs outputs =
  let dur = undefined
      castHeader =
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
      castEvents = interleaveEvents start inputs outputs
   in Cast {..}

interleaveEvents :: UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> [Event]
interleaveEvents start inputs outputs = go (sortOn fst inputs) (sortOn fst outputs)
  where
    go [] [] = []
    go is [] = map (uncurry makeInput) is
    go [] os = map (uncurry makeOutput) os
    go iss@((it, i) : is) oss@((ot, o) : os) =
      if it <= ot
        then makeInput it i : go is oss
        else makeOutput ot o : go iss os
    makeTime :: UTCTime -> Double
    makeTime t =
      let d = diffUTCTime t start
       in realToFrac d
    makeInput :: UTCTime -> Text -> Event
    makeInput t d = Event {eventTime = makeTime t, eventData = EventInput d}
    makeOutput :: UTCTime -> ByteString -> Event
    makeOutput t d = Event {eventTime = makeTime t, eventData = EventOutput $ TE.decodeUtf8With TE.lenientDecode d}

sendCommands :: Double -> Bool -> Handle -> [ASCIInemaCommand] -> IO [(UTCTime, Text)]
sendCommands speed mistakes h cs = do
  var <- newTVarIO []
  mapM_ (sendAsciinemaCommand speed mistakes h var) cs
  readTVarIO var

outputConduit :: MonadIO m => Handle -> ConduitT () void m [(UTCTime, ByteString)]
outputConduit h = sourceHandle h .| outputDebugConduit .| timerConduit .| sinkList
  where
    timerConduit :: MonadIO m => ConduitT i (UTCTime, i) m ()
    timerConduit = C.mapM $ \i -> (,) <$> liftIO getCurrentTime <*> pure i

outputDebugConduit :: MonadIO m => ConduitT ByteString ByteString m ()
outputDebugConduit = C.mapM $ \bs -> do
  liftIO $ SB.putStr bs
  -- liftIO $ putStrLn $ "Got output: " <> show bs
  pure bs

sendAsciinemaCommand :: Double -> Bool -> Handle -> TVar [(UTCTime, Text)] -> ASCIInemaCommand -> IO ()
sendAsciinemaCommand speed mistakes h var = go
  where
    go = \case
      Wait i -> waitMilliSeconds speed i
      SendInput s -> do
        now <- getCurrentTime
        hPutStr h s
        atomically $ modifyTVar' var ((now, T.pack s) :)
        hFlush h
      Type s i ->
        let waitForChar c = do
              randomDelay <- normalIO' (0, 25) -- Add some random delay to make the typing feel more natural
              go $ Wait $ round (fromIntegral i * charSpeed c + randomDelay :: Double)
         in forM_ s $ \c -> do
              when mistakes $ do
                -- Make a mistake with a 3% likelihood
                randomMistake <- (> (97 :: Int)) <$> randomRIO (0, 100) :: IO Bool
                when randomMistake $ do
                  let possibleMistakes = validMistakes c
                  randomIndex <- randomRIO (0, length possibleMistakes - 1) :: IO Int
                  let c' = possibleMistakes !! randomIndex
                  waitForChar c'
                  go $ SendInput [c']
                  waitForChar '\b'
                  go $ SendInput ['\b'] -- Backspace
              waitForChar c
              go $ SendInput [c]
