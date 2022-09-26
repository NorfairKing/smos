{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.E2ESpec (spec) where

import Conduit
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Writer.Strict
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as SB8
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.Function
import Data.Functor.Classes
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
import Smos
import Smos.Data
import Smos.Default
import Smos.Instance
import Smos.OptParse.Bare
import Smos.Terminal
import Smos.Types
import TestImport

spec :: Spec
spec = do
  describe "Preconditions" $
    specify "all actions have unique names" $
      let allActionNames = map anyActionName allActions
          grouped = group $ sort allActionNames
       in forM_ grouped $ \case
            [] -> pure () -- impossible, but fine
            [_] -> pure () -- fine
            (an : _) -> expectationFailure $ unwords ["This action name occurred more than once: ", T.unpack (actionNameText an)]

  scenarioDir "test_resources/e2e" $ \scenarioFile ->
    case parseRelFile scenarioFile >>= fileExtension of
      Just ".yaml" -> makeTestcase scenarioFile
      _ -> pure ()

data ScenarioTestCase = ScenarioTestCase
  { scenarioTestCaseStartingFile :: FilePath,
    scenarioTestCaseBefore :: DirForest SmosFile,
    scenarioTestCaseCommands :: [Text],
    scenarioTestCaseAfter :: DirForest SmosFile
  }
  deriving (Show, Generic)

instance Validity ScenarioTestCase

instance FromJSON ScenarioTestCase where
  parseJSON =
    withObject "ScenarioTestCase" $ \o ->
      let defaultFile = [relfile|example.smos|]
          dirForestParser :: JSON.Key -> Parser (DirForest SmosFile)
          dirForestParser k =
            ( (o .: k :: Parser (DirForest SmosFile))
                <|> (DF.singletonFile defaultFile <$> (o .: k :: Parser SmosFile))
            )
       in ScenarioTestCase
            <$> o .:? "starting-file" .!= fromRelFile defaultFile
            <*> dirForestParser "before"
            <*> o .:? "commands" .!= []
            <*> dirForestParser "after"

makeTestcase :: FilePath -> Spec
makeTestcase fp = sequential $ -- Because otherwise it segfaults (?!)
  it fp $ do
    gtc@ScenarioTestCase {..} <- decodeFileThrow fp
    run <- runCommandsOn scenarioTestCaseStartingFile scenarioTestCaseBefore scenarioTestCaseCommands
    shouldBeValid gtc
    expectResults fp scenarioTestCaseBefore scenarioTestCaseAfter run

data CommandsRun = CommandsRun
  { intermidiaryResults :: [(Text, DirForest SmosFile)],
    finalResult :: DirForest SmosFile
  }

runCommandsOn :: FilePath -> DirForest SmosFile -> [Text] -> IO CommandsRun
runCommandsOn startingFilePath start commands =
  withSystemTempDir "smos-scenario" $ \tdir -> do
    workflowDir <- resolveDir tdir "workflow"
    ensureDir workflowDir
    let testConf :: SmosConfig
        testConf =
          defaultConfig
            { configReportConfig =
                defaultReportConfig
                  { smosReportConfigDirectoryConfig =
                      defaultDirectoryConfig
                        { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
                        }
                  }
            }
    DF.write workflowDir start writeSmosFile
    startingPath <- resolveStartingPath workflowDir startingFilePath

    let readWorkflowDir :: IO (DirForest SmosFile)
        readWorkflowDir =
          DF.read workflowDir $ \p -> do
            mErrOrSmosFile <- readSmosFile p
            case mErrOrSmosFile of
              Nothing -> expectationFailure "File somehow did not exist. (Should not happen.)"
              Just (Left err) -> expectationFailure $ "File was not a smos file: " <> err
              Just (Right sf) -> pure sf

    rs <- withSmosInstance testConf (Just startingPath) $ \terminalHandle -> do
      threadDelay 1_000_000 -- Let the TUI start
      -- We need to receive output, otherwise nothing will actually happen.
      let receiveOutput =
            runConduit $
              terminalOutputSource terminalHandle
                -- To debug:
                -- .| (iterMC $ \bs -> liftIO $ print bs)
                .| sinkNull
      -- We send the commands one by one, with some time inbetween.
      let sendInput =
            execWriterT $
              runConduit $
                ( do
                    -- Resize
                    let sendText bs = do
                          yield bs
                          -- Wait a bit to let the tui think.
                          liftIO $ threadDelay 100_000

                    liftIO $ terminalResize terminalHandle (TerminalSize 80 60)
                    forM_ commands $ \c -> do
                      -- Send the command
                      yield (TE.encodeUtf8 c)
                      -- Save, so we can read the inbetween state
                      sendText "w"
                      -- Read the inbetween state
                      df <- liftIO readWorkflowDir
                      tell [(c, df)]
                    -- Close the tui to make the program end.
                    sendText "q"
                )
                  -- To debug:
                  -- .| (iterMC $ \bs -> liftIO $ print bs)
                  .| terminalInputSink terminalHandle
      Left rs <- race sendInput receiveOutput
      pure rs

    finalState <- readWorkflowDir
    pure $
      CommandsRun
        { intermidiaryResults = rs,
          finalResult = finalState
        }

expectResults :: FilePath -> DirForest SmosFile -> DirForest SmosFile -> CommandsRun -> IO ()
expectResults fp bf af CommandsRun {..} =
  let ctx =
        unlines $
          concat
            [ [ "The expected result did not match the actual result.",
                "The starting situation looked as follows:",
                ppShow bf,
                "The commands to run were these:",
                ppShow $ map fst intermidiaryResults,
                "The result was supposed to look like this:",
                ppShow af,
                "",
                "The intermediary steps built up to the result as follows:",
                ""
              ],
              concatMap (\(a, b) -> go a b) intermidiaryResults,
              [ "The expected result was the following:",
                ppShow af,
                "The actual result was the following:",
                ppShow finalResult
              ],
              [ unwords
                  [ "If this was intentional, you can replace the contents of the 'after' part in",
                    fp,
                    "by the following:"
                  ],
                "---[START]---",
                SB8.unpack (Yaml.encode finalResult) <> "---[END]---"
              ]
            ]
   in context ctx $ unless (finalResult `dEqForTest` af) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow finalResult) (ppShow af)
  where
    go :: Text -> DirForest SmosFile -> [String]
    go c isf =
      [ "After running the following command:",
        show c,
        "The situation looked as follows:",
        ppShow isf
      ]

dEqForTest :: DirForest SmosFile -> DirForest SmosFile -> Bool
dEqForTest = liftEq eqForTest

eqForTest :: SmosFile -> SmosFile -> Bool
eqForTest = forestEqForTest `on` smosFileForest
  where
    forestEqForTest :: Forest Entry -> Forest Entry -> Bool
    forestEqForTest = forestEq1 entryEqForTest
    forestEq1 :: (a -> a -> Bool) -> Forest a -> Forest a -> Bool
    forestEq1 eq = listEq1 (treeEq1 eq)
    treeEq1 :: (a -> a -> Bool) -> Tree a -> Tree a -> Bool
    treeEq1 eq (Node n1 fs1) (Node n2 fs2) = (n1 `eq` n2) && forestEq1 eq fs1 fs2
    listEq1 :: (a -> a -> Bool) -> [a] -> [a] -> Bool
    listEq1 eq as bs = go $ zip (map Just as ++ repeat Nothing) (map Just bs ++ repeat Nothing)
      where
        go [] = True
        go (a : rest) =
          case a of
            (Nothing, Nothing) -> True
            (Just _, Nothing) -> False
            (Nothing, Just _) -> False
            (Just t1, Just t2) -> eq t1 t2 && go rest
    entryEqForTest :: Entry -> Entry -> Bool
    entryEqForTest =
      ((==) `on` entryHeader)
        &&& ((==) `on` entryContents)
        &&& ((==) `on` entryTimestamps)
        &&& ((==) `on` entryProperties)
        &&& (stateHistoryEqForTest `on` entryStateHistory)
        &&& ((==) `on` entryTags)
        &&& (logbookEqForTest `on` entryLogbook)
    stateHistoryEqForTest :: StateHistory -> StateHistory -> Bool
    stateHistoryEqForTest sh1 sh2 =
      listEq1 stateHistoryEntryEqForTest (unStateHistory sh1) (unStateHistory sh2)
    stateHistoryEntryEqForTest :: StateHistoryEntry -> StateHistoryEntry -> Bool
    stateHistoryEntryEqForTest =
      ((==) `on` stateHistoryEntryNewState) &&& (dateTimeEqForTest `on` stateHistoryEntryTimestamp)
    logbookEqForTest :: Logbook -> Logbook -> Bool
    logbookEqForTest lb1 lb2 =
      case (lb1, lb2) of
        (LogOpen ut1 lbes1, LogOpen ut2 lbes2) ->
          dateTimeEqForTest ut1 ut2 && listEq1 logbookEntryEqForTest lbes1 lbes2
        (LogClosed lbes1, LogClosed lbes2) -> listEq1 logbookEntryEqForTest lbes1 lbes2
        (_, _) -> False
    logbookEntryEqForTest :: LogbookEntry -> LogbookEntry -> Bool
    logbookEntryEqForTest (LogbookEntry s1 e1) (LogbookEntry s2 e2) =
      dateTimeEqForTest s1 s2 && dateTimeEqForTest e1 e2
    dateTimeEqForTest :: UTCTime -> UTCTime -> Bool
    dateTimeEqForTest _ _ = True
    (&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
    (&&&) f g a b = f a b && g a b
