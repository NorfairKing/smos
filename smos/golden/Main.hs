{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import GHC.Generics

import System.Exit

import qualified Data.ByteString.Char8 as SB8
import Data.Function
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Yaml
import Data.Yaml.Builder as Yaml

import Control.Monad

import Test.Hspec
import Test.Validity

import Path
import Path.IO

import Brick hiding (on)

import Smos.Data

import Smos
import Smos.App
import Smos.Types

import Text.Show.Pretty

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    tfs <-
        runIO $ do
            resourcesDir <- resolveDir' "test_resources/golden"
            fs <- snd <$> listDirRecur resourcesDir
            pure $ filter ((== ".yaml") . fileExtension) fs
    describe "Preconditions" $ do
        let distinct ls = sort (nub ls) == sort ls
        specify "all actions have unique names" $
            distinct $
            concat
                [ map actionName allPlainActions
                , map actionUsingName allUsingCharActions
                ]
    describe "Golden" $ makeTestcases tfs

data GoldenTestCase = GoldenTestCase
    { goldenTestCaseBefore :: SmosFile
    , goldenTestCaseCommands :: [Command]
    , goldenTestCaseAfter :: SmosFile
    } deriving (Show, Generic)

instance Validity GoldenTestCase

instance FromJSON GoldenTestCase where
    parseJSON =
        withObject "GoldenTestCase" $ \o ->
            GoldenTestCase <$> o .: "before" <*> o .: "commands" <*>
            o .: "after"

makeTestcases :: [Path Abs File] -> Spec
makeTestcases = mapM_ makeTestcase

makeTestcase :: Path Abs File -> Spec
makeTestcase p =
    it (fromAbsFile p) $ do
        gtc@GoldenTestCase {..} <- decodeFileThrow (fromAbsFile p)
        run <- runCommandsOn (Just goldenTestCaseBefore) goldenTestCaseCommands
        shouldBeValid gtc
        expectResults goldenTestCaseBefore goldenTestCaseAfter run

failure :: String -> IO a
failure s = expectationFailure s >> undefined

data Command where
    CommandPlain :: Action -> Command
    CommandUsing :: Show a => ActionUsing a -> a -> Command

instance Validity Command where
    validate = trivialValidation

instance Show Command where
    show (CommandPlain a) = T.unpack $ actionNameText $ actionName a
    show (CommandUsing au inp) =
        unwords [T.unpack $ actionNameText $ actionUsingName au, show inp]

instance FromJSON Command where
    parseJSON =
        withText "Command" $ \t ->
            case parseCommand t of
                Left err -> fail err
                Right c -> pure c

parseCommand :: Text -> Either String Command
parseCommand t =
    case T.words t of
        [] -> Left "Should never happen."
        [n] ->
            case filter ((== ActionName n) . actionName) allPlainActions of
                [] -> Left $ unwords ["No action found with name", show n]
                [a] -> pure $ CommandPlain a
                _ ->
                    Left $
                    unwords ["More than one action found with name", show n]
        [n, arg] ->
            case T.unpack arg of
                [] -> Left "Should never happen."
                [c] ->
                    case filter ((==ActionName n) . actionUsingName) allUsingCharActions of
                        [] ->
                            Left $ unwords ["No action found with name", show n]
                        [a] -> pure $ CommandUsing a c
                        _ ->
                            Left $
                            unwords
                                ["More than one action found with name", show n]
                _ -> Left "Multichar operand"
        _ -> Left "Unable to parse command: more than two words"

data CommandsRun = CommandsRun
    { intermidiaryResults :: [(Command, SmosFile)]
    , finalResult :: SmosFile
    }

runCommandsOn :: Maybe SmosFile -> [Command] -> IO CommandsRun
runCommandsOn mstart commands =
    withSystemTempFile "smos-golden" $ \af _ -> do
        zt <- getZonedTime
        mfl <- lockFile af
        case mfl of
            Nothing -> die "Could not lock pretend file."
            Just fl -> do
                let startState = initState zt af fl mstart
                (fs, rs) <- foldM go (startState, []) commands
                let cr =
                        CommandsRun
                            { intermidiaryResults = reverse rs
                            , finalResult =
                                  rebuildEditorCursor $ smosStateCursor fs
                            }
                unlockFile fl
                pure cr
  where
    testConf = error "tried to access the config"
    go :: (SmosState, [(Command, SmosFile)])
       -> Command
       -> IO (SmosState, [(Command, SmosFile)])
    go (ss, rs) c = do
        let recordCursorHistory :: SmosM ()
            recordCursorHistory =
                modify $ \ss_ ->
                    ss_
                        { smosStateCursorHistory =
                              smosStateCursor ss_ : smosStateCursorHistory ss_
                        }
        let func = do
                recordCursorHistory
                case c of
                    CommandPlain a -> actionFunc a
                    CommandUsing a arg -> actionUsingFunc a arg
        let eventFunc = runSmosM testConf ss func
        ((s, ss'), _) <-
            runStateT
                (runReaderT
                     (runEventM eventFunc)
                     (error "Tried to access the brick env"))
                (error "Tried to access the brick state")
        case s of
            Stop -> failure "Premature stop"
            Continue () ->
                pure (ss', (c, rebuildEditorCursor $ smosStateCursor ss') : rs)

expectResults :: SmosFile -> SmosFile -> CommandsRun -> IO ()
expectResults bf af CommandsRun {..} =
    unless (finalResult `eqForTest` af) $
    failure $
    unlines $
    concat
        [ [ "The expected result did not match the actual result."
          , "The starting file looked as follows:"
          , ppShow bf
          , "The commands to run were these:"
          , ppShow $ map fst intermidiaryResults
          , "The result was supposed to look like this:"
          , ppShow af
          , ""
          , "The intermediary steps built up to the result as follows:"
          , ""
          ]
        , concatMap (uncurry go) intermidiaryResults
        , [ "The expected result was the following:"
          , ppShow af
          , "The actual result was the following:"
          , ppShow finalResult
          ]
        , [ "If this was intentional, you can replace the contents of the expected results file by the following:"
          , "---[START]---"
          , SB8.unpack (Yaml.toByteString finalResult) <> "---[END]---"
          ]
        ]
  where
    go :: Command -> SmosFile -> [String]
    go c isf =
        [ "After running the following command:"
        , show c
        , "The file looked as follows:"
        , ppShow isf
        ]

eqForTest :: SmosFile -> SmosFile -> Bool
eqForTest = forestEqForTest `on` smosFileForest
  where
    forestEqForTest :: Forest Entry -> Forest Entry -> Bool
    forestEqForTest = forestEq1 entryEqForTest
    forestEq1 :: (a -> a -> Bool) -> Forest a -> Forest a -> Bool
    forestEq1 eq = listEq1 (treeEq1 eq)
    treeEq1 :: (a -> a -> Bool) -> Tree a -> Tree a -> Bool
    treeEq1 eq (Node n1 fs1) (Node n2 fs2) =
        (n1 `eq` n2) && (forestEq1 eq fs1 fs2)
    listEq1 :: (a -> a -> Bool) -> [a] -> [a] -> Bool
    listEq1 eq as bs =
        go $ zip (map Just as ++ repeat Nothing) (map Just bs ++ repeat Nothing)
      where
        go [] = True
        go (a:rest) =
            case a of
                (Nothing, Nothing) -> True
                (Just _, Nothing) -> False
                (Nothing, Just _) -> False
                (Just t1, Just t2) -> eq t1 t2 && go rest
    entryEqForTest :: Entry -> Entry -> Bool
    entryEqForTest =
        ((==) `on` entryHeader) &&&
        ((==) `on` entryContents) &&&
        ((==) `on` entryTimestamps) &&&
        ((==) `on` entryProperties) &&&
        (stateHistoryEqForTest `on` entryStateHistory) &&&
        ((==) `on` entryTags) &&& (logbookEqForTest `on` entryLogbook)
    stateHistoryEqForTest :: StateHistory -> StateHistory -> Bool
    stateHistoryEqForTest sh1 sh2 =
        listEq1
            stateHistoryEntryEqForTest
            (unStateHistory sh1)
            (unStateHistory sh2)
    stateHistoryEntryEqForTest :: StateHistoryEntry -> StateHistoryEntry -> Bool
    stateHistoryEntryEqForTest =
        ((==) `on` stateHistoryEntryNewState) &&&
        (dateTimeEqForTest `on` stateHistoryEntryTimestamp)
    logbookEqForTest :: Logbook -> Logbook -> Bool
    logbookEqForTest lb1 lb2 =
        case (lb1, lb2) of
            (LogOpen ut1 lbes1, LogOpen ut2 lbes2) ->
                dateTimeEqForTest ut1 ut2 &&
                listEq1 logbookEntryEqForTest lbes1 lbes2
            (LogClosed lbes1, LogClosed lbes2) ->
                listEq1 logbookEntryEqForTest lbes1 lbes2
            (_, _) -> False
    logbookEntryEqForTest :: LogbookEntry -> LogbookEntry -> Bool
    logbookEntryEqForTest (LogbookEntry s1 e1) (LogbookEntry s2 e2) =
        dateTimeEqForTest s1 s2 && dateTimeEqForTest e1 e2
    dateTimeEqForTest :: UTCTime -> UTCTime -> Bool
    dateTimeEqForTest _ _ = True
    (&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
    (&&&) f g a b = f a b && g a b
