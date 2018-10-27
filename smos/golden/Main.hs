{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Exit

import Test.Hspec

import qualified Data.ByteString.Char8 as SB8
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Yaml.Builder as Yaml

import Control.Monad

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
        runIO $
            -- TODO verify that all plain actions and using char actions are unique
         do
            resourcesDir <- resolveDir' "test_resources"
            fs <- snd <$> listDirRecur resourcesDir
            pure $ mapMaybe classify fs
    describe "Golden" $ makeTestcases tfs

data TestFile
    = Before (Path Abs File)
    | Commands (Path Abs File)
    | After (Path Abs File)
    deriving (Show, Eq)

classify :: Path Abs File -> Maybe TestFile
classify fp
    | ".before" `isSuffixOf` fromAbsFile fp = Just $ Before fp
    | ".commands" `isSuffixOf` fromAbsFile fp = Just $ Commands fp
    | ".after" `isSuffixOf` fromAbsFile fp = Just $ After fp
    | otherwise = Nothing

data GoldenTestCase = GoldenTestCase
    { testCaseName :: Path Abs File
    , beforeFile :: Path Abs File
    , commandsFile :: Path Abs File
    , afterFile :: Path Abs File
    } deriving (Show, Eq)

matchUp :: [TestFile] -> Either String [GoldenTestCase]
matchUp tfs =
    fmap nub $
    forM tfs $ \tf ->
        case tf of
            Before b -> do
                c <- findCommands b
                a <- findAfter b
                pure $
                    GoldenTestCase
                    { testCaseName = stripFileExtension b
                    , beforeFile = b
                    , commandsFile = c
                    , afterFile = a
                    }
            Commands c -> do
                b <- findBefore c
                a <- findAfter c
                pure $
                    GoldenTestCase
                    { testCaseName = stripFileExtension b
                    , beforeFile = b
                    , commandsFile = c
                    , afterFile = a
                    }
            After a -> do
                b <- findBefore a
                c <- findCommands a
                pure $
                    GoldenTestCase
                    { testCaseName = stripFileExtension b
                    , beforeFile = b
                    , commandsFile = c
                    , afterFile = a
                    }
  where
    findBefore :: Path Abs File -> Either String (Path Abs File)
    findBefore p =
        let fs =
                mapMaybe
                    (\case
                         Before bp ->
                             if stripFileExtension p == stripFileExtension bp
                                 then Just bp
                                 else Nothing
                         _ -> Nothing)
                    tfs
        in case fs of
               [] -> Left $ unwords ["Before file not found for", fromAbsFile p]
               [f] -> Right f
               _ ->
                   Left $
                   unwords
                       [ "Multiple Before files found:"
                       , show $ map fromAbsFile fs
                       ]
    findCommands :: Path Abs File -> Either String (Path Abs File)
    findCommands p =
        let fs =
                mapMaybe
                    (\case
                         Commands cp ->
                             if stripFileExtension p == stripFileExtension cp
                                 then Just cp
                                 else Nothing
                         _ -> Nothing)
                    tfs
        in case fs of
               [] ->
                   Left $ unwords ["Commands file not found for", fromAbsFile p]
               [f] -> Right f
               _ ->
                   Left $
                   unwords
                       [ "Multiple Commands files found:"
                       , show $ map fromAbsFile fs
                       ]
    findAfter :: Path Abs File -> Either String (Path Abs File)
    findAfter p =
        let fs =
                mapMaybe
                    (\case
                         After ap_ ->
                             if stripFileExtension p == stripFileExtension ap_
                                 then Just ap_
                                 else Nothing
                         _ -> Nothing)
                    tfs
        in case fs of
               [] -> Left $ unwords ["After file not found for", fromAbsFile p]
               [f] -> Right f
               _ ->
                   Left $
                   unwords
                       [ "Multiple After files found:"
                       , show $ map fromAbsFile fs
                       ]

stripFileExtension :: Path b File -> Path b File
stripFileExtension = fromJust . setFileExtension ""

makeTestcases :: [TestFile] -> Spec
makeTestcases tfs =
    case matchUp tfs of
        Left err -> runIO $ die err
        Right ts -> mapM_ makeTestcase ts

makeTestcase :: GoldenTestCase -> Spec
makeTestcase GoldenTestCase {..} =
    it (fromAbsFile testCaseName) $ do
        bf <- readSmosfileForTest beforeFile
        cs <- readCommandsFileForTest commandsFile
        af <- readSmosfileForTest afterFile
        run <- runCommandsOn (Just bf) cs
        expectResults bf af run

readSmosfileForTest :: Path Abs File -> IO SmosFile
readSmosfileForTest fp = do
    mb <- readSmosFile fp
    case mb of
        Nothing -> failure "Could not find before file anymore."
        Just errOrSmosFile ->
            case errOrSmosFile of
                Left err ->
                    failure $ unlines ["Failed to read smos file:", show err]
                Right sf -> pure sf

failure :: String -> IO a
failure s = expectationFailure s >> undefined

readCommandsFileForTest :: Path Abs File -> IO [Command]
readCommandsFileForTest f = do
    t <- T.readFile $ fromAbsFile f
    case mapM parseCommand $ T.lines t of
        Left e -> failure $ unlines ["Failed to parse command:", e]
        Right c -> pure c

data Command where
    CommandPlain :: Action -> Command
    CommandUsing :: Show a => ActionUsing a -> a -> Command

parseCommand :: Text -> Either String Command
parseCommand t =
    case T.words t of
        [] -> Left "Should never happen."
        [n] ->
            case filter ((== n) . actionName) allPlainActions of
                [] -> Left $ unwords ["No action found with name", show n]
                [a] -> pure $ CommandPlain a
                _ ->
                    Left $
                    unwords ["More than one action found with name", show n]
        [n, arg] ->
            case T.unpack arg of
                [] -> Left "Should never happen."
                [c] ->
                    case filter ((== n) . actionUsingName) allUsingCharActions of
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
                        , finalResult = rebuildEditorCursor $ smosStateCursor fs
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
          , "The starting file looked os follows"
          , ppShow bf
          , "The intermediary steps built up to the result as follows:"
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
        , case c of
              CommandPlain a -> T.unpack $ actionName a
              CommandUsing a arg ->
                  unwords [T.unpack $ actionUsingName a, show arg]
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
