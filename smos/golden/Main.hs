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
import Data.Yaml as Yaml

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
        runIO $ do
            resourcesDir <- resolveDir' "test_resources"
            fs <- snd <$> listDirRecur resourcesDir
            pure $ mapMaybe classify fs
    makeTestcases tfs

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
        Right ts ->
            forM_ ts $ \GoldenTestCase {..} ->
                it (fromAbsFile testCaseName) $ do
                    bf <- readSmosfileForTest beforeFile
                    cs <- readCommandsFileForTest commandsFile
                    af <- readSmosfileForTest afterFile
                    run <- runCommandsOn bf cs
                    expectResults af run

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
            case find ((== n) . actionName) allPlainActions of
                Nothing -> Left $ unwords ["No action found with name", show n]
                Just a -> pure $ CommandPlain a
        [n, arg] ->
            case T.unpack arg of
                [] -> Left "Should never happen."
                [c] ->
                    case find ((== n) . actionUsingName) allUsingCharActions of
                        Nothing ->
                            Left $ unwords ["No action found with name", show n]
                        Just a -> pure $ CommandUsing a c
                _ -> Left "Multichar operand"
        _ -> Left "Unable to parse command: more than two words"

data CommandsRun = CommandsRun
    { intermidiaryResults :: [(Command, SmosFile)]
    , finalResult :: SmosFile
    }

runCommandsOn :: SmosFile -> [Command] -> IO CommandsRun
runCommandsOn start commands = do
    (fs, rs) <- foldM go (startState, []) commands
    pure
        CommandsRun
        { intermidiaryResults = reverse rs
        , finalResult = rebuildEditorCursor $ smosStateCursor fs
        }
  where
    startState = initState $(mkAbsFile "/pretend/test/file") start
    testConf = SmosConfig {configKeyMap = mempty}
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

expectResults :: SmosFile -> CommandsRun -> IO ()
expectResults sf CommandsRun {..} =
    unless (finalResult `eqForTest` sf) $
    failure $
    unlines $
    concat
        [ [ "The expected result did not match the actual result."
          , "The expected result was the following:"
          , ppShow sf
          , "The actual result was the following:"
          , ppShow finalResult
          , "The intermediary steps built up to the result as follows:"
          ]
        , concatMap (uncurry go) intermidiaryResults
        , [ "If this was intentional, you can replace the contents of the expected results file by the following:"
          , SB8.unpack $ Yaml.encode finalResult
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
    forestEqForTest fs1 fs2 =
        all
            (\(t1, mt2) ->
                 case mt2 of
                     Nothing -> False
                     Just t2 -> treeEqForTest t1 t2) $
        zip fs1 (map Just fs2 ++ repeat Nothing)
    treeEqForTest :: Tree Entry -> Tree Entry -> Bool
    treeEqForTest (Node n1 fs1) (Node n2 fs2) =
        (n1 `entryEqForTest` n2) && (fs1 `forestEqForTest` fs2)
    entryEqForTest :: Entry -> Entry -> Bool
    entryEqForTest =
        ((==) `on` entryHeader) &&&
        ((==) `on` entryContents) &&&
        ((==) `on` entryTimestamps) &&&
        ((==) `on` entryProperties) &&&
        (stateHistoryEqForTest `on` entryStateHistory) &&&
        ((==) `on` entryTags) &&& ((==) `on` entryLogbook)
    stateHistoryEqForTest :: StateHistory -> StateHistory -> Bool
    stateHistoryEqForTest sh1 sh2 =
        all
            (\(she1, mshe2) ->
                 case mshe2 of
                     Nothing -> False
                     Just she2 -> stateHistoryEntryEqForTest she1 she2) $
        zip
            (unStateHistory sh1)
            (map Just (unStateHistory sh2) ++ repeat Nothing)
    stateHistoryEntryEqForTest :: StateHistoryEntry -> StateHistoryEntry -> Bool
    stateHistoryEntryEqForTest =
        ((==) `on` stateHistoryEntryNewState) &&&
        (dateTimeEqForTest `on` stateHistoryEntryTimestamp)
    dateTimeEqForTest :: UTCTime -> UTCTime -> Bool
    dateTimeEqForTest _ _ = True
    (&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
    (&&&) f g a b = f a b && g a b
