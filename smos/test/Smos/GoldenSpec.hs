{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.GoldenSpec where

import System.Exit

import Test.Hspec

import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad

import Path
import Path.IO

import Brick

import Smos.Data

import Smos
import Smos.App
import Smos.Types

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
                    af' <- runCommandsOn bf cs
                    af' `shouldBe` af

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
    CommandUsing :: ActionUsing a -> a -> Command

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

runCommandsOn :: SmosFile -> [Command] -> IO SmosFile
runCommandsOn start =
    fmap (rebuildEditorCursor . smosStateCursor) . foldM go startState
  where
    startState = initState $(mkAbsFile "/pretend/test/file") start
    testConf = SmosConfig {configKeyMap = mempty}
    go :: SmosState -> Command -> IO SmosState
    go ss c = do
        let func =
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
            Continue () -> pure ss'
