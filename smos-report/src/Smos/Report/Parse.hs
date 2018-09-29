{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Parse where

import Import

import Smos.Data
import Smos.Report.OptParse.Types

applyToAllSmosFiles ::
       Settings
    -> (Path Abs File -> SmosFile -> IO (Either ProcessSmosFileException a))
    -> IO [a]
applyToAllSmosFiles Settings {..} action = do
    (_, files) <- listDir setWorkDir
    let smosFiles = filter isSmosFile files
    (exceptions, as) <-
        fmap partitionEithers $
        for smosFiles $ \file -> processSmosFile file $ action file
    printErrorMessages setShouldPrint $ displayErrMess exceptions
    pure as

processSmosFile ::
       Path Abs File
    -> (SmosFile -> IO (Either ProcessSmosFileException a))
    -> IO (Either ProcessSmosFileException a)
processSmosFile file action = do
    contents <- readSmosFile file
    case contents of
        Nothing -> pure . Left $ FileDoesntExist file
        Just (Left errMess) -> pure . Left $ SmosFileParseError file errMess
        Just (Right smosFile) -> action smosFile

isSmosFile :: Path Abs File -> Bool
isSmosFile file = fileExtension file == ".smos" && not (isHidden file)

isHidden :: Path Abs File -> Bool
isHidden file =
    case toFilePath $ filename file of
        '.':_ -> True
        _ -> False

displayErrMess :: [ProcessSmosFileException] -> String
displayErrMess = unlines . fmap displayException

data ProcessSmosFileException
    = FileDoesntExist (Path Abs File)
    | SmosFileParseError (Path Abs File)
                         String
    | NoNextActions (Path Abs File)
    deriving (Show, Eq)

instance Exception ProcessSmosFileException where
    displayException (FileDoesntExist file) =
        "The file " <> fromAbsFile file <> " doesn't exist."
    displayException (SmosFileParseError file errMess) =
        "The file " <> fromAbsFile file <> " can't be parsed:\n\t" <> errMess
    displayException (NoNextActions file) =
        "The file " <> fromAbsFile file <> " doesn't have any next actions."

entries :: SmosFile -> [Entry]
entries SmosFile {..} = concat $ toList <$> smosFileForest
