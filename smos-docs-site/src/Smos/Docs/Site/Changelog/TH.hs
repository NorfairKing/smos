{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Changelog.TH where

import CMarkGFM as MD
import Data.Data
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Path
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

deriving instance Data Rel

deriving instance Data File

deriving instance Lift Day

data Changelog = Changelog
  { changelogDay :: !Day,
    changelogAttributes :: ![(Text, Text)],
    changelogContents :: !Text,
    changelogRendered :: !Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, Lift)

isMarkdown :: Path b File -> Bool
isMarkdown f = case fileExtension f of
  Just ".md" -> True
  Just ".markdown" -> True
  _ -> False

changelogKeyFunc :: Path Rel File -> [Text]
changelogKeyFunc = map T.pack . FP.splitDirectories . FP.dropExtension . toFilePath

changelogValFunc :: [Text] -> Text -> Changelog
changelogValFunc urlPieces rawContents =
  let (attributes, contents) = splitContents rawContents
      maybeAtt k =
        case lookup k attributes of
          Nothing ->
            error $
              unlines
                [ "The changelog with url",
                  show urlPieces,
                  "Does not have an attribute with key",
                  T.unpack k
                ]
          Just a -> a
      dayStr = T.unpack $ maybeAtt "day"
      day = case parseTimeM True defaultTimeLocale "%F" dayStr of
        Nothing -> error $ unlines ["the changelog with url", show urlPieces, "did not have a parsable 'day' attribute:", dayStr]
        Just d -> d
      rendered = MD.commonmarkToHtml [optUnsafe] [] contents
   in Changelog
        { changelogDay = day,
          changelogAttributes = attributes,
          changelogContents = contents,
          changelogRendered = rendered
        }

splitContents :: Text -> ([(Text, Text)], Text)
splitContents cs =
  let threeDashes = "---"
      parts = T.splitOn threeDashes cs
   in case parts of
        "" : ts : rest ->
          let attLines = T.lines ts
              tags =
                flip mapMaybe attLines $ \l ->
                  let column = ":"
                   in case T.splitOn column l of
                        [] -> Nothing
                        (key : valParts) ->
                          Just (key, T.intercalate column valParts)
              contents = T.intercalate threeDashes rest
           in (tags, contents)
        [contents] -> ([], contents)
        _ -> error $ "Failed to parse attributes in" <> T.unpack cs
