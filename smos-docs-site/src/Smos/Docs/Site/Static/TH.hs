{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Static.TH where

import CMarkGFM as MD
import Data.Data
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Path
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data DocPage = DocPage
  { docPageTitle :: !Text,
    docPageDescription :: !Text,
    docPageAttributes :: ![(Text, Text)],
    docPageContents :: !Text,
    docPageRendered :: !Text
  }
  deriving (Typeable, Lift)

docPageKeyFunc :: Path Rel File -> [Text]
docPageKeyFunc = map T.pack . FP.splitDirectories . FP.dropExtension . toFilePath

docPageValFunc :: [Text] -> Text -> DocPage
docPageValFunc urlPieces rawContents =
  let (attributes, contents) = splitContents rawContents
      maybeAtt k =
        case lookup k attributes of
          Nothing ->
            error $
              unlines
                [ "The post with url",
                  show urlPieces,
                  "Does not have an attribute with key",
                  T.unpack k
                ]
          Just a -> a
      title = maybeAtt "title"
      description = maybeAtt "description"
      rendered = MD.commonmarkToHtml [optUnsafe] [] contents
   in DocPage
        { docPageTitle = title,
          docPageDescription = description,
          docPageAttributes = attributes,
          docPageContents = contents,
          docPageRendered = rendered
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
