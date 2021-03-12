{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Data.GoldenSpec (spec) where

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.SemVer as Version
import qualified Data.Set as S
import Data.Time
import Smos.Data
import Test.Syd

spec :: Spec
spec = do
  goldenFormatsSpec "Empty" "empty" emptySmosFile
  goldenFormatsSpec
    "Single simple entry"
    "single-simple-entry"
    ( makeSmosFile
        [ Node
            ( newEntry "hello world"
            )
            []
        ]
    )
  goldenFormatsSpec
    "Single complex entry"
    "single-complex-entry"
    ( makeSmosFile
        [ Node
            ( (newEntry "hello world")
                { entryContents = Just "some\nbig\ncontents",
                  entryTimestamps =
                    M.fromList
                      [ ("SCHEDULED", TimestampDay (fromGregorian 2021 03 12)),
                        ("DEADLINE", TimestampDay (fromGregorian 2021 03 13))
                      ],
                  entryProperties =
                    M.fromList
                      [ ("timewindow", "30m"),
                        ("client", "cssyd")
                      ],
                  entryTags = S.fromList ["online", "home"]
                }
            )
            []
        ]
    )
  goldenFormatsSpec
    "Single simple Tree"
    "single-simple-Tree"
    ( makeSmosFile
        [ Node
            ( newEntry "foo"
            )
            [ Node (newEntry "bar") [],
              Node (newEntry "quux") []
            ]
        ]
    )

goldenFormatsSpec ::
  String ->
  FilePath ->
  SmosFile ->
  Spec
goldenFormatsSpec name fp sf = do
  let formats = [".smos", ".yaml", ".json", ".pretty-json"]
  let encodeAs = \case
        ".smos" -> smosFileYamlBS
        ".yaml" -> smosFileYamlBS
        ".json" -> LB.toStrict . smosFileJSONBS
        ".pretty-json" -> LB.toStrict . smosFileJSONPrettyBS
        s -> error $ "unknown format:" <> s
  forM_ formats $ \format ->
    it (unwords ["outputs the", show name, "as", format, "the same as before"]) $
      pureGoldenByteStringFile ("test_resources/compatibility/v" <> Version.toString currentDataVersion <> "/" <> fp <> format) (encodeAs format sf)
