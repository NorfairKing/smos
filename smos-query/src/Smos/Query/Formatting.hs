{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Formatting where

import qualified Data.ByteString as SB
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import Path

import Rainbow
import Rainbox as Box

import Smos.Data

import Smos.Report.Path

type Table = Seq (Chunk Text)

formatAsTable :: [[Chunk Text]] -> Seq (Chunk Text)
formatAsTable =
    Box.render .
    tableByRows .
    S.fromList .
    map (Box.intersperse (separator mempty 1) . S.fromList . map mkCell)

mkCell :: Chunk Text -> Cell
mkCell c = Cell (S.singleton (S.singleton c)) center left mempty

putTableLn :: Seq (Chunk Text) -> IO ()
putTableLn myChunks = do
    printer <- byteStringMakerFromEnvironment
    mapM_ SB.putStr $ chunksToByteStrings printer $ toList myChunks

rootedPathChunk :: RootedPath -> Chunk Text
rootedPathChunk rp =
    chunk $
    T.pack $
    case rp of
        Relative _ rf -> fromRelFile rf
        Absolute af -> fromAbsFile af

todoStateChunk :: TodoState -> Chunk Text
todoStateChunk ts = fore color . chunk . todoStateText $ ts
  where
    color =
        case todoStateText ts of
            "TODO" -> red
            "NEXT" -> orange
            "STARTED" -> orange
            "WAITING" -> blue
            "READY" -> brown
            "DONE" -> green
            "CANCELLED" -> green
            _ -> mempty
      where
        orange = color256 214
        brown = color256 166

timestampNameChunk :: TimestampName -> Chunk Text
timestampNameChunk tsn = fore color . chunk . timestampNameText $ tsn
  where
    color =
        case timestampNameText tsn of
            "BEGIN" -> brown
            "END" -> brown
            "SCHEDULED" -> orange
            "DEADLINE" -> red
            _ -> mempty
    orange = color256 214
    brown = color256 166

headerChunk :: Header -> Chunk Text
headerChunk = fore yellow . chunk . headerText
