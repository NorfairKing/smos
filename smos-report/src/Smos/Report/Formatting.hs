{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Formatting where

import qualified Data.ByteString as SB
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Text (Text)

import Rainbow
import Rainbox as Box

import Smos.Data

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

headerChunk :: Header -> Chunk Text
headerChunk = fore yellow . chunk . headerText

