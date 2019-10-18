{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Formatting where

import qualified Data.ByteString as SB
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import Path

import Rainbow
import Rainbox as Box

import Smos.Data

import Smos.Report.Path
import Smos.Report.Projection

type Table = Seq (Chunk Text)

formatAsTable :: [[Chunk Text]] -> Seq (Chunk Text)
formatAsTable =
  Box.render .
  tableByRows . S.fromList . map (Box.intersperse (separator mempty 1) . S.fromList . map mkCell)

mkCell :: Chunk Text -> Cell
mkCell c = Cell (S.singleton (S.singleton c)) center left mempty

putTableLn :: Seq (Chunk Text) -> IO ()
putTableLn myChunks = do
  printer <- byteStringMakerFromEnvironment
  mapM_ SB.putStr $ chunksToByteStrings printer $ toList myChunks

putBoxLn :: Orientation a => Box a -> IO ()
putBoxLn box = do
  printer <- byteStringMakerFromEnvironment
  mapM_ SB.putStr $ chunksToByteStrings printer $ toList $ Box.render box

rootedPathChunk :: RootedPath -> Chunk Text
rootedPathChunk rp =
  chunk $
  T.pack $
  case rp of
    Relative _ rf -> fromRelFile rf
    Absolute af -> fromAbsFile af

renderEntryTable :: NonEmpty Projection -> [(RootedPath, Entry)] -> Table
renderEntryTable ne tups =
  formatAsTable $
  (\l ->
     if null l
       then []
       else map renderProjectionHeader (toList ne) : l) $
  map renderProjectees $
  flip map tups $ \(rp, e) ->
    flip map (toList ne) $ \projection -> performProjection projection rp e

renderProjectionHeader :: Projection -> Chunk Text
renderProjectionHeader p =
  case p of
    OntoFile -> chunk "file"
    OntoHeader -> chunk "header"
    OntoProperty pn -> chunk $ propertyNameText pn
    OntoState -> chunk "state"

renderProjectees :: [Projectee] -> [Chunk Text]
renderProjectees = map projecteeChunk

projecteeChunk :: Projectee -> Chunk Text
projecteeChunk p =
  case p of
    FileProjection rp -> rootedPathChunk rp
    HeaderProjection h -> headerChunk h
    StateProjection s -> maybe (chunk "") todoStateChunk s
    PropertyProjection pn pv -> maybe (chunk "") (propertyValueChunk pn) pv

mTodoStateChunk :: Maybe TodoState -> Chunk Text
mTodoStateChunk = maybe (chunk "(none)") todoStateChunk

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
        "FAILED" -> brightRed
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

propertyValueChunk :: PropertyName -> PropertyValue -> Chunk Text
propertyValueChunk pn = fore color . chunk . propertyValueText
  where
    color =
      case propertyNameText pn of
        "timewindow" -> magenta
        "client" -> green
        _ -> mempty

tagChunk :: Tag -> Chunk Text
tagChunk = fore cyan . chunk . tagText

intChunk :: Int -> Chunk Text
intChunk = chunk . T.pack . show
