{-# LANGUAGE OverloadedStrings #-}

module Smos.Style
    ( defaultAttrMap
    , selectedAttr
    , headerAttr
    , contentsAttr
    , todoStateAttr
    , todoStateSpecificAttr
    , todoStateHistoryAttr
    , timestampNameAttr
    , timestampNameSpecificAttr
    , tagAttr
    , tagSpecificAttr
    -- * Names of widgets
    , textCursorName
    -- * Re-exports
    , applyAttrMappings
    , fg
    , bg
    , module Graphics.Vty.Attributes
    ) where

import Import

import qualified Data.Text as T

import Brick.AttrMap as B
import Brick.Util as B
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import Smos.Data
import Smos.Types

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
    let col = rgbColor :: Int -> Int -> Int -> Color
        orange = col 255 175 0 -- 214, #ffaf00
        brown = col 215 95 0 -- 166, #d75f00
     in applyAttrMappings
            [ (todoStateSpecificAttr "TODO", fg red)
            , (todoStateSpecificAttr "NEXT", fg orange)
            , (todoStateSpecificAttr "STARTED", fg orange)
            , (todoStateSpecificAttr "WAITING", fg blue)
            , (todoStateSpecificAttr "READY", fg brown)
            , (todoStateSpecificAttr "DONE", fg green)
            , (todoStateSpecificAttr "CANCELLED", fg green)
            , (timestampNameSpecificAttr "BEGIN", fg brown)
            , (timestampNameSpecificAttr "END", fg brown)
            , (timestampNameSpecificAttr "SCHEDULED", fg orange)
            , (timestampNameSpecificAttr "DEADLINE", fg red)
            , (selectedAttr <> tagAttr, fg brightWhite)
            ] $
        attrMap defAttr [(selectedAttr, fg V.white), (headerAttr, fg V.yellow)]

selectedAttr :: AttrName
selectedAttr = "selected"

headerAttr :: AttrName
headerAttr = "header"

contentsAttr :: AttrName
contentsAttr = "contents"

todoStateAttr :: AttrName
todoStateAttr = "todostate"

todoStateSpecificAttr :: TodoState -> AttrName
todoStateSpecificAttr tss =
    fromString $ "todostate-" ++ T.unpack (todoStateText tss)

todoStateHistoryAttr :: AttrName
todoStateHistoryAttr = "todostatehistory"

timestampNameAttr :: AttrName
timestampNameAttr = "timestampname"

timestampNameSpecificAttr :: TimestampName -> AttrName
timestampNameSpecificAttr tsn =
    fromString $ "timestampname-" ++ T.unpack (timestampNameText tsn)

tagAttr :: AttrName
tagAttr = "tag"

tagSpecificAttr :: Tag -> AttrName
tagSpecificAttr t = fromString $ "tag-" ++ T.unpack (tagText t)

textCursorName :: ResourceName
textCursorName = "text-cursor"
