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
    , propertyNameAttr
    , propertyNameSpecificAttr
    , tagAttr
    , tagSpecificAttr
    , helpNameAttr
    , helpKeyCombinationAttr
    , helpDescriptionAttr
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
            , (propertyNameSpecificAttr "effort", fg magenta)
            , (propertyNameSpecificAttr "client", fg green)
            , (selectedAttr <> tagAttr, fg brightWhite)
            , (selectedAttr <> headerAttr, fg brightWhite)
            ] $
        attrMap
            defAttr
            [ (selectedAttr, fg V.white)
            , (headerAttr, fg V.yellow)
            , (helpNameAttr, fg V.yellow)
            , (helpKeyCombinationAttr, fg V.blue)
            , (helpDescriptionAttr, fg V.yellow)
            ]

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

propertyNameAttr :: AttrName
propertyNameAttr = "propertyname"

propertyNameSpecificAttr :: PropertyName -> AttrName
propertyNameSpecificAttr pn =
    fromString $ "propertyname-" ++ T.unpack (propertyNameText pn)

tagAttr :: AttrName
tagAttr = "tag"

tagSpecificAttr :: Tag -> AttrName
tagSpecificAttr t = fromString $ "tag-" ++ T.unpack (tagText t)

helpNameAttr :: AttrName
helpNameAttr = "helpdescription"

helpKeyCombinationAttr :: AttrName
helpKeyCombinationAttr = "helpkeycombination"

helpDescriptionAttr :: AttrName
helpDescriptionAttr = "helpdescription"

textCursorName :: ResourceName
textCursorName = "text-cursor"
