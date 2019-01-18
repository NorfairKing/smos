{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Properties
    ( PropertiesCursor
    , emptyPropertiesCursor
    , makePropertiesCursor
    , rebuildPropertiesCursor
    , propertiesCursorToggleSelected
    , propertiesCursorSelectNextChar
    , propertiesCursorSelectPrevChar
    , propertiesCursorInsert
    , propertiesCursorAppend
    , propertiesCursorRemove
    , propertiesCursorDelete
    , propertiesCursorStartNewPropertyBefore
    , propertiesCursorStartNewPropertyAfter
    ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Validity

import Lens.Micro

import Cursor.Map
import Cursor.Text
import Cursor.Types

import Smos.Data.Types

newtype PropertiesCursor = PropertiesCursor
    { propertiesCursorMapCursor :: MapCursor TextCursor TextCursor PropertyName PropertyValue
    } deriving (Show, Eq, Generic)

emptyPropertiesCursor :: PropertiesCursor
emptyPropertiesCursor =
    makePropertiesCursor $ (emptyPropertyName, emptyPropertyValue) :| []

makePropertiesCursor ::
       NonEmpty (PropertyName, PropertyValue) -> PropertiesCursor
makePropertiesCursor = makeMapCursor makePropertyNameCursor

rebuildPropertiesCursor ::
       PropertiesCursor -> NonEmpty (PropertyName, PropertyValue)
rebuildPropertiesCursor =
    rebuildMapCursor rebuildPropertyNameCursor rebuildPropertyValueCursor

propertiesCursorCurrentTextCursorL :: Lens' PropertiesCursor TextCursor
propertiesCursorCurrentTextCursorL =
    lens
        (\tsc ->
             case tsc ^. mapCursorElemL of
                 KeyValueCursorKey kc _ -> kc
                 KeyValueCursorValue _ vc -> vc)
        (\tsc tc ->
             tsc & mapCursorElemL %~
             (\kvc ->
                  case kvc of
                      KeyValueCursorKey _ v -> KeyValueCursorKey tc v
                      KeyValueCursorValue k _ -> KeyValueCursorValue k tc))

propertiesCursorToggleSelected :: PropertiesCursor -> PropertiesCursor
propertiesCursorToggleSelected =
    mapCursorToggleSelected
        rebuildPropertyNameCursor
        makePropertyNameCursor
        rebuildPropertyValueCursor
        makePropertyValueCursor

propertiesCursorSelectNextChar :: PropertiesCursor -> Maybe PropertiesCursor
propertiesCursorSelectNextChar =
    propertiesCursorCurrentTextCursorL textCursorSelectNext

propertiesCursorSelectPrevChar :: PropertiesCursor -> Maybe PropertiesCursor
propertiesCursorSelectPrevChar =
    propertiesCursorCurrentTextCursorL textCursorSelectPrev

propertiesCursorInsert :: Char -> PropertiesCursor -> Maybe PropertiesCursor
propertiesCursorInsert c =
    propertiesCursorCurrentTextCursorL (textCursorInsert c)

propertiesCursorAppend :: Char -> PropertiesCursor -> Maybe PropertiesCursor
propertiesCursorAppend c =
    propertiesCursorCurrentTextCursorL (textCursorAppend c)

propertiesCursorRemove ::
       PropertiesCursor -> Maybe (DeleteOrUpdate PropertiesCursor)
propertiesCursorRemove =
    focusPossibleDeleteOrUpdate
        propertiesCursorCurrentTextCursorL
        textCursorRemove

propertiesCursorDelete ::
       PropertiesCursor -> Maybe (DeleteOrUpdate PropertiesCursor)
propertiesCursorDelete =
    focusPossibleDeleteOrUpdate
        propertiesCursorCurrentTextCursorL
        textCursorDelete

propertiesCursorStartNewPropertyBefore :: PropertiesCursor -> PropertiesCursor
propertiesCursorStartNewPropertyBefore =
    mapCursorInsertAndSelectKey
        rebuildPropertyNameCursor
        rebuildPropertyValueCursor
        emptyTextCursor
        emptyPropertyValue

propertiesCursorStartNewPropertyAfter :: PropertiesCursor -> PropertiesCursor
propertiesCursorStartNewPropertyAfter =
    mapCursorAppendAndSelectKey
        rebuildPropertyNameCursor
        rebuildPropertyValueCursor
        emptyTextCursor
        emptyPropertyValue

-- safe because of validity
rebuildPropertyNameCursor :: TextCursor -> PropertyName
rebuildPropertyNameCursor = fromJust . propertyName . rebuildTextCursor

-- safe because of validity
rebuildPropertyValueCursor :: TextCursor -> PropertyValue
rebuildPropertyValueCursor = fromJust . propertyValue . rebuildTextCursor

makePropertyNameCursor :: PropertyName -> TextCursor
makePropertyNameCursor = fromJust . makeTextCursor . propertyNameText

makePropertyValueCursor :: PropertyValue -> TextCursor
makePropertyValueCursor = fromJust . makeTextCursor . propertyValueText
