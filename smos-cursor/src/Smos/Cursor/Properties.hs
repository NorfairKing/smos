module Smos.Cursor.Properties
    ( PropertiesCursor
    , makePropertiesCursor
    , rebuildPropertiesCursor
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe

import Cursor.Map
import Cursor.Text

import Smos.Data.Types

type PropertiesCursor
     = MapCursor TextCursor TextCursor PropertyName PropertyValue

makePropertiesCursor ::
       NonEmpty (PropertyName, PropertyValue) -> PropertiesCursor
makePropertiesCursor =
    makeMapCursor (fromJust . makeTextCursor . propertyNameText)

rebuildPropertiesCursor ::
       PropertiesCursor -> NonEmpty (PropertyName, PropertyValue)
rebuildPropertiesCursor =
    rebuildMapCursor
        (fromJust . propertyName . rebuildTextCursor)
        (fromJust . propertyValue . rebuildTextCursor)
