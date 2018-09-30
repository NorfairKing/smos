module Smos.Cursor.Properties
    ( PropertiesCursor
    , makePropertiesCursor
    , rebuildPropertiesCursor
    ) where

import Data.List.NonEmpty (NonEmpty)

import Cursor.Simple.Map

import Smos.Data.Types

type PropertiesCursor = MapCursor PropertyName PropertyValue

makePropertiesCursor ::
       NonEmpty (PropertyName, PropertyValue) -> PropertiesCursor
makePropertiesCursor = makeMapCursor

rebuildPropertiesCursor ::
       PropertiesCursor -> NonEmpty (PropertyName, PropertyValue)
rebuildPropertiesCursor = rebuildMapCursor
