module Smos.Cursor.Tags
    ( TagsCursor
    , makeTagsCursor
    , rebuildTagsCursor
    ) where

import Data.List.NonEmpty (NonEmpty)

import Cursor.NonEmpty

import Smos.Data.Types

type TagsCursor = NonEmptyCursor Tag

makeTagsCursor :: NonEmpty Tag -> TagsCursor
makeTagsCursor = makeNonEmptyCursor

rebuildTagsCursor :: TagsCursor -> NonEmpty Tag
rebuildTagsCursor = rebuildNonEmptyCursor
