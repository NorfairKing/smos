module Smos.Cursor.Tags
    ( TagsCursor
    , makeTagsCursor
    , rebuildTagsCursor
    , tagsCursorSetTag
    , tagsCursorUnsetTag
    , tagsCursorToggleTag
    ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Cursor.Simple.List.NonEmpty
import Cursor.Types

import Smos.Data.Types

type TagsCursor = NonEmptyCursor Tag

makeTagsCursor :: NonEmpty Tag -> TagsCursor
makeTagsCursor = makeNonEmptyCursor

rebuildTagsCursor :: TagsCursor -> NonEmpty Tag
rebuildTagsCursor = rebuildNonEmptyCursor

tagsCursorSetTag :: Tag -> Maybe TagsCursor -> Maybe TagsCursor
tagsCursorSetTag t mtc =
    case mtc of
        Nothing -> Just $ singletonNonEmptyCursor t
        Just tc ->
            if t `elem` rebuildNonEmptyCursor tc
                then Nothing
                else Just $ nonEmptyCursorAppendAtEnd t tc

tagsCursorUnsetTag :: Tag -> TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorUnsetTag t tc =
    let ne = rebuildNonEmptyCursor tc
    in if t `elem` ne
           then do
               let ts = NE.filter (/= t) ne
               pure $
                   case NE.nonEmpty ts of
                       Nothing -> Deleted
                       Just ne' -> Updated $ makeNonEmptyCursor ne'
           else Nothing

tagsCursorToggleTag :: Tag -> Maybe TagsCursor -> DeleteOrUpdate TagsCursor
tagsCursorToggleTag t mtc =
    case mtc of
        Nothing -> Updated $ singletonNonEmptyCursor t
        Just tc ->
            let ne = rebuildNonEmptyCursor tc
            in if t `elem` ne
                   then let ts = NE.filter (/= t) ne
                        in case NE.nonEmpty ts of
                               Nothing -> Deleted
                               Just ne' -> Updated $ makeNonEmptyCursor ne'
                   else Updated $ nonEmptyCursorAppendAtEnd t tc
