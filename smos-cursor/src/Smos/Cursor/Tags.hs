{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Tags
    ( TagsCursor(..)
    , makeTagsCursor
    , singletonTagsCursor
    , rebuildTagsCursor
    , tagsCursorSetTag
    , tagsCursorUnsetTag
    , tagsCursorToggleTag
    , tagsCursorInsert
    , tagsCursorAppend
    , tagsCursorDelete
    , tagsCursorRemove
    , tagsCursorSelectPrev
    , tagsCursorSelectNext
    , tagsCursorSelectPrevChar
    , tagsCursorSelectNextChar
    , tagsCursorSelectPrevTag
    , tagsCursorSelectNextTag
    ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Validity

import Control.Applicative

import Lens.Micro

import Cursor.List.NonEmpty
import Cursor.Types

import Smos.Data

import Smos.Cursor.Tag

newtype TagsCursor = TagsCursor
    { tagsCursorNonEmptyCursor :: NonEmptyCursor TagCursor Tag
    } deriving (Show, Eq, Generic)

instance Validity TagsCursor

tagsCursorNonEmptyCursorL :: Lens' TagsCursor (NonEmptyCursor TagCursor Tag)
tagsCursorNonEmptyCursorL =
    lens tagsCursorNonEmptyCursor $ \tc nec ->
        tc {tagsCursorNonEmptyCursor = nec}

makeTagsCursor :: NonEmpty Tag -> TagsCursor
makeTagsCursor = TagsCursor . makeNonEmptyCursor makeTagCursor

singletonTagsCursor :: Tag -> TagsCursor
singletonTagsCursor = TagsCursor . singletonNonEmptyCursor . makeTagCursor

rebuildTagsCursor :: TagsCursor -> NonEmpty Tag
rebuildTagsCursor =
    rebuildNonEmptyCursor rebuildTagCursor . tagsCursorNonEmptyCursor

tagsCursorSetTag :: Tag -> Maybe TagsCursor -> Maybe TagsCursor
tagsCursorSetTag t mtc =
    case mtc of
        Nothing -> Just $ singletonTagsCursor t
        Just tc ->
            if t `elem` rebuildTagsCursor tc
                then Nothing
                else Just $
                     tc & tagsCursorNonEmptyCursorL %~
                     nonEmptyCursorAppendAtEnd t

tagsCursorUnsetTag :: Tag -> TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorUnsetTag t tc =
    let ne = rebuildTagsCursor tc
    in if t `elem` ne
           then do
               let ts = NE.filter (/= t) ne
               pure $
                   case NE.nonEmpty ts of
                       Nothing -> Deleted
                       Just ne' -> Updated $ makeTagsCursor ne'
           else Nothing

tagsCursorToggleTag :: Tag -> Maybe TagsCursor -> DeleteOrUpdate TagsCursor
tagsCursorToggleTag t mtc =
    case mtc of
        Nothing -> Updated $ singletonTagsCursor t
        Just tc ->
            let ne = rebuildTagsCursor tc
            in if t `elem` ne
                   then let ts = NE.filter (/= t) ne
                        in case NE.nonEmpty ts of
                               Nothing -> Deleted
                               Just ne' -> Updated $ makeTagsCursor ne'
                   else Updated $
                        tc & tagsCursorNonEmptyCursorL %~
                        nonEmptyCursorAppendAtEnd t

tagsCursorInsert :: Char -> TagsCursor -> Maybe TagsCursor
tagsCursorInsert '\t' =
    tagsCursorNonEmptyCursorL $
    pure . nonEmptyCursorAppendAndSelect rebuildTagCursor emptyTagCursor
tagsCursorInsert c =
    tagsCursorNonEmptyCursorL . nonEmptyCursorElemL $ tagCursorInsert c

tagsCursorAppend :: Char -> TagsCursor -> Maybe TagsCursor
tagsCursorAppend '\t' =
    tagsCursorNonEmptyCursorL $
    pure . nonEmptyCursorInsertAndSelect rebuildTagCursor emptyTagCursor
tagsCursorAppend c =
    tagsCursorNonEmptyCursorL . nonEmptyCursorElemL $ tagCursorAppend c

tagsCursorDelete :: TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorDelete tc =
    case tc & (tagsCursorNonEmptyCursorL . nonEmptyCursorElemL) tagCursorDelete of
        Just tc' -> Just $ Updated tc'
        Nothing ->
            tc &
            focusPossibleDeleteOrUpdate
                tagsCursorNonEmptyCursorL
                (nonEmptyCursorDeleteElemAndSelectNext makeTagCursor)

tagsCursorRemove :: TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorRemove tc =
    case tc & (tagsCursorNonEmptyCursorL . nonEmptyCursorElemL) tagCursorRemove of
        Just tc' -> Just $ Updated tc'
        Nothing ->
            tc &
            focusPossibleDeleteOrUpdate
                tagsCursorNonEmptyCursorL
                (nonEmptyCursorRemoveElemAndSelectPrev makeTagCursor)

tagsCursorSelectPrev :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrev tc =
    tagsCursorSelectPrevChar tc <|> tagsCursorSelectPrevTag tc

tagsCursorSelectNext :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNext tc =
    tagsCursorSelectNextChar tc <|> tagsCursorSelectNextTag tc

tagsCursorSelectPrevChar :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrevChar =
    tagsCursorNonEmptyCursorL . nonEmptyCursorElemL $ tagCursorSelectPrevChar

tagsCursorSelectNextChar :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNextChar =
    tagsCursorNonEmptyCursorL . nonEmptyCursorElemL $ tagCursorSelectNextChar

tagsCursorSelectPrevTag :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrevTag =
    fmap
        ((tagsCursorNonEmptyCursorL . nonEmptyCursorElemL) %~ tagCursorSelectEnd) .
    tagsCursorNonEmptyCursorL
        (nonEmptyCursorSelectPrev rebuildTagCursor makeTagCursor)

tagsCursorSelectNextTag :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNextTag =
    fmap
        ((tagsCursorNonEmptyCursorL . nonEmptyCursorElemL) %~
         tagCursorSelectStart) .
    tagsCursorNonEmptyCursorL
        (nonEmptyCursorSelectNext rebuildTagCursor makeTagCursor)
