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
  , tagsCursorInsertTag
  , tagsCursorAppendTag
  , tagsCursorInsertAndSelectTag
  , tagsCursorAppendAndSelectTag
  , tagsCursorDelete
  , tagsCursorRemove
  , tagsCursorSelectPrev
  , tagsCursorSelectNext
  , tagsCursorSelectOrCreatePrev
  , tagsCursorSelectOrCreateNext
  , tagsCursorSelectPrevChar
  , tagsCursorSelectNextChar
  , tagsCursorSelectPrevTag
  , tagsCursorSelectNextTag
  , tagsCursorSelectOrCreatePrevTag
  , tagsCursorSelectOrCreateNextTag
  ) where

import GHC.Generics (Generic)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Validity

import Control.Applicative

import Lens.Micro

import Cursor.List.NonEmpty
import Cursor.Types

import Smos.Data

import Smos.Cursor.Tag

newtype TagsCursor =
  TagsCursor
    { tagsCursorNonEmptyCursor :: NonEmptyCursor TagCursor Tag
    }
  deriving (Show, Eq, Generic)

instance Validity TagsCursor

tagsCursorNonEmptyCursorL :: Lens' TagsCursor (NonEmptyCursor TagCursor Tag)
tagsCursorNonEmptyCursorL =
  lens tagsCursorNonEmptyCursor $ \tc nec -> tc {tagsCursorNonEmptyCursor = nec}

tagsCursorSelectedTagL :: Lens' TagsCursor TagCursor
tagsCursorSelectedTagL = tagsCursorNonEmptyCursorL . nonEmptyCursorElemL

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
             tc & tagsCursorNonEmptyCursorL %~ nonEmptyCursorAppendAtEnd t

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
                 tc & tagsCursorNonEmptyCursorL %~ nonEmptyCursorAppendAtEnd t

tagsCursorInsert :: Char -> TagsCursor -> Maybe TagsCursor
tagsCursorInsert '\t' =
  tagsCursorNonEmptyCursorL $
  pure . nonEmptyCursorAppendAndSelect rebuildTagCursor emptyTagCursor
tagsCursorInsert c = tagsCursorSelectedTagL $ tagCursorInsert c

tagsCursorAppend :: Char -> TagsCursor -> Maybe TagsCursor
tagsCursorAppend '\t' =
  tagsCursorNonEmptyCursorL $
  pure . nonEmptyCursorInsertAndSelect rebuildTagCursor emptyTagCursor
tagsCursorAppend c = tagsCursorSelectedTagL $ tagCursorAppend c

tagsCursorInsertTag :: Tag -> TagsCursor -> TagsCursor
tagsCursorInsertTag t = tagsCursorNonEmptyCursorL %~ nonEmptyCursorInsert t

tagsCursorAppendTag :: Tag -> TagsCursor -> TagsCursor
tagsCursorAppendTag t = tagsCursorNonEmptyCursorL %~ nonEmptyCursorAppend t

tagsCursorInsertAndSelectTag :: TagCursor -> TagsCursor -> TagsCursor
tagsCursorInsertAndSelectTag tc =
  tagsCursorNonEmptyCursorL %~ nonEmptyCursorInsertAndSelect rebuildTagCursor tc

tagsCursorAppendAndSelectTag :: TagCursor -> TagsCursor -> TagsCursor
tagsCursorAppendAndSelectTag tc =
  tagsCursorNonEmptyCursorL %~ nonEmptyCursorAppendAndSelect rebuildTagCursor tc

tagsCursorDelete :: TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorDelete tc =
  case tc & focusPossibleDeleteOrUpdate tagsCursorSelectedTagL tagCursorDelete of
    Just (Updated tc') -> Just $ Updated tc'
    _ -- TODO fix the entire deletion
     ->
      tc &
      focusPossibleDeleteOrUpdate
        tagsCursorNonEmptyCursorL
        (nonEmptyCursorDeleteElemAndSelectNext makeTagCursor)

tagsCursorRemove :: TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)
tagsCursorRemove tc =
  case tc & focusPossibleDeleteOrUpdate tagsCursorSelectedTagL tagCursorRemove of
    Just (Updated tc') -> Just $ Updated tc'
    _ -- TODO fix the entire deletion
     ->
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

tagsCursorSelectOrCreatePrev :: TagsCursor -> TagsCursor
tagsCursorSelectOrCreatePrev tc =
  fromMaybe (tagsCursorSelectOrCreatePrevTag tc) (tagsCursorSelectPrevChar tc)

tagsCursorSelectOrCreateNext :: TagsCursor -> TagsCursor
tagsCursorSelectOrCreateNext tc =
  fromMaybe (tagsCursorSelectOrCreateNextTag tc) (tagsCursorSelectNextChar tc)

tagsCursorSelectPrevChar :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrevChar = tagsCursorSelectedTagL tagCursorSelectPrevChar

tagsCursorSelectNextChar :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNextChar = tagsCursorSelectedTagL tagCursorSelectNextChar

tagsCursorSelectPrevTag :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectPrevTag =
  fmap (tagsCursorSelectedTagL %~ tagCursorSelectEnd) .
  tagsCursorNonEmptyCursorL
    (nonEmptyCursorSelectPrev rebuildTagCursor makeTagCursor)

tagsCursorSelectNextTag :: TagsCursor -> Maybe TagsCursor
tagsCursorSelectNextTag =
  fmap (tagsCursorSelectedTagL %~ tagCursorSelectStart) .
  tagsCursorNonEmptyCursorL
    (nonEmptyCursorSelectNext rebuildTagCursor makeTagCursor)

tagsCursorSelectOrCreatePrevTag :: TagsCursor -> TagsCursor
tagsCursorSelectOrCreatePrevTag tc =
  fromMaybe
    (tagsCursorInsertAndSelectTag emptyTagCursor tc)
    (tagsCursorSelectPrevTag tc)

tagsCursorSelectOrCreateNextTag :: TagsCursor -> TagsCursor
tagsCursorSelectOrCreateNextTag tc =
  fromMaybe
    (tagsCursorAppendAndSelectTag emptyTagCursor tc)
    (tagsCursorSelectNextTag tc)
