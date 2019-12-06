{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Tag
  ( TagCursor(..)
  , emptyTagCursor
  , makeTagCursor
  , rebuildTagCursor
  , tagCursorInsert
  , tagCursorAppend
  , tagCursorDelete
  , tagCursorRemove
  , tagCursorSelectStart
  , tagCursorSelectEnd
  , tagCursorSelectPrevChar
  , tagCursorSelectNextChar
  ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.Maybe
import Data.Validity

import Control.DeepSeq

import Lens.Micro

import Cursor.Text
import Cursor.Types

import Smos.Data.Types

newtype TagCursor =
  TagCursor
    { tagCursorTextCursor :: TextCursor
    }
  deriving (Show, Eq, Generic)

instance Validity TagCursor where
  validate tc@TagCursor {..} =
    mconcat
      [ genericValidate tc
      , decorate "The resulting Tag is valid" $
        case parseTag (rebuildTextCursor tagCursorTextCursor) of
          Left err -> invalid err
          Right t -> validate t
      ]

instance NFData TagCursor

tagCursorTextCursorL :: Lens' TagCursor TextCursor
tagCursorTextCursorL = lens tagCursorTextCursor $ \tagc textc -> tagc {tagCursorTextCursor = textc}

emptyTagCursor :: TagCursor
emptyTagCursor = TagCursor emptyTextCursor

makeTagCursor :: Tag -> TagCursor
makeTagCursor = TagCursor . fromJust . makeTextCursor . tagText

rebuildTagCursor :: TagCursor -> Tag
rebuildTagCursor = fromJust . tag . rebuildTextCursor . tagCursorTextCursor

tagCursorInsert :: Char -> TagCursor -> Maybe TagCursor
tagCursorInsert c = tagCursorTextCursorL (textCursorInsert c) >=> constructValid

tagCursorAppend :: Char -> TagCursor -> Maybe TagCursor
tagCursorAppend c = tagCursorTextCursorL (textCursorAppend c) >=> constructValid

tagCursorDelete :: TagCursor -> Maybe (DeleteOrUpdate TagCursor)
tagCursorDelete = focusPossibleDeleteOrUpdate tagCursorTextCursorL textCursorDelete

tagCursorRemove :: TagCursor -> Maybe (DeleteOrUpdate TagCursor)
tagCursorRemove = focusPossibleDeleteOrUpdate tagCursorTextCursorL textCursorRemove

tagCursorSelectStart :: TagCursor -> TagCursor
tagCursorSelectStart = tagCursorTextCursorL %~ textCursorSelectStart

tagCursorSelectEnd :: TagCursor -> TagCursor
tagCursorSelectEnd = tagCursorTextCursorL %~ textCursorSelectEnd

tagCursorSelectPrevChar :: TagCursor -> Maybe TagCursor
tagCursorSelectPrevChar = tagCursorTextCursorL textCursorSelectPrev

tagCursorSelectNextChar :: TagCursor -> Maybe TagCursor
tagCursorSelectNextChar = tagCursorTextCursorL textCursorSelectNext
