{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Tag
  ( TagCursor (..),
    emptyTagCursor,
    makeTagCursor,
    rebuildTagCursor,
    tagCursorInsert,
    tagCursorAppend,
    tagCursorDelete,
    tagCursorRemove,
    tagCursorSelectStart,
    tagCursorSelectEnd,
    tagCursorSelectPrevChar,
    tagCursorSelectNextChar,
    tagCursorSplit,
  )
where

import Control.DeepSeq
import Control.Monad
import Cursor.Text
import Cursor.Types
import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Smos.Data.Types

newtype TagCursor = TagCursor
  { tagCursorTextCursor :: TextCursor
  }
  deriving (Show, Eq, Generic)

instance Validity TagCursor where
  validate tc@TagCursor {..} =
    mconcat
      [ genericValidate tc,
        decorate "The resulting Tag is valid" $
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

tagCursorSplit :: TagCursor -> (TagCursor, TagCursor)
tagCursorSplit (TagCursor tc) =
  let (a, b) = textCursorSplit tc
   in (TagCursor a, TagCursor b)
