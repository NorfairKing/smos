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
    , tagCursorSelectPrevChar
    , tagCursorSelectNextChar
    ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.Maybe
import Data.Validity

import Lens.Micro

import Cursor.Text

import Smos.Data.Types

newtype TagCursor = TagCursor
    { tagCursorTextCursor :: TextCursor
    } deriving (Show, Eq, Generic)

instance Validity TagCursor where
    validate tc@TagCursor {..} =
        mconcat
            [ genericValidate tc
            , decorate "The resulting Tag is valid" $
              case parseTag (rebuildTextCursor tagCursorTextCursor) of
                  Left err -> invalid err
                  Right t -> validate t
            ]

tagCursorTextCursorL :: Lens' TagCursor TextCursor
tagCursorTextCursorL =
    lens tagCursorTextCursor $ \tagc textc -> tagc {tagCursorTextCursor = textc}

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

tagCursorDelete :: TagCursor -> Maybe TagCursor
tagCursorDelete = tagCursorTextCursorL textCursorDelete

tagCursorRemove :: TagCursor -> Maybe TagCursor
tagCursorRemove = tagCursorTextCursorL textCursorRemove

tagCursorSelectPrevChar :: TagCursor -> Maybe TagCursor
tagCursorSelectPrevChar = tagCursorTextCursorL textCursorSelectPrev

tagCursorSelectNextChar :: TagCursor -> Maybe TagCursor
tagCursorSelectNextChar = tagCursorTextCursorL textCursorSelectPrev
