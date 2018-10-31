{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Tag
    ( TagCursor(..)
    , makeTagCursor
    , rebuildTagCursor
    , tagCursorInsert
    , tagCursorAppend
    , tagCursorDelete
    , tagCursorRemove
    ) where

import GHC.Generics (Generic)

import Data.Maybe
import Control.Monad
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
