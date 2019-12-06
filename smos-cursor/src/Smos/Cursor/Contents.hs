{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Contents
  ( ContentsCursor(..)
  , makeContentsCursor
  , makeContentsCursorWithSelection
  , rebuildContentsCursor
  , emptyContentsCursor
  , nullContentsCursor
  , contentsCursorSelection
  , contentsCursorSelectPrevLine
  , contentsCursorSelectNextLine
  , contentsCursorSelectFirstLine
  , contentsCursorSelectLastLine
  , contentsCursorSelectPrevChar
  , contentsCursorSelectNextChar
  , contentsCursorIndexOnLine
  , contentsCursorSelectIndexOnLine
  , contentsCursorInsertChar
  , contentsCursorAppendChar
  , contentsCursorInsertNewline
  , contentsCursorAppendNewline
  , contentsCursorRemove
  , contentsCursorDelete
  , contentsCursorSelectStartOfLine
  , contentsCursorSelectEndOfLine
  ) where

import GHC.Generics

import Data.Validity

import Control.DeepSeq

import Cursor.TextField
import Cursor.Types

import Lens.Micro

import Smos.Data.Types

newtype ContentsCursor =
  ContentsCursor
    { contentsCursorTextFieldCursor :: TextFieldCursor
    }
  deriving (Show, Eq, Generic)

instance Validity ContentsCursor where
  validate cc@ContentsCursor {..} =
    mconcat
      [ genericValidate cc
      , decorate "The resulting Contents is valid" $
        case parseContents (rebuildTextFieldCursor contentsCursorTextFieldCursor) of
          Left err -> invalid err
          Right t -> validate t
      ]

instance NFData ContentsCursor

contentsCursorTextFieldCursorL :: Lens' ContentsCursor TextFieldCursor
contentsCursorTextFieldCursorL =
  lens contentsCursorTextFieldCursor $ \contentsc textc ->
    contentsc {contentsCursorTextFieldCursor = textc}

makeContentsCursor :: Contents -> ContentsCursor
makeContentsCursor = ContentsCursor . makeTextFieldCursor . contentsText

makeContentsCursorWithSelection :: Int -> Int -> Contents -> Maybe ContentsCursor
makeContentsCursorWithSelection x y =
  fmap ContentsCursor . makeTextFieldCursorWithSelection x y . contentsText

rebuildContentsCursor :: ContentsCursor -> Contents
rebuildContentsCursor = Contents . rebuildTextFieldCursor . contentsCursorTextFieldCursor

emptyContentsCursor :: ContentsCursor
emptyContentsCursor = ContentsCursor emptyTextFieldCursor

nullContentsCursor :: ContentsCursor -> Bool
nullContentsCursor = nullTextFieldCursor . contentsCursorTextFieldCursor

contentsCursorSelection :: ContentsCursor -> (Int, Int)
contentsCursorSelection = textFieldCursorSelection . contentsCursorTextFieldCursor

contentsCursorSelectPrevLine :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectPrevLine = contentsCursorTextFieldCursorL textFieldCursorSelectPrevLine

contentsCursorSelectNextLine :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectNextLine = contentsCursorTextFieldCursorL textFieldCursorSelectNextLine

contentsCursorSelectFirstLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectFirstLine = contentsCursorTextFieldCursorL %~ textFieldCursorSelectFirstLine

contentsCursorSelectLastLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectLastLine = contentsCursorTextFieldCursorL %~ textFieldCursorSelectLastLine

contentsCursorSelectPrevChar :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectPrevChar = contentsCursorTextFieldCursorL textFieldCursorSelectPrevChar

contentsCursorSelectNextChar :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectNextChar = contentsCursorTextFieldCursorL textFieldCursorSelectNextChar

contentsCursorIndexOnLine :: ContentsCursor -> Int
contentsCursorIndexOnLine = textFieldCursorIndexOnLine . contentsCursorTextFieldCursor

contentsCursorSelectIndexOnLine :: Int -> ContentsCursor -> ContentsCursor
contentsCursorSelectIndexOnLine i =
  contentsCursorTextFieldCursorL %~ textFieldCursorSelectIndexOnLine i

contentsCursorInsertChar :: Char -> Maybe ContentsCursor -> Maybe ContentsCursor
contentsCursorInsertChar c mcc =
  constructValid =<<
  ContentsCursor <$> textFieldCursorInsertChar c (contentsCursorTextFieldCursor <$> mcc)

contentsCursorAppendChar :: Char -> Maybe ContentsCursor -> Maybe ContentsCursor
contentsCursorAppendChar c mcc =
  constructValid =<<
  ContentsCursor <$> textFieldCursorAppendChar c (contentsCursorTextFieldCursor <$> mcc)

contentsCursorInsertNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorInsertNewline cc =
  ContentsCursor $ textFieldCursorInsertNewline $ contentsCursorTextFieldCursor <$> cc

contentsCursorAppendNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorAppendNewline cc =
  ContentsCursor $ textFieldCursorAppendNewline $ contentsCursorTextFieldCursor <$> cc

contentsCursorRemove :: ContentsCursor -> Maybe (DeleteOrUpdate ContentsCursor)
contentsCursorRemove =
  focusPossibleDeleteOrUpdate contentsCursorTextFieldCursorL textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe (DeleteOrUpdate ContentsCursor)
contentsCursorDelete =
  focusPossibleDeleteOrUpdate contentsCursorTextFieldCursorL textFieldCursorDelete

contentsCursorSelectStartOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectStartOfLine = contentsCursorTextFieldCursorL %~ textFieldCursorSelectStartOfLine

contentsCursorSelectEndOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectEndOfLine = contentsCursorTextFieldCursorL %~ textFieldCursorSelectEndOfLine
