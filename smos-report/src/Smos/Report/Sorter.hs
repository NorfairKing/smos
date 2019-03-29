{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Sorter where

import GHC.Generics (Generic)

import Data.Char as Char
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Path

import Control.Monad

import Lens.Micro

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Smos.Data

import Smos.Report.Path

data Sorter
    = ByFile -- Substring of the filename
    | ByProperty PropertyName
    deriving (Show, Eq, Generic)

instance Validity Sorter

sorterOrdering ::
       Sorter
    -> RootedPath
    -> ForestCursor Entry
    -> RootedPath
    -> ForestCursor Entry
    -> Ordering
sorterOrdering s_ rpa fca rpb fcb = go s_ fca fcb
  where
    go s fca fcb =
        case s of
            ByFile -> comparing resolveRootedPath rpa rpb
            ByProperty pn ->
                comparing
                    (\pc ->
                         M.lookup pn $ entryProperties $ pc ^.
                         forestCursorSelectedTreeL .
                         treeCursorCurrentL)
                    fca
                    fcb

type P = Parsec Void Text

parseSorter :: Text -> Maybe Sorter
parseSorter = parseMaybe sorterP

sorterP :: P Sorter
sorterP = try byFileP <|> byPropertyP

byFileP :: P Sorter
byFileP = do
    void $ string' "file"
    pure ByFile

byPropertyP :: P Sorter
byPropertyP = do
    void $ string' "property:"
    pn <- propertyNameP
    pure $ ByProperty pn

propertyNameP :: P PropertyName
propertyNameP = do
    s <-
        many
            (satisfy $ \c ->
                 Char.isPrint c && not (Char.isSpace c) &&
                 not (Char.isPunctuation c))
    either fail pure $ parsePropertyName $ T.pack s

renderSorter :: Sorter -> Text
renderSorter f =
    case f of
        ByFile -> "file"
        ByProperty pn -> "property:" <> propertyNameText pn
