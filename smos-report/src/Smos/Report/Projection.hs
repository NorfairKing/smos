{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projection where

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

data Projection
    = OntoFile
    | OntoHeader
    | OntoProperty PropertyName
    | OntoState
    | AndAlso Projection Projection
    deriving (Show, Eq, Generic)

instance Validity Projection

data Projectee
    = FileProjection RootedPath
    | HeaderProjection Header
    | StateProjection (Maybe TodoState)
    | PropertyProjection PropertyName (Maybe PropertyValue)
    deriving (Show, Eq, Generic)

instance Validity Projectee

performProjection ::
       Projection -> RootedPath -> ForestCursor Entry -> [Projectee]
performProjection p_ rp fc = go p_ fc
  where
    go p fc =
        let cur = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL
         in case p of
                OntoFile -> [FileProjection rp]
                OntoHeader -> [HeaderProjection $ entryHeader cur]
                OntoState -> [StateProjection $ entryState cur]
                OntoProperty pn ->
                    [PropertyProjection pn $ M.lookup pn $ entryProperties cur]
                AndAlso p1 p2 -> go p1 fc ++ go p2 fc

type P = Parsec Void Text

parseProjection :: Text -> Maybe Projection
parseProjection = parseMaybe projectionP

projectionP :: P Projection
projectionP =
    try ontoFileP <|> try ontoHeaderP <|> try ontoStateP <|> try ontoPropertyP <|>
    andAlsoP

ontoFileP :: P Projection
ontoFileP = do
    void $ string' "file"
    pure OntoFile

ontoHeaderP :: P Projection
ontoHeaderP = do
    void $ string' "header"
    pure OntoHeader

ontoStateP :: P Projection
ontoStateP = do
    void $ string' "state"
    pure OntoState

ontoPropertyP :: P Projection
ontoPropertyP = do
    void $ string' "property:"
    pn <- propertyNameP
    pure $ OntoProperty pn

andAlsoP :: P Projection
andAlsoP = do
    void $ char '('
    s1 <- projectionP
    void $ string' " also "
    s2 <- projectionP
    void $ char ')'
    pure $ AndAlso s1 s2

propertyNameP :: P PropertyName
propertyNameP = do
    s <-
        many
            (satisfy $ \c ->
                 Char.isPrint c && not (Char.isSpace c) &&
                 not (Char.isPunctuation c))
    either fail pure $ parsePropertyName $ T.pack s

renderProjection :: Projection -> Text
renderProjection f =
    case f of
        OntoFile -> "file"
        OntoHeader -> "header"
        OntoState -> "state"
        OntoProperty pn -> "property:" <> propertyNameText pn
        AndAlso s1 s2 ->
            T.concat
                ["(", renderProjection s1, " also ", renderProjection s2, ")"]
