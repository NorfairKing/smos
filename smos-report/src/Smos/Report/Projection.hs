{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projection where

import GHC.Generics (Generic)

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Lens.Micro

import Control.Monad

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Text.Megaparsec
import Text.Megaparsec.Char

import Smos.Data

import Smos.Report.Path

data Projection
  = OntoFile
  | OntoHeader
  | OntoState
  | OntoTag Tag
  | OntoProperty PropertyName
  | OntoAncestor Projection
  deriving (Show, Eq, Generic)

instance Validity Projection

instance FromJSON Projection where
  parseJSON v =
    flip (withText "Projection") v $ \t ->
      case parseProjection t of
        Nothing -> fail "could not parse projection."
        Just f -> pure f

instance ToJSON Projection where
  toJSON = toJSON . renderProjection

data Projectee
  = FileProjection RootedPath
  | HeaderProjection Header
  | StateProjection (Maybe TodoState)
  | TagProjection (Maybe Tag)
  | PropertyProjection PropertyName (Maybe PropertyValue)
  deriving (Show, Eq, Generic)

instance Validity Projectee

instance Semigroup Projectee where
  p1 <> p2 =
    case (p1, p2) of
      (TagProjection mt1, TagProjection mt2) -> TagProjection $ mt1 <|> mt2
      (PropertyProjection pn1 mpv1, PropertyProjection pn2 mpv2) ->
        if pn1 == pn2
          then PropertyProjection pn1 $ mpv1 <|> mpv2
          else p1
      (_, _) -> p1

performProjection :: Projection -> RootedPath -> ForestCursor Entry -> Projectee
performProjection p rp fc =
  let cur = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL
   in case p of
        OntoFile -> FileProjection rp
        OntoHeader -> HeaderProjection $ entryHeader cur
        OntoState -> StateProjection $ entryState cur
        OntoTag t ->
          TagProjection $
          if t `elem` entryTags cur
            then Just t
            else Nothing
        OntoProperty pn -> PropertyProjection pn $ M.lookup pn $ entryProperties cur
        OntoAncestor p' ->
          (case forestCursorSelectAbove fc of
             Nothing -> id
             Just fc' -> (<> performProjection p rp fc')) $
          performProjection p' rp fc

type P = Parsec Void Text

parseProjection :: Text -> Maybe Projection
parseProjection = parseMaybe projectionP

projectionP :: P Projection
projectionP =
  try ontoFileP <|> try ontoHeaderP <|> try ontoStateP <|> try ontoTagP <|> ontoPropertyP <|>
  ontoAncestorP

ontoFileP :: P Projection
ontoFileP = do
  void $ string' "file"
  pure OntoFile

ontoHeaderP :: P Projection
ontoHeaderP = do
  void $ string' "header"
  pure OntoHeader

ontoTagP :: P Projection
ontoTagP = do
  void $ string' "tag:"
  OntoTag <$> tagP

ontoStateP :: P Projection
ontoStateP = do
  void $ string' "state"
  pure OntoState

ontoPropertyP :: P Projection
ontoPropertyP = do
  void $ string' "property:"
  OntoProperty <$> propertyNameP

ontoAncestorP :: P Projection
ontoAncestorP = do
  void $ string' "ancestor:"
  OntoAncestor <$> projectionP

tagP :: P Tag
tagP = do
  s <- many (satisfy validTagChar)
  either fail pure $ parseTag $ T.pack s

propertyNameP :: P PropertyName
propertyNameP = do
  s <- many (satisfy validPropertyNameChar)
  either fail pure $ parsePropertyName $ T.pack s

renderProjection :: Projection -> Text
renderProjection f =
  case f of
    OntoFile -> "file"
    OntoHeader -> "header"
    OntoState -> "state"
    OntoTag t -> "tag:" <> tagText t
    OntoProperty pn -> "property:" <> propertyNameText pn
    OntoAncestor p -> "ancestor:" <> renderProjection p
