{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projection where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Void
import Data.Yaml.Builder as Yaml
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Data
import Text.Megaparsec
import Text.Megaparsec.Char

data Projection
  = OntoFile
  | OntoHeader
  | OntoState
  | OntoTag !Tag
  | OntoProperty !PropertyName
  | OntoTimestamp !TimestampName
  | OntoAncestor !Projection
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Projection)

instance Validity Projection

instance HasCodec Projection where
  codec = named "Projection" (bimapCodec parseProjection renderProjection codec <??> projectionDocs)

projectionDocs :: [Text]
projectionDocs =
  [ "A column (projection) is a string of one of the following forms:",
    ""
  ]
    <> projectionFormsDocs

projectionFormsDocs :: [Text]
projectionFormsDocs =
  [ "file",
    "state",
    "header",
    "tag:<tag-name>",
    "property:<property-name>",
    "timestamp:<timestamp-name>",
    "ancestor:<projection>"
  ]

projectionExamples :: [(Text, Projection)]
projectionExamples =
  [ ("A column with whether the entry has the 'toast' tag:", OntoTag "toast"),
    ("A column with the value of the 'effort' property:", OntoProperty "effort"),
    ("A column with the value of the 'DEADLINE' timestamp:", OntoTimestamp "DEADLINE"),
    ("A column with whether the entry has an ancestor with the 'toast' tag:", OntoAncestor (OntoTag "toast"))
  ]

data Projectee
  = FileProjection !(Path Rel File)
  | HeaderProjection !Header
  | StateProjection !(Maybe TodoState)
  | TagProjection !(Maybe Tag)
  | PropertyProjection !PropertyName !(Maybe PropertyValue)
  | TimestampProjection !TimestampName !(Maybe Timestamp)
  deriving (Show, Eq, Generic)

instance Validity Projectee

instance ToJSON Projectee where
  toJSON = \case
    FileProjection p -> toJSON p
    HeaderProjection h -> toJSON h
    StateProjection mts -> toJSON mts
    TagProjection mt -> toJSON mt
    PropertyProjection _ mpv -> toJSON mpv
    TimestampProjection _ mt -> toJSON mt

instance ToYaml Projectee where
  toYaml = \case
    FileProjection p -> toYaml p
    HeaderProjection h -> toYaml h
    StateProjection mts -> toYaml mts
    TagProjection mt -> toYaml mt
    PropertyProjection _ mpv -> toYaml mpv
    TimestampProjection _ mt -> toYaml mt

instance Semigroup Projectee where
  p1 <> p2 =
    case (p1, p2) of
      (TagProjection mt1, TagProjection mt2) -> TagProjection $ mt1 <|> mt2
      (PropertyProjection pn1 mpv1, PropertyProjection pn2 mpv2) ->
        if pn1 == pn2
          then PropertyProjection pn1 $ mpv1 <|> mpv2
          else p1
      (_, _) -> p1

performProjectionNE :: NonEmpty Projection -> Path Rel File -> ForestCursor Entry -> NonEmpty Projectee
performProjectionNE ne rp fc = NE.map (\p -> performProjection p rp fc) ne

performProjection :: Projection -> Path Rel File -> ForestCursor Entry -> Projectee
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
        OntoTimestamp tn -> TimestampProjection tn $ M.lookup tn $ entryTimestamps cur
        OntoAncestor p' ->
          ( case forestCursorSelectAbove fc of
              Nothing -> id
              Just fc' -> (<> performProjection p rp fc')
          )
            $ performProjection p' rp fc

type P = Parsec Void Text

parseProjection :: Text -> Either String Projection
parseProjection = left errorBundlePretty . parse projectionP ""

projectionP :: P Projection
projectionP =
  try ontoFileP
    <|> try ontoHeaderP
    <|> try ontoStateP
    <|> try ontoTagP
    <|> try ontoPropertyP
    <|> try ontoTimestampP
    <|> ontoAncestorP

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

ontoTimestampP :: P Projection
ontoTimestampP = do
  void $ string' "timestamp:"
  OntoTimestamp <$> timestampNameP

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

timestampNameP :: P TimestampName
timestampNameP = do
  s <- many (satisfy validTimestampNameChar)
  either fail pure $ parseTimestampName $ T.pack s

renderProjection :: Projection -> Text
renderProjection f =
  case f of
    OntoFile -> "file"
    OntoHeader -> "header"
    OntoState -> "state"
    OntoTag t -> "tag:" <> tagText t
    OntoProperty pn -> "property:" <> propertyNameText pn
    OntoAncestor p -> "ancestor:" <> renderProjection p
    OntoTimestamp p -> "timestamp:" <> timestampNameText p
