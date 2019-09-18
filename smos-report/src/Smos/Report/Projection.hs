{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projection where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Char as Char
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void

import Control.Monad

import Text.Megaparsec
import Text.Megaparsec.Char

import Smos.Data

import Smos.Report.Path

data Projection
  = OntoFile
  | OntoHeader
  | OntoProperty PropertyName
  | OntoState
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
  | PropertyProjection PropertyName (Maybe PropertyValue)
  deriving (Show, Eq, Generic)

instance Validity Projectee

performProjection :: Projection -> RootedPath -> Entry -> Projectee
performProjection p rp cur =
  case p of
    OntoFile -> FileProjection rp
    OntoHeader -> HeaderProjection $ entryHeader cur
    OntoState -> StateProjection $ entryState cur
    OntoProperty pn -> PropertyProjection pn $ M.lookup pn $ entryProperties cur

type P = Parsec Void Text

parseProjection :: Text -> Maybe Projection
parseProjection = parseMaybe projectionP

projectionP :: P Projection
projectionP = try ontoFileP <|> try ontoHeaderP <|> try ontoStateP <|> try ontoPropertyP

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
  OntoProperty <$> propertyNameP

propertyNameP :: P PropertyName
propertyNameP = do
  s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
  either fail pure $ parsePropertyName $ T.pack s

renderProjection :: Projection -> Text
renderProjection f =
  case f of
    OntoFile -> "file"
    OntoHeader -> "header"
    OntoState -> "state"
    OntoProperty pn -> "property:" <> propertyNameText pn
