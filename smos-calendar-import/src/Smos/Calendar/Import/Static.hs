{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Static where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text (validateTextSingleLine)
import GHC.Generics (Generic)
import YamlParse.Applicative

data Static
  = Static
      { staticSummary :: !(Maybe Text),
        staticDescription :: !(Maybe Text)
      }
  deriving (Show, Eq, Generic)

instance Validity Static where
  validate e@Static {..} =
    mconcat
      [ genericValidate e,
        decorate "The title is a single line if it exists" $ maybe valid validateTextSingleLine staticSummary
      ]

instance YamlSchema Static where
  yamlSchema = objectParser "Static" staticObjectParser

staticObjectParser :: ObjectParser Static
staticObjectParser =
  Static
    <$> optionalField' "summary"
    <*> optionalField' "description"

instance FromJSON Static where
  parseJSON = viaYamlSchema

instance ToJSON Static where
  toJSON s = object $ staticToObject s

staticToObject :: Static -> [Pair]
staticToObject Static {..} =
  [ "summary" .= staticSummary,
    "description" .= staticDescription
  ]
