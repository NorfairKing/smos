{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.SchemaSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml.Document
import Control.Monad
import Data.Aeson as JSON
import Data.Data
import Data.Time
import Smos.Data.Gen ()
import Smos.Data.Types
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  yamlSchemaSpec @Header "header"
  yamlSchemaSpec @Contents "contents"
  yamlSchemaSpec @PropertyName "property-name"
  yamlSchemaSpec @PropertyValue "property-value"
  yamlSchemaSpec @TimestampName "timestamp-name"
  yamlSchemaSpec @Timestamp "timestamp"
  yamlSchemaSpec @TodoState "todostate"
  yamlSchemaSpec @StateHistory "statehistory"
  yamlSchemaSpec @StateHistoryEntry "statehistory-entry"
  yamlSchemaSpec @Tag "tag"
  yamlSchemaSpec @LogbookEntry "logbook-entry"
  yamlSchemaSpec @Logbook "logbook"
  yamlSchemaSpec @Entry "entry"
  yamlSchemaSpec @SmosFile "smosfile"

yamlSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
yamlSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenByteStringFile ("test_resources/yaml-schema/" <> filePath <> ".txt") (renderColouredSchemaViaCodec @a)
