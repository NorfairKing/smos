{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.SchemaSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml.Schema
import Data.Data
import Smos.Data.Gen ()
import Smos.Data.Types
import Test.Syd
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

yamlSchemaSpec :: forall a. (Typeable a, HasCodec a) => FilePath -> Spec
yamlSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenByteStringFile ("test_resources/yaml-schema/" <> filePath <> ".txt") (renderColouredSchemaViaCodec @a)
