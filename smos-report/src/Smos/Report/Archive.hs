{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Archive where

import Data.Aeson
import Data.Validity
import GHC.Generics (Generic)
import YamlParse.Applicative

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

instance Validity HideArchive

instance FromJSON HideArchive where
  parseJSON = viaYamlSchema

instance YamlSchema HideArchive where
  yamlSchema = (\b -> if b then HideArchive else Don'tHideArchive) <$> yamlSchema

instance ToJSON HideArchive where
  toJSON HideArchive = Bool True
  toJSON Don'tHideArchive = Bool False
