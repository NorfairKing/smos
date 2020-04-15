{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Path where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Function
import Data.Validity
import Data.Validity.Path ()
import Data.Yaml.Builder (ToYaml (..))
import qualified Data.Yaml.Builder as Yaml
import GHC.Generics (Generic)
import Path
import Smos.Data.Types ()

data RootedPath
  = Relative (Path Abs Dir) (Path Rel File)
  | Absolute (Path Abs File)
  deriving (Show, Generic)

instance Validity RootedPath

instance Eq RootedPath where
  (==) = (==) `on` resolveRootedPath

instance Ord RootedPath where
  compare = compare `on` resolveRootedPath

instance NFData RootedPath

instance FromJSON RootedPath where
  parseJSON v =
    withObject "RootedPath" (\o -> Relative <$> o .: "root" <*> o .: "relative") v
      <|> (Absolute <$> parseJSON v)

instance ToJSON RootedPath where
  toJSON (Absolute p) = toJSON p
  toJSON (Relative ad rf) = object ["root" .= ad, "relative" .= rf]

instance ToYaml RootedPath where
  toYaml (Absolute p) = toYaml p
  toYaml (Relative ad rf) = Yaml.mapping [("root", toYaml ad), ("relative", toYaml rf)]

resolveRootedPath :: RootedPath -> Path Abs File
resolveRootedPath (Relative ad rf) = ad </> rf
resolveRootedPath (Absolute af) = af
