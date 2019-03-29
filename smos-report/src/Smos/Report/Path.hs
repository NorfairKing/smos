{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Path where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Validity
import Data.Validity.Path ()
import Data.Yaml.Builder (ToYaml(..))
import qualified Data.Yaml.Builder as Yaml
import Path

import Control.Applicative

import Smos.Data.Types ()

data RootedPath
    = Relative (Path Abs Dir) (Path Rel File)
    | Absolute (Path Abs File)
    deriving (Show, Eq, Generic)

instance Validity RootedPath

instance FromJSON RootedPath where
    parseJSON v =
        withObject
            "RootedPath"
            (\o -> Relative <$> o .: "root" <*> o .: "relative")
            v <|>
        (Absolute <$> parseJSON v)

instance ToJSON RootedPath where
    toJSON (Absolute p) = toJSON p
    toJSON (Relative ad rf) = object ["root" .= ad, "relative" .= rf]

instance ToYaml RootedPath where
    toYaml (Absolute p) = toYaml p
    toYaml (Relative ad rf) =
        Yaml.mapping [("root", toYaml ad), ("relative", toYaml rf)]

resolveRootedPath :: RootedPath -> Path Abs File
resolveRootedPath (Relative ad rf) = ad </> rf
resolveRootedPath (Absolute af) = af
