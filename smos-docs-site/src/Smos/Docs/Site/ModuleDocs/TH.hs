{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.ModuleDocs.TH where

import Control.Applicative
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax

moduleDocFunc :: Text -> [(Text, ModuleOption)]
moduleDocFunc jsonText = case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 jsonText)) of
  Left err -> error err
  Right v -> M.toList $ M.filterWithKey (\k _ -> "services.smos" `T.isPrefixOf` k) v

homeManagerModuleDocFunc :: Text -> [(Text, ModuleOption)]
homeManagerModuleDocFunc jsonText = case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 jsonText)) of
  Left err -> error err
  Right v -> M.toList $ M.filterWithKey (\k _ -> "programs.smos" `T.isPrefixOf` k) v

data ModuleOption = ModuleOption
  { moduleOptionExample :: Maybe JSON.Value,
    moduleOptionDefault :: Maybe JSON.Value,
    moduleOptionLoc :: [Text],
    moduleOptionType :: Text,
    moduleOptionReadOnly :: Bool,
    moduleOptionDescription :: Text
  }
  deriving (Show, Eq, Generic, Lift)

instance FromJSON ModuleOption where
  parseJSON = withObject "ModuleOption" $ \o ->
    ModuleOption
      <$> o
        .:? "example"
      <*> o
        .:? "default"
      <*> o
        .: "loc"
      <*> o
        .: "type"
      <*> o
        .: "readOnly"
      <*> ( ( do
                descriptionObject <- o .: "description"
                descriptionObject .: "text"
            )
              <|> (o .: "description")
          )
