{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.ModuleDocs.TH where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

moduleDocFunc :: Text -> [(Text, Value)]
moduleDocFunc jsonText = case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 jsonText)) of
  Left err -> error err
  Right v -> M.toList $ M.filterWithKey (\k _ -> "services.smos" `T.isPrefixOf` k) v
