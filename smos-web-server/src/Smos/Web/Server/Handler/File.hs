{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.File where

import Data.Aeson.Text as JSON
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Smos.Client hiding (Header)
import Smos.Data
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Text.Julius
import Text.Show.Pretty (ppShow)
import Yesod hiding (Header)

getOldFileR :: [Text] -> Handler Html
getOldFileR ts = withLogin $ \t -> do
  case parseRelFile $ T.unpack $ T.intercalate "/" ts of
    Nothing -> notFound
    Just rf -> do
      sf <- runClientOrErr $ clientGetSmosFile t rf
      editorWidget <- getEditorWidget rf sf
      defaultLayout $(widgetFile "old-file")

getEditorWidget :: Path Rel File -> SmosFile -> Handler Widget
getEditorWidget _ sf = do
  -- TODO use the relative file to contact the api from purescript
  let startingForest = map (headerText . entryHeader <$>) (smosFileForest sf)
      encodedTree = JSON.encodeToLazyText (ForYaml (map (fmap (newEntry . Header)) startingForest))
  pure $ do
    addScript $ StaticR smos_web_server_front_js
    $(widgetFile "editor")
