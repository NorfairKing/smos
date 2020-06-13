{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.File where

import Data.Text (Text)
import qualified Data.Text as T
import Path
import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Text.Show.Pretty (ppShow)
import Yesod

getFileR :: [Text] -> Handler Html
getFileR ts = withLogin $ \t -> do
  case parseRelFile $ T.unpack $ T.intercalate "/" ts of
    Nothing -> notFound
    Just p -> do
      sf <- runClientOrErr $ clientGetSmosFile t p
      defaultLayout $(widgetFile "file")
