{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Page
  ( getPageR,
  )
where

import Data.List
import Data.Text (Text)
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Yesod

getPageR :: Text -> Handler Html
getPageR url =
  case find ((== url) . docPageUrl) docPages of
    Nothing -> notFound
    Just DocPage {..} ->
      defaultLayout $ do
        setTitle $ toHtml $ "Smos Documentation - " <> docPageTitle
        $(widgetFile "page")
