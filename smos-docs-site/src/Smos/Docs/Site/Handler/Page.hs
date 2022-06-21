{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Page
  ( getPageR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Foundation

getPageR :: [Text] -> Handler Html
getPageR ts = do
  DocPage {..} <- lookupPage' ts
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescriptionIdemp docPageDescription
    $(widgetFile "page")
