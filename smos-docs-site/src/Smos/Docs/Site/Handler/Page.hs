{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Page
  ( getPageR,
  )
where

import Smos.Docs.Site.Foundation

getPageR :: DocPage -> Handler Html
getPageR DocPage {..} =
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescription docPageDescription
    $(widgetFile "page")
