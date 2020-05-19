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
    setTitle $ toHtml $ "Smos Documentation - " <> docPageTitle
    $(widgetFile "page")
