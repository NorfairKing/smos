{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Smos
  ( getSmosR,
  )
where

import Data.List
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Smos.OptParse
import Yesod

getSmosR :: Handler Html
getSmosR =
  case find ((== "smos") . docPageUrl) docPages of
    Nothing -> notFound
    Just DocPage {..} -> do
      let helpText = getHelpPageOf []
      defaultLayout $ do
        setTitle "Smos Documentation - smos"
        $(widgetFile "smos")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
