{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.SmosQuery
  ( getSmosQueryR,
    getSmosQueryCommandR,
  )
where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Smos.Query.OptParse
import Yesod

getSmosQueryR :: Handler Html
getSmosQueryR =
  case find ((== "smos-query") . docPageUrl) docPages of
    Nothing -> notFound
    Just DocPage {..} -> do
      let helpText = getHelpPageOf []
      defaultLayout $ do
        setTitle "Smos Documentation - smos-query"
        $(widgetFile "smos-query")

getSmosQueryCommandR :: Text -> Handler Html
getSmosQueryCommandR cmd =
  case find ((== ("smos-query/" <> cmd)) . docPageUrl) docPages of
    Nothing -> notFound
    Just DocPage {..} -> do
      let helpText = getHelpPageOf [T.unpack cmd]
      defaultLayout $ do
        setTitle $ toHtml $ "Smos Documentation - " <> docPageTitle
        $(widgetFile "smos-query-command")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-query"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
