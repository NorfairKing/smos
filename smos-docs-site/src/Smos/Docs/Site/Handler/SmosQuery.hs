{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.SmosQuery
  ( getSmosQueryR,
    getSmosQueryCommandR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Query.OptParse

getSmosQueryR :: Handler Html
getSmosQueryR = do
  DocPage {..} <- lookupPage "smos-query"
  let helpText = getHelpPageOf []
  defaultLayout $ do
    setTitle "Smos Documentation - smos-query"
    $(widgetFile "args")

getSmosQueryCommandR :: Text -> Handler Html
getSmosQueryCommandR cmd = do
  DocPage {..} <- lookupPage $ "smos-query_" <> cmd
  let helpText = getHelpPageOf [T.unpack cmd]
  defaultLayout $ do
    setTitle $ toHtml $ "Smos Documentation - " <> docPageTitle
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-query"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
