{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.SmosServer
  ( getSmosServerR,
  )
where

import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Server.OptParse

getSmosServerR :: Handler Html
getSmosServerR = do
  DocPage {..} <- lookupPage "smos-server"
  let helpText = getHelpPageOf []
  defaultLayout $ do
    setTitle "Smos Documentation - smos-server"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-server"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
