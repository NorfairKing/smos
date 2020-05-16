{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.SmosSyncClient
  ( getSmosSyncClientR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Sync.Client.OptParse

getSmosSyncClientR :: Handler Html
getSmosSyncClientR = do
  DocPage {..} <- lookupPage "smos-sync-client"
  let helpText = getHelpPageOf []
  defaultLayout $ do
    setTitle "Smos Documentation - smos-sync-client"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-sync-client"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
