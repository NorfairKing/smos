module Smos.Docs.Site.Handler.Import
  ( module Smos.Docs.Site.Handler.Import,
    module X,
  )
where

import Data.List
import Data.Text (Text)
import Smos.Docs.Site.Constants as X
import Smos.Docs.Site.Foundation as X
import Smos.Docs.Site.Static as X
import Smos.Docs.Site.Widget as X
import Yesod as X

lookupPage :: Text -> Handler DocPage
lookupPage url =
  case find ((== url) . docPageUrl) docPages of
    Nothing -> notFound
    Just dp -> pure dp
