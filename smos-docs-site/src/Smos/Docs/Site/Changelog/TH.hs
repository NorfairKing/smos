module Smos.Docs.Site.Changelog.TH where

import Data.Yaml as Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Smos.Docs.Site.Changelog.Type

mkChangelog :: Q Exp
mkChangelog = do
  let fp = "CHANGELOG.yaml"
  qAddDependentFile fp
  changelog <- runIO $ Yaml.decodeFileThrow fp
  lift (changelog :: Changelog)
