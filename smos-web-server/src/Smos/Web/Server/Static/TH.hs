module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Smos.Web.Server.Constants
import System.Environment
import System.FilePath
import Yesod.EmbeddedStatic

mkStatic :: Q [Dec]
mkStatic = do
  env <- runIO getEnvironment
  let var = "SMOS_WEB_SERVER_FRONT_JS"
      mFront pathLocal pathNix =
        case lookup var env of
          Nothing -> pathLocal
          Just d -> d </> pathNix
      mFrontF fp = mFront ("front/dist" </> fp ++ ".js") ("dist" </> fp ++ ".js")
  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    [ embedFileAt "smos-web-server-front.js" $ mFrontF "smos-web-server-front"
    ]
