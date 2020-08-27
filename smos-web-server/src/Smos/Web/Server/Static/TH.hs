module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Smos.Web.Server.Constants
import System.Environment
import System.FilePath
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  env <- runIO getEnvironment
  let var = "SMOS_WEB_SERVER_FRONT_JS"
      localRelDir = "front/dist"
      specificRelDir = "dist"
  runIO $
    case lookup var env of
      Nothing -> putStrLn $ unwords [var, "not set, using the front-end in", localRelDir]
      Just v -> putStrLn $ unwords [var, "set, using the front-end in", v </> specificRelDir]
  let mFront pathLocal pathNix =
        case lookup var env of
          Nothing -> pathLocal
          Just d -> d </> pathNix
      mFrontF fp = mFront (localRelDir </> fp ++ ".js") (specificRelDir </> fp ++ ".js")
  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    [ embedFileAt "smos-web-server-front.js" $ mFrontF "smos-web-server-front",
      embedRemoteFileAt "hterm.js" "https://raw.githubusercontent.com/lehins/haskell-webshell/master/files/static/hterm_all.js",
      embedRemoteFileAt "jquery.js" "https://code.jquery.com/jquery-3.3.1.min.js"
    ]
