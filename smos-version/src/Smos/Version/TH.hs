module Smos.Version.TH where

import GitHash
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment

mkVersionInfo :: Q Exp
mkVersionInfo = do
  errOrGitInfo <- runIO $ do
    eroot <- getGitRoot "."
    case eroot of
      Left e -> return $ Left $ show e
      Right root -> do
        einfo <- getGitInfo root
        case einfo of
          Left e -> return $ Left $ show e
          Right info -> return $ Right info
  case errOrGitInfo of
    Right gi -> lift $ makeVersionInfo gi
    Left err -> do
      env <- runIO getEnvironment
      case lookup "SMOS_GIT_INFO" env of
        Nothing -> fail err
        Just gisPath -> runIO (readFile gisPath) >>= lift

makeVersionInfo :: GitInfo -> String
makeVersionInfo gi = giHash gi
