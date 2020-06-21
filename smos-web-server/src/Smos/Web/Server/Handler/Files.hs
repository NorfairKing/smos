{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Files where

import Data.DirForest (DirForest (..), DirTree (..))
import Data.List (sortOn)
import qualified Data.Map as M
import Path
import Smos.Client hiding (Header)
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import qualified System.FilePath as FP
import Text.Show.Pretty (ppShow)
import Yesod hiding (Header)

getFilesR :: Handler Html
getFilesR = withLogin $ \t -> do
  df <- runClientOrErr $ clientGetListSmosFiles t
  defaultLayout $(widgetFile "files")

dirForestWidget :: DirForest a -> Widget
dirForestWidget = dirForestWidget' ""

dirForestWidget' :: FilePath -> DirForest a -> Widget
dirForestWidget' dir (DirForest m) =
  let tups = sortOn fst $ M.toList m
   in mconcat $ map (uncurry (dirTreeWidget dir)) tups

dirTreeWidget :: FilePath -> FilePath -> DirTree a -> Widget
dirTreeWidget dir fp = \case
  NodeDir df ->
    let dir' = dir FP.</> fp
     in [whamlet|
          <div>
            #{fp}
          <div style="margin-left: 30px;">
            ^{dirForestWidget' dir' df}
        |]
  NodeFile _ ->
    case parseRelFile $ dir FP.</> fp of
      Nothing -> "<Invalid filepath>"
      Just rf ->
        [whamlet|
          <div>
            <a href=@{fileR rf}>
              #{fp}
        |]
