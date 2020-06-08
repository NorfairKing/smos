{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Cursor.Tree
import Data.Aeson.Text as JSON
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Tree
import Smos.Data
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Text.Julius
import Yesod hiding (Header)
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addScript $ StaticR smos_web_server_front_js
  let startingTreeCursor =
        TreeCursor
          { treeAbove =
              Just
                ( TreeAbove
                    { treeAboveLefts = [CNode "left" EmptyCForest],
                      treeAboveAbove = Nothing,
                      treeAboveNode = "hello",
                      treeAboveRights = [CNode "right" EmptyCForest, CNode "Right2" EmptyCForest]
                    }
                ),
            treeBelow =
              OpenForest $ CNode "below" EmptyCForest :| [],
            treeCurrent = "world"
          } ::
          TreeCursor Text Text
      startingTree = rebuildCTree $ rebuildTreeCursor id startingTreeCursor :: Tree Text
      encodedTree = JSON.encodeToLazyText (ForYaml (fmap (newEntry . Header) startingTree))
  $(widgetFile "home")
