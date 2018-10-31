{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Tags
    ( allTagsPlainActions
    , allTagsUsingCharActions
    , tagsSet
    , tagsUnset
    , tagsToggle
    , tagsInsert
    , tagsAppend
    ) where

import Data.Maybe
import qualified Data.Text as T

import Smos.Data.Types

import Smos.Types

import Smos.Actions.Utils

allTagsPlainActions :: [Action]
allTagsPlainActions = do
    act <- [tagsSet, tagsUnset, tagsToggle]
    arg <- catMaybes [tag "work", tag "online"]
    pure $ act arg

allTagsUsingCharActions :: [ActionUsing Char]
allTagsUsingCharActions = [tagsInsert, tagsAppend]

tagsSet :: Tag -> Action
tagsSet t =
    Action
    { actionName = "tagsSet_" <> tagText t
    , actionFunc = modifyMTagsCursorM $ tagsCursorSetTag t
    , actionDescription = T.unwords ["Set the", tagText t, "tag"]
    }

tagsUnset :: Tag -> Action
tagsUnset t =
    Action
    { actionName = "tagsUnset_" <> tagText t
    , actionFunc = modifyTagsCursorMD $ tagsCursorUnsetTag t
    , actionDescription = T.unwords ["Unset the", tagText t, "tag"]
    }

tagsToggle :: Tag -> Action
tagsToggle t =
    Action
    { actionName = "tagsToggle_" <> tagText t
    , actionFunc = modifyMTagsCursorD $ tagsCursorToggleTag t
    , actionDescription = T.unwords ["Toggle the", tagText t, "tag"]
    }

tagsInsert :: ActionUsing Char
tagsInsert =
    ActionUsing
    { actionUsingName = "tagsInsert"
    , actionUsingFunc = \c -> modifyTagsCursorM $ tagsCursorInsert c
    , actionUsingDescription =
          "Insert a character at the cursor select the space after it"
    }

tagsAppend :: ActionUsing Char
tagsAppend =
    ActionUsing
    { actionUsingName = "tagsAppend"
    , actionUsingFunc = \c -> modifyTagsCursorM $ tagsCursorAppend c
    , actionUsingDescription =
          "Insert a character at the cursor select the space before it"
    }
