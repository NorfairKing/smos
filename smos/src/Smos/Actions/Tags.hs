{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Tags
  ( allTagsPlainActions,
    allTagsUsingCharActions,
    tagsSet,
    tagsUnset,
    tagsToggle,
    tagsInsert,
    tagsAppend,
    tagsRemove,
    tagsDelete,
    tagsPrev,
    tagsNext,
    tagsPrevTag,
    tagsNextTag,
  )
where

import Data.Maybe
import qualified Data.Text as T
import Smos.Actions.Utils
import Smos.Data.Types
import Smos.Types

allTagsPlainActions :: [Action]
allTagsPlainActions =
  concat
    [ do
        act <-
          [ tagsSet,
            tagsUnset,
            tagsToggle
            ]
        arg <-
          catMaybes
            [ tag "work",
              tag "online",
              tag "home",
              tag "offline",
              tag "toast",
              tag "external",
              tag "power",
              tag "code"
            ]
        pure $ act arg,
      [tagsRemove, tagsDelete, tagsPrev, tagsNext, tagsPrevTag, tagsNextTag]
    ]

allTagsUsingCharActions :: [ActionUsing Char]
allTagsUsingCharActions = [tagsInsert, tagsAppend]

tagsSet :: Tag -> Action
tagsSet t =
  Action
    { actionName = "tagsSet_" <> ActionName (tagText t),
      actionFunc = modifyMTagsCursorM $ tagsCursorSetTag t,
      actionDescription = T.unwords ["Set the", tagText t, "tag"]
    }

tagsUnset :: Tag -> Action
tagsUnset t =
  Action
    { actionName = "tagsUnset_" <> ActionName (tagText t),
      actionFunc = modifyTagsCursorMD $ tagsCursorUnsetTag t,
      actionDescription = T.unwords ["Unset the", tagText t, "tag"]
    }

tagsToggle :: Tag -> Action
tagsToggle t =
  Action
    { actionName = "tagsToggle_" <> ActionName (tagText t),
      actionFunc = modifyMTagsCursorD $ tagsCursorToggleTag t,
      actionDescription = T.unwords ["Toggle the", tagText t, "tag"]
    }

tagsInsert :: ActionUsing Char
tagsInsert =
  ActionUsing
    { actionUsingName = "tagsInsert",
      actionUsingFunc = \c -> modifyTagsCursorM $ tagsCursorInsert c,
      actionUsingDescription = "Insert a character at the cursor select the space after it"
    }

tagsAppend :: ActionUsing Char
tagsAppend =
  ActionUsing
    { actionUsingName = "tagsAppend",
      actionUsingFunc = \c -> modifyTagsCursorM $ tagsCursorAppend c,
      actionUsingDescription = "Insert a character at the cursor select the space before it"
    }

tagsRemove :: Action
tagsRemove =
  Action
    { actionName = "tagsRemove",
      actionFunc = modifyTagsCursorMD tagsCursorRemove,
      actionDescription = "Remove from the tags cursor"
    }

tagsDelete :: Action
tagsDelete =
  Action
    { actionName = "tagsDelete",
      actionFunc = modifyTagsCursorMD tagsCursorDelete,
      actionDescription = "Delete from the tags cursor"
    }

tagsPrev :: Action
tagsPrev =
  Action
    { actionName = "tagsPrev",
      actionFunc = modifyTagsCursor tagsCursorSelectOrCreatePrev,
      actionDescription = "Move left in the tags cursor"
    }

tagsNext :: Action
tagsNext =
  Action
    { actionName = "tagsNext",
      actionFunc = modifyTagsCursor tagsCursorSelectOrCreateNext,
      actionDescription = "Move right in the tags cursor"
    }

tagsPrevTag :: Action
tagsPrevTag =
  Action
    { actionName = "tagsPrevTag",
      actionFunc = modifyTagsCursor tagsCursorSelectOrCreatePrevTag,
      actionDescription = "Move to the next tag in the tags cursor"
    }

tagsNextTag :: Action
tagsNextTag =
  Action
    { actionName = "tagsNextTag",
      actionFunc = modifyTagsCursor tagsCursorSelectOrCreateNextTag,
      actionDescription = "Move to the previous tag in the tags cursor"
    }
