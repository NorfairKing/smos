{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry.Properties
  ( allPropertiesPlainActions,
    allPropertiesUsingCharActions,
    propertiesToggleSelected,
    propertiesMoveLeft,
    propertiesMoveRight,
    propertiesMoveUp,
    propertiesMoveDown,
    propertiesInsert,
    propertiesAppend,
    propertiesRemove,
    propertiesDelete,
    propertiesInsertNewProperty,
    propertiesAppendNewProperty,
    propertiesEditProperty,
    propertiesSetProperty,
  )
where

import Smos.Actions.Utils
import Smos.Data
import Smos.Types

allPropertiesPlainActions :: [Action]
allPropertiesPlainActions =
  concat
    [ [ propertiesToggleSelected,
        propertiesMoveLeft,
        propertiesMoveRight,
        propertiesMoveUp,
        propertiesMoveDown,
        propertiesRemove,
        propertiesDelete,
        propertiesInsertNewProperty,
        propertiesAppendNewProperty
      ],
      fmap
        propertiesEditProperty
        [ "assignee",
          "brainpower",
          "client",
          "effort",
          "estimate",
          "goal",
          "lead",
          "timewindow",
          "url"
        ]
    ]

allPropertiesUsingCharActions :: [ActionUsing Char]
allPropertiesUsingCharActions = [propertiesInsert, propertiesAppend]

propertiesToggleSelected :: Action
propertiesToggleSelected =
  Action
    { actionName = "propertiesToggleSelected",
      actionFunc = modifyPropertiesCursor propertiesCursorToggleSelected,
      actionDescription =
        "Switch the properties cursor selection between the property name and value"
    }

propertiesMoveLeft :: Action
propertiesMoveLeft =
  Action
    { actionName = "propertiesMoveLeft",
      actionFunc = modifyPropertiesCursorM propertiesCursorSelectPrevChar,
      actionDescription = "Move left in the properties cursor"
    }

propertiesMoveRight :: Action
propertiesMoveRight =
  Action
    { actionName = "propertiesMoveRight",
      actionFunc = modifyPropertiesCursorM propertiesCursorSelectNextChar,
      actionDescription = "Move right in the properties cursor"
    }

propertiesMoveUp :: Action
propertiesMoveUp =
  Action
    { actionName = "propertiesMoveUp",
      actionFunc = modifyPropertiesCursorM propertiesCursorSelectPrevProperty,
      actionDescription = "Move up in the properties cursor"
    }

propertiesMoveDown :: Action
propertiesMoveDown =
  Action
    { actionName = "propertiesMoveDown",
      actionFunc = modifyPropertiesCursorM propertiesCursorSelectNextProperty,
      actionDescription = "Move down in the properties cursor"
    }

propertiesInsert :: ActionUsing Char
propertiesInsert =
  ActionUsing
    { actionUsingName = "propertiesInsert",
      actionUsingFunc = \c -> do
        modifyPropertiesCursorM $ propertiesCursorInsert c
        unrecordFileCursorHistory,
      actionUsingDescription = "Insert a character at the cursor select the space after it"
    }

propertiesAppend :: ActionUsing Char
propertiesAppend =
  ActionUsing
    { actionUsingName = "propertiesAppend",
      actionUsingFunc = \c -> do
        modifyPropertiesCursorM $ propertiesCursorAppend c
        unrecordFileCursorHistory,
      actionUsingDescription = "Insert a character at the cursor select the space before it"
    }

propertiesRemove :: Action
propertiesRemove =
  Action
    { actionName = "propertiesRemove",
      actionFunc = do
        modifyPropertiesCursorMD propertiesCursorRemove
        unrecordFileCursorHistory,
      actionDescription = "Remove from the properties cursor"
    }

propertiesDelete :: Action
propertiesDelete =
  Action
    { actionName = "propertiesDelete",
      actionFunc = do
        modifyPropertiesCursorMD propertiesCursorDelete
        unrecordFileCursorHistory,
      actionDescription = "Delete from the properties cursor"
    }

propertiesInsertNewProperty :: Action
propertiesInsertNewProperty =
  Action
    { actionName = "propertiesInsertNewProperty",
      actionFunc = do
        modifyPropertiesCursor propertiesCursorStartNewPropertyBefore
        unrecordFileCursorHistory,
      actionDescription = "Insert a new property before the currently selected property"
    }

propertiesAppendNewProperty :: Action
propertiesAppendNewProperty =
  Action
    { actionName = "propertiesAppendNewProperty",
      actionFunc = do
        modifyPropertiesCursor propertiesCursorStartNewPropertyAfter
        unrecordFileCursorHistory,
      actionDescription = "Append a new property before the currently selected property"
    }

propertiesEditProperty :: PropertyName -> Action
propertiesEditProperty pn =
  Action
    { actionName = "propertiesEditProperty_" <> ActionName (propertyNameText pn),
      actionFunc = do
        modifyMPropertiesCursorM $ Just . propertiesCursorAddOrSelect pn
        modifyEntryCursor entryCursorSelectProperties
        unrecordFileCursorHistory,
      actionDescription =
        "Start editing a property with the given name, create it if it does not exist yet"
    }

propertiesSetProperty :: PropertyName -> PropertyValue -> Action
propertiesSetProperty pn pv =
  Action
    { actionName =
        mconcat
          [ "propertiesSetProperty_",
            ActionName (propertyNameText pn),
            "_",
            ActionName (propertyValueText pv)
          ],
      actionFunc = modifyMPropertiesCursorM $ Just . propertiesCursorSet pn pv,
      actionDescription =
        "Set a property with the given name to the given value, create it if it does not exist yet"
    }
