{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Properties
    ( allPropertiesPlainActions
    , allPropertiesUsingCharActions
    , propertiesToggleSelected
    , propertiesMoveLeft
    , propertiesMoveRight
    , propertiesMoveUp
    , propertiesMoveDown
    , propertiesInsert
    , propertiesAppend
    , propertiesRemove
    , propertiesDelete
    , propertiesInsertNewProperty
    , propertiesAppendNewProperty
    , propertiesEditProperty
    ) where

import Smos.Data

import Smos.Types

import Smos.Actions.Utils

allPropertiesPlainActions :: [Action]
allPropertiesPlainActions =
    concat
        [ [ propertiesToggleSelected
          , propertiesMoveLeft
          , propertiesMoveRight
          , propertiesMoveUp
          , propertiesMoveDown
          , propertiesRemove
          , propertiesDelete
          , propertiesInsertNewProperty
          , propertiesAppendNewProperty
          ]
        , do pn <- ["client", "timebox", "lead"]
             pure $ propertiesEditProperty pn
        ]

allPropertiesUsingCharActions :: [ActionUsing Char]
allPropertiesUsingCharActions = [propertiesInsert, propertiesAppend]

propertiesToggleSelected :: Action
propertiesToggleSelected =
    Action
    { actionName = "propertiesToggleSelected"
    , actionFunc = modifyPropertiesCursor propertiesCursorToggleSelected
    , actionDescription =
          "Switch the properties cursor selection between the property name and value"
    }

propertiesMoveLeft :: Action
propertiesMoveLeft =
    Action
    { actionName = "propertiesMoveLeft"
    , actionFunc = modifyPropertiesCursorM propertiesCursorSelectPrevChar
    , actionDescription = "Move left in the properties cursor"
    }

propertiesMoveRight :: Action
propertiesMoveRight =
    Action
    { actionName = "propertiesMoveRight"
    , actionFunc = modifyPropertiesCursorM propertiesCursorSelectNextChar
    , actionDescription = "Move right in the properties cursor"
    }

propertiesMoveUp :: Action
propertiesMoveUp =
    Action
    { actionName = "propertiesMoveUp"
    , actionFunc = modifyPropertiesCursorM propertiesCursorSelectPrevProperty
    , actionDescription = "Move up in the properties cursor"
    }

propertiesMoveDown :: Action
propertiesMoveDown =
    Action
    { actionName = "propertiesMoveDown"
    , actionFunc = modifyPropertiesCursorM propertiesCursorSelectNextProperty
    , actionDescription = "Move down in the properties cursor"
    }

propertiesInsert :: ActionUsing Char
propertiesInsert =
    ActionUsing
    { actionUsingName = "propertiesInsert"
    , actionUsingFunc = \c -> modifyPropertiesCursorM $ propertiesCursorInsert c
    , actionUsingDescription =
          "Insert a character at the cursor select the space after it"
    }

propertiesAppend :: ActionUsing Char
propertiesAppend =
    ActionUsing
    { actionUsingName = "propertiesAppend"
    , actionUsingFunc = \c -> modifyPropertiesCursorM $ propertiesCursorAppend c
    , actionUsingDescription =
          "Insert a character at the cursor select the space before it"
    }

propertiesRemove :: Action
propertiesRemove =
    Action
    { actionName = "propertiesRemove"
    , actionFunc = modifyPropertiesCursorMD propertiesCursorRemove
    , actionDescription = "Remove from the properties cursor"
    }

propertiesDelete :: Action
propertiesDelete =
    Action
    { actionName = "propertiesDelete"
    , actionFunc = modifyPropertiesCursorMD propertiesCursorDelete
    , actionDescription = "Delete from the properties cursor"
    }

propertiesInsertNewProperty :: Action
propertiesInsertNewProperty =
    Action
    { actionName = "propertiesInsertNewProperty"
    , actionFunc = modifyPropertiesCursor propertiesCursorStartNewPropertyBefore
    , actionDescription =
          "Insert a new property before the currently selected property"
    }

propertiesAppendNewProperty :: Action
propertiesAppendNewProperty =
    Action
    { actionName = "propertiesAppendNewProperty"
    , actionFunc = modifyPropertiesCursor propertiesCursorStartNewPropertyAfter
    , actionDescription =
          "Append a new property before the currently selected property"
    }

propertiesEditProperty :: PropertyName -> Action
propertiesEditProperty pn =
    Action
    { actionName = "propertiesEditProperty_" <> ActionName (propertyNameText pn)
    , actionFunc = do
             modifyMPropertiesCursorM $ Just . propertiesCursorAddOrSelect pn
             modifyEntryCursor entryCursorSelectProperties
    , actionDescription =
          "Start editing a property with the given name, create it if it does not exist yet"
    }
