{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Properties
    ( allPropertiesPlainActions
    , allPropertiesUsingCharActions
    , propertiesToggleSelected
    , propertiesMoveLeft
    , propertiesMoveRight
    , propertiesInsert
    , propertiesAppend
    , propertiesRemove
    , propertiesDelete
    , propertiesInsertNewProperty
    , propertiesAppendNewProperty
    ) where

import Smos.Types

import Smos.Actions.Utils

allPropertiesPlainActions :: [Action]
allPropertiesPlainActions =
    [ propertiesToggleSelected
    , propertiesMoveLeft
    , propertiesMoveRight
    , propertiesRemove
    , propertiesDelete
    , propertiesInsertNewProperty
    , propertiesAppendNewProperty
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
    , actionDescription = "Move left in the properties cursor"
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
