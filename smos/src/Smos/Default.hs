{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Smos

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig =
    SmosConfig
    { configKeyMap = defaultKeyMap
    , configReportsKeyMap = defaultReportsKeymap
    , configReportConfig = defaultReportConfig
    }

defaultKeyMap :: KeyMap
defaultKeyMap =
    KeyMap
    { keyMapEmptyMatchers =
          listMatchers
              [ exactChar 'q' stop
              , exactKey KEsc stop
              , exactChar 'h' startHeaderFromEmptyAndSelectHeader
              , exactChar 'H' startHeaderFromEmptyAndSelectHeader
                  -- Reports
              , exactString "rn" reportNextActions
              ]
    , keyMapEntryMatchers =
          listMatchers
              [ exactChar 'q' stop
              , exactChar 'w' saveFile
                  -- Selections
              , exactChar 'a' entrySelectHeaderAtEnd
              , exactChar 'A' entrySelectHeaderAtEnd
              , exactChar 'i' entrySelectHeaderAtStart
              , exactChar 'I' entrySelectHeaderAtStart
                  -- Movements
              , exactChar 'k' forestMoveUp
              , exactKey KUp forestMoveUp
              , exactChar 'j' forestMoveDown
              , exactKey KDown forestMoveDown
              , exactString "gg" forestMoveToFirst
              , exactChar 'G' forestMoveToLast
                  -- Swaps
              , modifiedChar 'k' [MMeta] forestSwapUp
              , modifiedChar 'j' [MMeta] forestSwapDown
              , modifiedChar 'h' [MMeta] forestPromoteEntry
              , modifiedChar 'H' [MMeta] forestPromoteSubTree
              , modifiedChar 'l' [MMeta] forestDemoteEntry
              , modifiedChar 'L' [MMeta] forestDemoteSubTree
                  -- Forest manipulation
              , exactChar 'h' forestInsertEntryAfterAndSelectHeader
              , exactChar 'H' forestInsertEntryBelowAndSelectHeader
                  -- Deletion
              , exactChar 'd' forestDeleteCurrentEntry
              , exactChar 'D' forestDeleteCurrentSubTree
                  -- Fast todo state manipulation
              , exactString "tt" $ entrySetTodoState "TODO"
              , exactString "tn" $ entrySetTodoState "NEXT"
              , exactString "ts" $ entrySetTodoState "STARTED"
              , exactString "tr" $ entrySetTodoState "READY"
              , exactString "tw" $ entrySetTodoState "WAITING"
              , exactString "td" $ entrySetTodoState "DONE"
              , exactString "tc" $ entrySetTodoState "CANCELLED"
              , exactString "t " entryUnsetTodoState
              , exactString "Tt" $ subtreeSetTodoState "TODO"
              , exactString "Tn" $ subtreeSetTodoState "NEXT"
              , exactString "Ts" $ subtreeSetTodoState "STARTED"
              , exactString "Tr" $ subtreeSetTodoState "READY"
              , exactString "Tw" $ subtreeSetTodoState "WAITING"
              , exactString "Td" $ subtreeSetTodoState "DONE"
              , exactString "Tc" $ subtreeSetTodoState "CANCELLED"
              , exactString "T " subtreeUnsetTodoState
                  -- Fast tag manipulation
              , exactString "gw" $ tagsToggle "work"
              , exactString "go" $ tagsToggle "online"
                  -- Fast timestamps manipulation
              , exactString "sb" $ timestampsSelect "BEGIN"
              , exactString "se" $ timestampsSelect "END"
              , exactString "sd" $ timestampsSelect "DEADLINE"
              , exactString "ss" $ timestampsSelect "SCHEDULED"
                  -- Clocking
              , exactString
                    "ci"
                    forestClockOutEverywhereInAllFilesAndClockInHere
              , exactString "co" forestClockOutEverywhereInAllFiles
                  -- Reports
              , exactString "rn" reportNextActions
                  -- Convenience
              , exactString " nw" convDoneAndWaitForResponse
              , exactString " rp" convRepinged
                  -- Collapsing
              , exactChar '?' selectHelp
              , exactChar '\t' forestToggleCollapse
              , exactKeyPress
                    (KeyPress (KChar '\t') [MMeta])
                    forestToggleCollapseRecursively
              , exactKey KBackTab forestToggleHideEntireEntry
                  -- Entering contents
              , combo
                    [KeyPress KEnter [], KeyPress KEnter []]
                    entrySelectContents
                  -- Entering tags
              , exactString "gi" entrySelectTags
              ]
    , keyMapHeaderMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKey KEnter entrySelectWhole
              , anyChar headerInsert
              , exactKey KBS headerRemove
              , exactKey KDel headerDelete
              , exactKey KLeft headerMoveLeft
              , exactKey KRight headerMoveRight
              , exactKey KHome headerMoveToStart
              , exactKey KEnd headerMoveToEnd
              , modifiedChar 'k' [MMeta] forestSwapUp
              , modifiedChar 'j' [MMeta] forestSwapDown
              , modifiedChar 'h' [MMeta] forestPromoteEntry
              , modifiedChar 'H' [MMeta] forestPromoteSubTree
              , modifiedChar 'l' [MMeta] forestDemoteEntry
              , modifiedChar 'L' [MMeta] forestDemoteSubTree
              ]
    , keyMapContentsMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , anyChar contentsInsert
              , exactKey KEnter contentsInsertNewline
              , exactKey KBS contentsRemove
              , exactKey KDel contentsDelete
              , exactKey KLeft contentsMoveLeft
              , exactKey KRight contentsMoveRight
              , exactKey KUp contentsMoveUp
              , exactKey KDown contentsMoveDown
              , exactKey KHome contentsMoveToStartOfLine
              , exactKey KEnd contentsMoveToEndOfLine
              , modifiedChar 'k' [MMeta] forestSwapUp
              , modifiedChar 'j' [MMeta] forestSwapDown
              , modifiedChar 'h' [MMeta] forestPromoteEntry
              , modifiedChar 'H' [MMeta] forestPromoteSubTree
              , modifiedChar 'l' [MMeta] forestDemoteEntry
              , modifiedChar 'L' [MMeta] forestDemoteSubTree
              ]
    , keyMapTimestampsMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKey KEnter entrySelectWhole
              , anyChar timestampsInsert
              , exactKey KLeft timestampsMoveLeft
              , exactKey KRight timestampsMoveRight
              , exactKey KBS timestampsRemove
              , exactKey KDel timestampsDelete
              , exactChar '\t' timestampsToggle
              ]
    , keyMapPropertiesMatchers = listMatchers [exactKey KEsc entrySelectWhole]
    , keyMapStateHistoryMatchers = listMatchers [exactKey KEsc entrySelectWhole]
    , keyMapTagsMatchers = listMatchers [
                anyChar tagsInsert
            , exactKey KBS tagsRemove, exactKey KDel tagsDelete
        ,exactKey KEsc entrySelectWhole]
    , keyMapLogbookMatchers = listMatchers [exactKey KEsc entrySelectWhole]
    , keyMapHelpMatchers =
          listMatchers
              [ exactKey KUp helpUp
              , exactChar 'k' helpUp
              , exactKey KDown helpDown
              , exactChar 'j' helpDown
              , exactKey KHome helpStart
              , exactString "gg" helpStart
              , exactKey KEnd helpEnd
              , exactChar 'G' helpEnd
              , exactKey KEsc selectEditor
              , exactChar 'q' selectEditor
              ]
    , keyMapAnyMatchers =
          listMatchers
              [ exactChar 'u' undo
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    }

defaultReportsKeymap :: ReportsKeyMap
defaultReportsKeymap =
    ReportsKeyMap
    { reportsKeymapNextActionReportMatchers =
          listMatchers
              [ exactKey KUp prevNextAction
              , exactChar 'k' prevNextAction
              , exactKey KDown nextNextAction
              , exactChar 'j' nextNextAction
              , exactKey KEsc selectEditor
              , exactKey KHome firstNextAction
              , exactString "gg" firstNextAction
              , exactKey KEnd lastNextAction
              , exactChar 'G' lastNextAction
              , exactChar 'q' selectEditor
              , exactKey KEnter enterNextActionFile
              , exactChar '?' selectHelp
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              ]
    }
