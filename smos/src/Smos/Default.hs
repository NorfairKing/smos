{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Smos

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig =
  SmosConfig
    { configKeyMap = defaultKeyMap,
      configReportConfig = defaultReportConfig,
      configExplainerMode = False
    }

defaultKeyMap :: KeyMap
defaultKeyMap =
  KeyMap
    { keyMapFileKeyMap = defaultFileKeyMap,
      keyMapBrowserKeyMap = defaultBrowserKeyMap,
      keyMapReportsKeyMap = defaultReportsKeyMap,
      keyMapHelpKeyMap = defaultHelpKeyMap,
      keyMapAnyKeyMap = defaultAnyKeyMap
    }

defaultFileKeyMap :: FileKeyMap
defaultFileKeyMap =
  FileKeyMap
    { fileKeyMapEmptyMatchers =
        listMatchers
          [ exactChar 'q' stop,
            exactKey KEsc stop,
            exactChar 'e' startHeaderFromEmptyAndSelectHeader,
            exactChar 'E' startHeaderFromEmptyAndSelectHeader,
            exactChar '?' selectHelp,
            exactString "co" forestClockOutEverywhereInAllFiles
          ],
      fileKeyMapEntryMatchers =
        listMatchers
          [ exactChar 'q' stop,
            exactChar 'w' saveFile,
            -- Selections
            exactChar 'a' entrySelectHeaderAtEnd,
            exactChar 'A' entrySelectHeaderAtEnd,
            exactChar 'i' entrySelectHeaderAtStart,
            exactChar 'I' entrySelectHeaderAtStart,
            -- Movements
            exactChar 'k' forestMoveUp,
            exactKey KUp forestMoveUp,
            exactChar 'j' forestMoveDown,
            exactKey KDown forestMoveDown,
            exactChar 'h' forestMoveLeft,
            exactKey KLeft forestMoveLeft,
            exactChar 'l' forestMoveRight,
            exactKey KRight forestMoveRight,
            exactString "gg" forestMoveToFirst,
            exactChar 'G' forestMoveToLast,
            -- Swaps
            modifiedChar 'k' [MMeta] forestSwapUp,
            modifiedChar 'K' [MMeta] forestSwapUp,
            modifiedChar 'j' [MMeta] forestSwapDown,
            modifiedChar 'J' [MMeta] forestSwapDown,
            modifiedChar 'h' [MMeta] forestPromoteEntry,
            modifiedChar 'H' [MMeta] forestPromoteSubTree,
            modifiedChar 'l' [MMeta] forestDemoteEntry,
            modifiedChar 'L' [MMeta] forestDemoteSubTree,
            -- Forest manipulation
            exactChar 'e' forestInsertEntryAfterAndSelectHeader,
            exactChar 'E' forestInsertEntryBelowAndSelectHeader,
            -- Deletion
            exactChar 'd' forestDeleteCurrentEntry,
            exactChar 'D' forestDeleteCurrentSubTree,
            -- Fast todo state manipulation
            exactString "tt" $ entrySetTodoState "TODO",
            exactString "tn" $ entrySetTodoState "NEXT",
            exactString "ts" $ entrySetTodoState "STARTED",
            exactString "tr" $ entrySetTodoState "READY",
            exactString "tw" $ entrySetTodoState "WAITING",
            exactString "td" $ entrySetTodoState "DONE",
            exactString "tc" $ entrySetTodoState "CANCELLED",
            exactString "tf" $ entrySetTodoState "FAILED",
            exactString "t " entryUnsetTodoState,
            exactString "Tt" $ subtreeSetTodoState "TODO",
            exactString "Tn" $ subtreeSetTodoState "NEXT",
            exactString "Ts" $ subtreeSetTodoState "STARTED",
            exactString "Tr" $ subtreeSetTodoState "READY",
            exactString "Tw" $ subtreeSetTodoState "WAITING",
            exactString "Td" $ subtreeSetTodoState "DONE",
            exactString "Tc" $ subtreeSetTodoState "CANCELLED",
            exactString "Tf" $ subtreeSetTodoState "FAILED",
            exactString "T " subtreeUnsetTodoState,
            -- Fast tag manipulation
            exactString "gh" $ tagsToggle "home",
            exactString "gon" $ tagsToggle "online",
            exactString "gof" $ tagsToggle "offline",
            exactString "gt" $ tagsToggle "toast",
            exactString "gw" $ tagsToggle "work",
            exactString "ge" $ tagsToggle "external",
            exactString "gp" $ tagsToggle "power",
            exactString "gc" $ tagsToggle "code",
            -- Fast timestamps manipulation
            exactString "sb" $ timestampsSelect "BEGIN",
            exactString "se" $ timestampsSelect "END",
            exactString "sd" $ timestampsSelect "DEADLINE",
            exactString "ss" $ timestampsSelect "SCHEDULED",
            exactString "pi" entrySelectProperties,
            exactString "pc" $ propertiesEditProperty "client",
            exactString "pt" $ propertiesEditProperty "timewindow",
            exactString "pb" $ propertiesEditProperty "brainpower",
            -- Clocking
            exactString "ci" forestClockOutEverywhereInAllFilesAndClockInHere,
            exactString "co" forestClockOutEverywhereInAllFiles,
            -- Reports
            exactString "rn" reportNextActions,
            -- Convenience
            exactString " nw" convDoneAndWaitForResponse,
            exactString " rp" convRepinged,
            exactString " rw" convRespondedButStillWaiting,
            exactString " ce" convNewEntryAndClockIn,
            -- Collapsing
            exactChar '?' selectHelp,
            exactChar '\t' forestToggleCollapse,
            exactKeyPress (KeyPress (KChar '\t') [MMeta]) forestToggleCollapseRecursively,
            exactKey KBackTab forestToggleCollapseEntireEntry,
            exactString "ze" forestToggleCollapseEntireEntry,
            exactString "zc" forestToggleCollapseEntryContents,
            exactString "zh" forestToggleCollapseEntryHistory,
            exactString "zl" forestToggleCollapseEntryLogbook,
            -- Entering contents
            combo [KeyPress KEnter [], KeyPress KEnter []] entrySelectContents,
            -- Entering tags
            exactString "gi" entrySelectTagsFromStart,
            exactString "ga" entrySelectTagsFromBack
          ],
      fileKeyMapHeaderMatchers =
        listMatchers
          [ exactKey KEnter entrySelectContents,
            anyChar headerInsert,
            exactKey KBS headerRemove,
            exactKey KDel headerDelete,
            exactKey KLeft headerMoveLeft,
            exactKey KRight headerMoveRight,
            exactKey KHome headerMoveToStart,
            modifiedChar 'a' [MCtrl] headerMoveToStart,
            exactKey KEnd headerMoveToEnd,
            modifiedChar 'e' [MCtrl] headerMoveToEnd,
            modifiedChar 'k' [MMeta] forestSwapUp,
            modifiedChar 'j' [MMeta] forestSwapDown,
            modifiedChar 'h' [MMeta] forestPromoteEntry,
            modifiedChar 'H' [MMeta] forestPromoteSubTree,
            modifiedChar 'l' [MMeta] forestDemoteEntry,
            modifiedChar 'L' [MMeta] forestDemoteSubTree
          ],
      fileKeyMapContentsMatchers =
        listMatchers
          [ anyChar contentsInsert,
            exactKey KEnter contentsInsertNewline,
            exactKey KBS contentsRemove,
            exactKey KDel contentsDelete,
            exactKey KLeft contentsMoveLeft,
            exactKey KRight contentsMoveRight,
            exactKey KUp contentsMoveUp,
            exactKey KDown contentsMoveDown,
            exactKey KHome contentsMoveToStartOfLine,
            modifiedChar 'a' [MCtrl] contentsMoveToStartOfLine,
            exactKey KEnd contentsMoveToEndOfLine,
            modifiedChar 'e' [MCtrl] contentsMoveToEndOfLine,
            modifiedChar 'k' [MMeta] forestSwapUp,
            modifiedChar 'j' [MMeta] forestSwapDown,
            modifiedChar 'h' [MMeta] forestPromoteEntry,
            modifiedChar 'H' [MMeta] forestPromoteSubTree,
            modifiedChar 'l' [MMeta] forestDemoteEntry,
            modifiedChar 'L' [MMeta] forestDemoteSubTree
          ],
      fileKeyMapTimestampsMatchers =
        listMatchers
          [ exactKey KEnter entrySelectWhole,
            anyChar timestampsInsert,
            exactKey KLeft timestampsMoveLeft,
            exactKey KRight timestampsMoveRight,
            exactKey KBS timestampsRemove,
            exactKey KDel timestampsDelete,
            exactChar '\t' timestampsToggle
          ],
      fileKeyMapPropertiesMatchers =
        listMatchers
          [ exactKey KEnter entrySelectWhole,
            anyChar propertiesInsert,
            exactKey KLeft propertiesMoveLeft,
            exactKey KRight propertiesMoveRight,
            exactKey KUp propertiesMoveUp,
            exactKey KDown propertiesMoveDown,
            exactKey KBS propertiesRemove,
            exactKey KDel propertiesDelete,
            modifiedChar 'k' [MMeta] propertiesInsertNewProperty,
            exactKeyPress (KeyPress KUp [MMeta]) propertiesInsertNewProperty,
            modifiedChar 'j' [MMeta] propertiesAppendNewProperty,
            exactKeyPress (KeyPress KDown [MMeta]) propertiesAppendNewProperty,
            exactChar '\t' propertiesToggleSelected
          ],
      fileKeyMapStateHistoryMatchers = listMatchers [],
      fileKeyMapTagsMatchers =
        listMatchers
          [ anyChar tagsInsert,
            exactKey KLeft tagsPrev,
            exactKey KRight tagsNext,
            exactChar '\t' tagsNextTag,
            exactKey KBackTab tagsPrevTag,
            exactKey KBS tagsRemove,
            exactKey KDel tagsDelete,
            exactKey KEnter entrySelectWhole,
            exactKey KEsc entrySelectWhole,
            exactChar ':' tagsSplit
          ],
      fileKeyMapLogbookMatchers = listMatchers [],
      fileKeyMapAnyMatchers =
        listMatchers
          [ exactChar 'u' undo,
            modifiedChar 'u' [MMeta] redo,
            exactKey KEsc entrySelectWhole
          ]
    }

defaultBrowserKeyMap :: KeyMappings
defaultBrowserKeyMap =
  listMatchers
    [ exactKey KDown browserSelectNext,
      exactChar 'j' browserSelectNext,
      exactKey KUp browserSelectPrev,
      exactChar 'k' browserSelectPrev,
      exactChar '\t' browserToggleCollapse,
      exactKey KEnter browserEnter,
      exactKey KBackTab browserToggleCollapseRecursively,
      exactString "ded" browserRemoveEmptyDir,
      exactChar 'a' browserArchive,
      exactChar 'u' browserUndo,
      exactChar 'r' browserRedo,
      exactKey KEsc selectEditor,
      exactChar 'q' stop
    ]

defaultReportsKeyMap :: ReportsKeyMap
defaultReportsKeyMap =
  ReportsKeyMap
    { reportsKeymapNextActionReportMatchers =
        listMatchers
          [ exactKey KUp prevNextAction,
            exactChar 'k' prevNextAction,
            exactKey KDown nextNextAction,
            exactChar 'j' nextNextAction,
            exactKey KHome firstNextAction,
            exactString "gg" firstNextAction,
            exactKey KEnd lastNextAction,
            exactChar 'G' lastNextAction,
            exactKey KEnter enterNextActionFile,
            exactChar '?' selectHelp,
            exactChar '/' selectNextActionFilter,
            exactKey KEsc selectEditor,
            exactChar 'q' selectEditor
          ],
      reportsKeymapNextActionReportFilterMatchers =
        listMatchers
          [ anyChar insertNextActionFilter,
            exactKey KEnter selectNextActionReport,
            exactKey KBS removeNextActionFilter,
            exactKey KDel deleteNextActionFilter,
            exactKey KEsc selectEditor
          ]
    }

defaultHelpKeyMap :: HelpKeyMap
defaultHelpKeyMap =
  HelpKeyMap
    { helpKeyMapHelpMatchers =
        listMatchers
          [ exactKey KUp helpUp,
            exactChar 'k' helpUp,
            exactKey KDown helpDown,
            exactChar 'j' helpDown,
            exactKey KHome helpStart,
            exactString "gg" helpStart,
            exactKey KEnd helpEnd,
            exactChar 'G' helpEnd,
            exactChar '/' helpSelectSearch,
            exactKey KEsc selectEditor,
            exactChar 'q' selectEditor
          ],
      helpKeyMapSearchMatchers =
        listMatchers
          [ anyChar helpInsert,
            exactKey KBS helpRemove,
            exactKey KDel helpDelete,
            exactKey KEsc selectEditor,
            exactKey KEnter helpSelectHelp
          ]
    }

defaultAnyKeyMap :: KeyMappings
defaultAnyKeyMap =
  listMatchers
    [ exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp,
      -- Browser
      exactString "bp" selectBrowserProjects,
      exactString "bw" selectBrowserWorkflow,
      exactString "ba" selectBrowserArchive,
      exactString "br" selectBrowserReview,
      exactString "bc" selectBrowserClient,
      -- Reports
      exactString "rn" reportNextActions
    ]
