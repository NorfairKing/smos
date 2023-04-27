{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Smos

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig =
  SmosConfig
    { configKeyMap = defaultKeyMap,
      configReportSettings = defaultReportSettings,
      configExplainerMode = True,
      configSandboxMode = False
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
          [ exactChar 'e' startEntryFromEmptyAndSelectHeader,
            exactChar 'E' startEntryFromEmptyAndSelectHeader,
            exactKeyPress (KeyPress (KChar 'e') [MMeta]) startEntryFromEmptyAndSelectHeader,
            exactString "co" forestClockOutEverywhereInAllFiles
          ],
      fileKeyMapEntryMatchers =
        listMatchers
          [ exactChar 'w' saveFile,
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
            exactChar 'E' forestInsertEntryBelowAtEndAndSelectHeader,
            exactKeyPress (KeyPress (KChar 'e') [MMeta]) forestInsertEntryBelowAtStartAndSelectHeader,
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
            exactString "TT" $ subtreeSetTodoState "TODO",
            exactString "TN" $ subtreeSetTodoState "NEXT",
            exactString "TS" $ subtreeSetTodoState "STARTED",
            exactString "TR" $ subtreeSetTodoState "READY",
            exactString "TW" $ subtreeSetTodoState "WAITING",
            exactString "TD" $ subtreeSetTodoState "DONE",
            exactString "TC" $ subtreeSetTodoState "CANCELLED",
            exactString "TF" $ subtreeSetTodoState "FAILED",
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
            exactString "sa" $ timestampsSelect "AFTER",
            exactString "sb" $ timestampsSelect "BEGIN",
            exactString "sd" $ timestampsSelect "DEADLINE",
            exactString "se" $ timestampsSelect "END",
            exactString "ss" $ timestampsSelect "SCHEDULED",
            exactString "pi" entrySelectProperties,
            exactString "pa" $ propertiesEditProperty "assignee",
            exactString "pb" $ propertiesEditProperty "brainpower",
            exactString "pc" $ propertiesEditProperty "client",
            exactString "pe" $ propertiesEditProperty "email_address",
            exactString "pg" $ propertiesEditProperty "goal",
            exactString "pp" $ propertiesEditProperty "phone_number",
            exactString "pt" $ propertiesEditProperty "timewindow",
            exactString "pu" $ propertiesEditProperty "url",
            exactString "pw" $ propertiesEditProperty "waiting_threshold",
            -- Clocking
            exactString "ci" forestClockOutEverywhereInAllFilesAndClockInHere,
            exactString "co" forestClockOutEverywhereInAllFiles,
            -- Convenience
            exactString " nw" convDoneAndWaitForResponse,
            exactString " rp" convRepinged,
            exactString " rs" convResponded,
            exactString " rw" convRespondedButStillWaiting,
            exactString " pr" convUrlWaitingForReview,
            exactString " ce" convNewEntryAndClockIn,
            exactString " fa" convArchiveFile,
            exactString " ou" convOpenUrl,
            exactString " cc" convCopyContentsToClipboard,
            -- Collapsing
            exactChar '\t' forestToggleCollapse,
            exactKeyPress (KeyPress (KChar '\t') [MMeta]) forestToggleCollapseRecursively,
            exactKey KBackTab forestToggleCollapseEntireEntry,
            exactString "ze" forestToggleCollapseEntireEntry,
            exactString "zc" forestToggleCollapseEntryContents,
            exactString "zh" forestToggleCollapseEntryHistory,
            exactString "zl" forestToggleCollapseEntryLogbook,
            exactString "zt" forestToggleCollapseEntryTimestamps,
            exactString "zp" forestToggleCollapseEntryProperties,
            -- Entering contents
            exactKey KEnter entrySelectContentsAtEnd,
            exactKeyPress (KeyPress KEnter [MMeta]) entrySelectContentsAtStart,
            exactKeyPress (KeyPress (KChar 'o') [MMeta]) entrySelectContentsAtStart,
            exactString "cv" contentsUseVim,
            exactString "ce" contentsUseEmacs,
            -- Entering tags
            exactString "gi" entrySelectTagsFromStart,
            exactString "ga" entrySelectTagsFromBack
          ],
      fileKeyMapHeaderMatchers =
        listMatchers
          [ exactKey KEnter entrySelectWhole,
            anyChar headerInsert,
            exactKey KBS headerRemove,
            exactKey KDel headerDelete,
            exactKey KLeft headerMoveLeft,
            exactKey KRight headerMoveRight,
            exactKey KHome headerMoveToStart,
            modifiedChar 'a' [MCtrl] headerMoveToStart,
            exactKey KEnd headerMoveToEnd,
            modifiedChar 'e' [MCtrl] headerMoveToEnd,
            modifiedChar 'b' [MMeta] headerMoveToBeginningOfWord,
            modifiedChar 'e' [MMeta] headerMoveToEndOfWord,
            modifiedChar 'n' [MMeta] headerMoveToNextWord,
            modifiedChar 'p' [MMeta] headerMoveToPrevWord,
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
            modifiedChar 'b' [MMeta] contentsMoveToBeginningOfWord,
            modifiedChar 'e' [MMeta] contentsMoveToEndOfWord,
            modifiedChar 'n' [MMeta] contentsMoveToNextWord,
            modifiedChar 'p' [MMeta] contentsMoveToPrevWord,
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

defaultBrowserKeyMap :: BrowserKeyMap
defaultBrowserKeyMap =
  BrowserKeyMap
    { browserKeyMapExistentMatchers =
        listMatchers
          [ exactChar '\t' browserToggleCollapse,
            exactKey KEnter browserEnter,
            exactKey KBackTab browserToggleCollapseRecursively,
            exactChar 'n' browserStartNew,
            exactChar 'N' browserStartNewBelowAtEnd,
            exactKeyPress (KeyPress (KChar 'n') [MMeta]) browserStartNewBelowAtStart,
            exactString "ded" browserRemoveEmptyDir,
            exactChar 'a' browserArchive,
            exactChar '/' browserSelectFilter
          ],
      browserKeyMapInProgressMatchers =
        listMatchers
          [ anyChar browserInsertChar,
            exactKey KBS browserRemoveChar,
            exactKey KDel browserDeleteChar,
            exactKey KLeft browserSelectPrevChar,
            exactKey KRight browserSelectNextChar,
            exactKey KEnter browserCompleteFile,
            exactKeyPress (KeyPress KEnter [MMeta]) browserCompleteDir,
            exactKey KEsc browserStopNew
          ],
      browserKeyMapEmptyMatchers =
        listMatchers
          [ exactChar 'e' browserStartNew,
            exactChar 'n' browserStartNew
          ],
      browserKeyMapFilterMatchers =
        listMatchers
          [ anyChar browserFilterInsertChar,
            exactKey KEnter browserEnter,
            exactKey KEsc browserUnselectFilter,
            exactKey KBS browserFilterRemoveChar,
            exactKey KDel browserFilterDeleteChar
          ],
      browserKeyMapAnyMatchers =
        listMatchers
          [ exactKey KDown browserSelectNext,
            exactChar 'j' browserSelectNext,
            exactKey KUp browserSelectPrev,
            exactChar 'k' browserSelectPrev,
            exactString "gg" browserSelectFirst,
            exactString "G" browserSelectLast,
            exactChar 'u' browserUndo,
            modifiedChar 'u' [MMeta] browserRedo
          ]
    }

defaultReportsKeyMap :: ReportsKeyMap
defaultReportsKeyMap =
  ReportsKeyMap
    { reportsKeymapNextActionReportKeyMap = defaultNextActionReportKeyMap,
      reportsKeymapWaitingReportKeyMap = defaultWaitingReportKeyMap,
      reportsKeymapTimestampsReportKeyMap = defaultTimestampsReportKeyMap,
      reportsKeymapStuckReportKeyMap = defaultStuckReportKeyMap,
      reportsKeymapWorkReportKeyMap = defaultWorkReportKeyMap,
      reportsKeymapAnyMatchers =
        listMatchers
          [ exactKey KEsc exitReport,
            exactChar 'q' exitReport
          ]
    }

defaultNextActionReportKeyMap :: NextActionReportKeyMap
defaultNextActionReportKeyMap =
  NextActionReportKeyMap
    { nextActionReportMatchers =
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
            exactChar '/' selectNextActionFilter
          ],
      nextActionReportSearchMatchers =
        listMatchers
          [ anyChar insertNextActionFilter,
            exactKey KEnter selectNextActionReport,
            exactKey KEsc selectNextActionReport,
            exactKey KBS removeNextActionFilter,
            exactKey KDel deleteNextActionFilter
          ],
      nextActionReportAnyMatchers = []
    }

defaultWaitingReportKeyMap :: WaitingReportKeyMap
defaultWaitingReportKeyMap =
  WaitingReportKeyMap
    { waitingReportMatchers =
        listMatchers
          [ exactKey KUp prevWaiting,
            exactChar 'k' prevWaiting,
            exactKey KDown nextWaiting,
            exactChar 'j' nextWaiting,
            exactKey KHome firstWaiting,
            exactString "gg" firstWaiting,
            exactKey KEnd lastWaiting,
            exactChar 'G' lastWaiting,
            exactKey KEnter enterWaitingFile,
            exactChar '/' selectWaitingFilter
          ],
      waitingReportSearchMatchers =
        listMatchers
          [ anyChar insertWaitingFilter,
            exactKey KEnter selectWaitingReport,
            exactKey KEsc selectWaitingReport,
            exactKey KBS removeWaitingFilter,
            exactKey KDel deleteWaitingFilter
          ],
      waitingReportAnyMatchers = listMatchers []
    }

defaultTimestampsReportKeyMap :: TimestampsReportKeyMap
defaultTimestampsReportKeyMap =
  TimestampsReportKeyMap
    { timestampsReportMatchers =
        listMatchers
          [ exactKey KUp prevTimestamps,
            exactChar 'k' prevTimestamps,
            exactKey KDown nextTimestamps,
            exactChar 'j' nextTimestamps,
            exactKey KHome firstTimestamps,
            exactString "gg" firstTimestamps,
            exactKey KEnd lastTimestamps,
            exactChar 'G' lastTimestamps,
            exactKey KEnter enterTimestampsFile,
            exactChar '/' selectTimestampsFilter
          ],
      timestampsReportSearchMatchers =
        listMatchers
          [ anyChar insertTimestampsFilter,
            exactKey KEnter selectTimestampsReport,
            exactKey KEsc selectTimestampsReport,
            exactKey KBS removeTimestampsFilter,
            exactKey KDel deleteTimestampsFilter
          ],
      timestampsReportAnyMatchers = listMatchers []
    }

defaultStuckReportKeyMap :: StuckReportKeyMap
defaultStuckReportKeyMap =
  StuckReportKeyMap
    { stuckReportMatchers =
        listMatchers
          [ exactKey KUp prevStuck,
            exactChar 'k' prevStuck,
            exactKey KDown nextStuck,
            exactChar 'j' nextStuck,
            exactKey KHome firstStuck,
            exactString "gg" firstStuck,
            exactKey KEnd lastStuck,
            exactChar 'G' lastStuck,
            exactKey KEnter enterStuckFile
          ],
      stuckReportAnyMatchers = listMatchers []
    }

defaultWorkReportKeyMap :: WorkReportKeyMap
defaultWorkReportKeyMap =
  WorkReportKeyMap
    { workReportMatchers =
        listMatchers
          [ exactKey KUp prevWork,
            exactChar 'k' prevWork,
            exactKey KDown nextWork,
            exactChar 'j' nextWork,
            exactKey KHome firstWork,
            exactString "gg" firstWork,
            exactKey KEnd lastWork,
            exactChar 'G' lastWork,
            exactKey KEnter enterWorkFile,
            exactChar '/' selectWorkFilter
          ],
      workReportSearchMatchers =
        listMatchers
          [ anyChar insertWorkFilter,
            exactKey KEnter selectWorkReport,
            exactKey KEsc selectWorkReport,
            exactKey KBS removeWorkFilter,
            exactKey KDel deleteWorkFilter
          ],
      workReportAnyMatchers = listMatchers []
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
            exactChar '/' helpSelectSearch
          ],
      helpKeyMapSearchMatchers =
        listMatchers
          [ anyChar helpInsert,
            exactKey KBS helpRemove,
            exactKey KDel helpDelete,
            exactKey KEnter helpSelectHelp,
            exactKey KEsc helpSelectHelp
          ],
      helpKeyMapAnyMatchers =
        listMatchers
          [ exactKey KEsc exitHelp,
            exactChar 'q' exitHelp,
            exactChar '?' exitHelp
          ]
    }

defaultAnyKeyMap :: KeyMappings
defaultAnyKeyMap =
  listMatchers
    [ exactChar 'q' quit,
      -- Help
      exactChar '?' selectHelp,
      exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp,
      -- Browser
      exactString "bp" selectBrowserProjects,
      exactString "bw" selectBrowserWorkflow,
      exactString "ba" selectBrowserArchive,
      exactString "br" selectBrowserReview,
      exactString "bc" selectBrowserClient,
      exactString "bs" selectBrowserSide,
      -- Reports
      exactString "rn" reportNextActions,
      exactString "rw" reportWaiting,
      exactString "ra" reportTimestamps,
      exactString "rs" reportStuck,
      exactString "rr" reportWork
    ]
