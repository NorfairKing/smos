{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.Smos
  ( getSmosR,
    getSmosFileR,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml.Builder as Yaml
import Smos.Data
import Smos.Docs.Site.Handler.Import hiding (Header)
import Smos.OptParse as TUI

getSmosR :: Handler Html
getSmosR = makeSettingsPage @TUI.Instructions "smos"

getSmosFileR :: Handler Html
getSmosFileR = do
  DocPage {..} <- lookupPage "smos-file"
  defaultLayout $ do
    setTitle "Smos File"
    setDescriptionIdemp "Documentation for the smos file format"
    $(widgetFile "smos-file")

exampleFile :: Versioned SmosFile
exampleFile =
  Versioned
    currentDataVersion
    $ SmosFile
      [ Node
          ( (newEntry "Use Smos")
              { entryTags = S.singleton "online",
                entryTimestamps =
                  M.fromList
                    [ ("DEADLINE", TimestampDay (fromGregorian 2018 10 30)),
                      ("SCHEDULED", TimestampDay (fromGregorian 2018 10 21))
                    ],
                entryStateHistory =
                  StateHistory
                    [ StateHistoryEntry
                        { stateHistoryEntryNewState = Just "STARTED",
                          stateHistoryEntryTimestamp = UTCTime (fromGregorian 2020 05 04) 12345
                        }
                    ]
              }
          )
          [ Node
              ( (newEntry "Don't mess it up")
                  { entryStateHistory =
                      StateHistory
                        [ StateHistoryEntry
                            { stateHistoryEntryNewState = Just "DONE",
                              stateHistoryEntryTimestamp = UTCTime (fromGregorian 2020 05 04) 12348
                            },
                          StateHistoryEntry
                            { stateHistoryEntryNewState = Just "NEXT",
                              stateHistoryEntryTimestamp = UTCTime (fromGregorian 2020 05 04) 12347
                            }
                        ]
                  }
              )
              [],
            Node
              ( (newEntry "Be smart about it")
                  { entryTags = S.singleton "work",
                    entryStateHistory =
                      StateHistory
                        [ StateHistoryEntry
                            { stateHistoryEntryNewState = Just "TODO",
                              stateHistoryEntryTimestamp = UTCTime (fromGregorian 2020 05 04) 12350
                            }
                        ]
                  }
              )
              []
          ]
      ]

headerDesc :: Text
headerDesc = yamlDesc @Contents

contentsDesc :: Text
contentsDesc = yamlDesc @Contents

timestampsDesc :: Text
timestampsDesc = yamlDesc @(Map TimestampName Timestamp)

propertiesDesc :: Text
propertiesDesc = yamlDesc @(Map PropertyName PropertyValue)

stateHistoryDesc :: Text
stateHistoryDesc = yamlDesc @StateHistory

tagsDesc :: Text
tagsDesc = yamlDesc @(Set Tag)

exampleStateHistory :: StateHistory
exampleStateHistory =
  let t2 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 50
          }
      t5 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 5500
          }
   in StateHistory
        { unStateHistory =
            [ StateHistoryEntry
                { stateHistoryEntryNewState = Just "NEXT",
                  stateHistoryEntryTimestamp = t5
                },
              StateHistoryEntry
                { stateHistoryEntryNewState = Just "TODO",
                  stateHistoryEntryTimestamp = t2
                }
            ]
        }

exampleTags :: Set Tag
exampleTags = S.fromList ["code", "online"]

exampleLogbook :: Logbook
exampleLogbook =
  let t3 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 500
          }
      t4 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 550
          }
      t5 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 5500
          }
   in LogOpen
        t5
        [ LogbookEntry
            { logbookEntryStart = t3,
              logbookEntryEnd = t4
            }
        ]

logbookDesc :: Text
logbookDesc = yamlDesc @Logbook

example :: (HasCodec a) => a -> Text
example = TE.decodeUtf8 . Yaml.toByteString . AutodocodecYaml

exampleEntry :: Entry
exampleEntry =
  let t2 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 50
          }
      t3 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 500
          }
      t4 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 550
          }
      t5 =
        UTCTime
          { utctDay = fromGregorian 2020 5 9,
            utctDayTime = 5500
          }
   in Entry
        { entryHeader = Header "Fix issue 25",
          entryContents = Just $ Contents "https://github.com/NorfairKing/smos/issues/25",
          entryTimestamps = M.singleton (TimestampName "SCHEDULED") (TimestampDay $ fromGregorian 2020 5 9),
          entryProperties = M.singleton (PropertyName "project") (PropertyValue "smos"),
          entryStateHistory =
            StateHistory
              { unStateHistory =
                  [ StateHistoryEntry
                      { stateHistoryEntryNewState = Just "NEXT",
                        stateHistoryEntryTimestamp = t5
                      },
                    StateHistoryEntry
                      { stateHistoryEntryNewState = Just "TODO",
                        stateHistoryEntryTimestamp = t2
                      }
                  ]
              },
          entryTags = S.fromList ["code", "online"],
          entryLogbook =
            LogOpen
              t5
              [ LogbookEntry
                  { logbookEntryStart = t3,
                    logbookEntryEnd = t4
                  }
              ]
        }

smosFileDesc :: Text
smosFileDesc = yamlDesc @SmosFile
