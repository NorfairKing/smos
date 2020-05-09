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

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
import Options.Applicative
import Options.Applicative.Help
import Smos.Data
import Smos.Docs.Site.Handler.Import hiding (Header)
import Smos.OptParse
import YamlParse.Applicative

getSmosR :: Handler Html
getSmosR = do
  DocPage {..} <- lookupPage "smos"
  let helpText = getHelpPageOf []
  defaultLayout $ do
    setTitle "Smos Documentation - smos"
    $(widgetFile "smos")

getSmosFileR :: Handler Html
getSmosFileR = do
  DocPage {..} <- lookupPage "smos-file"
  defaultLayout $ do
    setTitle "Smos Documentation - Smos File"
    $(widgetFile "smos-file")

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

yamlDesc :: forall a. YamlSchema a => Text
yamlDesc = prettySchema . explainParser $ yamlSchema @a

example :: ToJSON a => a -> Text
example = TE.decodeUtf8 . Yaml.encode

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
          entryContents = Just $ Contents "https://github.com/NorfairKing/smos/issues/53",
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

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
