{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.Regression.LaundrySpec
  ( spec,
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time hiding (DayOfWeek (Monday, Sunday))
import Smos.Calendar.Import
import Smos.Data
import Test.Hspec
import Text.ICalendar.Types as ICal

spec :: Spec
spec = do
  describe "processEvent"
    $ it "works for only the one recurrence of next week"
    $ let st =
            StartDateTime $
              ZonedDateTime
                { dateTimeFloating = LocalTime (fromGregorian 2020 06 08) (TimeOfDay 10 00 00),
                  dateTimeZone = "Europe/Zurich"
                }
       in case runReader (nextStartTime recur st) iEnv of
            Nothing -> expectationFailure "Should have gotten a next start time"
            Just st' ->
              st'
                `shouldBe` StartDateTime
                  ( ZonedDateTime
                      { dateTimeFloating = LocalTime (fromGregorian 2020 06 15) (TimeOfDay 10 00 00),
                        dateTimeZone = "Europe/Zurich"
                      }
                  )
  describe "processEvent"
    $ it "works for my regression event about laundry"
    $ case runReader (processEvent regressionLaundryVEvent) iEnv of
      Nothing -> expectationFailure "Should have gotten entries"
      Just (Node _ f) -> case f of
        [] -> expectationFailure "Should have gotten recurrences"
        (Node actual _ : _) -> actual `shouldBe` regressionLaundryEntry

regressionLaundryEntry :: Entry
regressionLaundryEntry =
  (newEntry (Header "Laundry Pick-up/drop-off"))
    { entryTimestamps =
        M.fromList
          [ ( TimestampName {timestampNameText = "BEGIN"},
              TimestampLocalTime
                ( LocalTime
                    (fromGregorian 2020 06 08)
                    (TimeOfDay 10 00 00)
                )
            ),
            ( TimestampName {timestampNameText = "END"},
              TimestampLocalTime
                ( LocalTime
                    (fromGregorian 2020 06 08)
                    (TimeOfDay 12 00 00)
                )
            )
          ]
    }

iEnv :: ImportEnv
iEnv =
  ImportEnv
    { importEnvStartDay = fromGregorian 2020 06 04,
      importEnvRecurrentLimit = fromGregorian 2020 07 07,
      importEnvTimeZone = TimeZone {timeZoneMinutes = 120, timeZoneSummerOnly = True, timeZoneName = "CEST"},
      importEnvDebug = False,
      importEnvNamedTimeZones =
        M.fromList
          [ ( "Europe/Zurich",
              VTimeZone
                { vtzId =
                    TZID
                      { tzidValue = "Europe/Zurich",
                        tzidGlobal = False,
                        tzidOther = OtherParams (S.fromList [])
                      },
                  vtzLastMod = Nothing,
                  vtzUrl = Nothing,
                  vtzStandardC =
                    S.fromList
                      [ TZProp
                          { tzpDTStart =
                              DTStartDateTime
                                { dtStartDateTimeValue =
                                    FloatingDateTime
                                      { dateTimeFloating = LocalTime (fromGregorian 1970 10 25) (TimeOfDay 03 00 00)
                                      },
                                  dtStartOther = OtherParams (S.fromList [])
                                },
                            tzpTZOffsetTo =
                              UTCOffset
                                { utcOffsetValue = 3600,
                                  utcOffsetOther = OtherParams (S.fromList [])
                                },
                            tzpTZOffsetFrom =
                              UTCOffset
                                { utcOffsetValue = 7200,
                                  utcOffsetOther = OtherParams (S.fromList [])
                                },
                            tzpRRule =
                              S.fromList
                                [ RRule
                                    { rRuleValue =
                                        Recur
                                          { recurFreq = Yearly,
                                            recurUntilCount = Nothing,
                                            recurInterval = 1,
                                            recurBySecond = [],
                                            recurByMinute = [],
                                            recurByHour = [],
                                            recurByDay = [Left (-1, Sunday)],
                                            recurByMonthDay = [],
                                            recurByYearDay = [],
                                            recurByWeekNo = [],
                                            recurByMonth = [10],
                                            recurBySetPos = [],
                                            recurWkSt = Monday
                                          },
                                      rRuleOther = OtherParams (S.fromList [])
                                    }
                                ],
                            tzpComment = S.fromList [],
                            tzpRDate = S.fromList [],
                            tzpTZName =
                              S.fromList
                                [ TZName
                                    { tzNameValue = "CET",
                                      tzNameLanguage = Nothing,
                                      tzNameOther = OtherParams (S.fromList [])
                                    }
                                ],
                            tzpOther = S.fromList []
                          }
                      ],
                  vtzDaylightC =
                    S.fromList
                      [ TZProp
                          { tzpDTStart =
                              DTStartDateTime
                                { dtStartDateTimeValue =
                                    FloatingDateTime
                                      { dateTimeFloating = LocalTime (fromGregorian 1970 03 29) (TimeOfDay 02 00 00)
                                      },
                                  dtStartOther = OtherParams (S.fromList [])
                                },
                            tzpTZOffsetTo =
                              UTCOffset
                                { utcOffsetValue = 7200,
                                  utcOffsetOther = OtherParams (S.fromList [])
                                },
                            tzpTZOffsetFrom =
                              UTCOffset
                                { utcOffsetValue = 3600,
                                  utcOffsetOther = OtherParams (S.fromList [])
                                },
                            tzpRRule =
                              S.fromList
                                [ RRule
                                    { rRuleValue =
                                        Recur
                                          { recurFreq = Yearly,
                                            recurUntilCount = Nothing,
                                            recurInterval = 1,
                                            recurBySecond = [],
                                            recurByMinute = [],
                                            recurByHour = [],
                                            recurByDay = [Left (-1, Sunday)],
                                            recurByMonthDay = [],
                                            recurByYearDay = [],
                                            recurByWeekNo = [],
                                            recurByMonth = [3],
                                            recurBySetPos = [],
                                            recurWkSt = Monday
                                          },
                                      rRuleOther = OtherParams (S.fromList [])
                                    }
                                ],
                            tzpComment = S.fromList [],
                            tzpRDate = S.fromList [],
                            tzpTZName =
                              S.fromList
                                [ TZName
                                    { tzNameValue = "CEST",
                                      tzNameLanguage = Nothing,
                                      tzNameOther = OtherParams (S.fromList [])
                                    }
                                ],
                            tzpOther = S.fromList []
                          }
                      ],
                  vtzOther =
                    S.fromList
                      [ OtherProperty
                          { otherName = "X-LIC-LOCATION",
                            otherValue = "Europe/Zurich",
                            otherParams = OtherParams (S.fromList [])
                          }
                      ]
                }
            )
          ]
    }

regressionLaundryVEvent :: VEvent
regressionLaundryVEvent =
  VEvent
    { veDTStamp =
        DTStamp
          { dtStampValue = UTCTime (fromGregorian 2020 06 07) (timeOfDayToTime (TimeOfDay 16 35 39)),
            dtStampOther = OtherParams (S.fromList [])
          },
      veUID =
        UID
          { uidValue = "7p0tnem92s2u2jf0ac3rls8ei3@google.com",
            uidOther = OtherParams (S.fromList [])
          },
      veClass =
        Class
          { classValue = Public,
            classOther = OtherParams (S.fromList [])
          },
      veDTStart =
        Just
          DTStartDateTime
            { dtStartDateTimeValue =
                ZonedDateTime
                  { dateTimeFloating = LocalTime (fromGregorian 2018 03 12) (TimeOfDay 10 00 00),
                    dateTimeZone = "Europe/Zurich"
                  },
              dtStartOther = OtherParams (S.fromList [])
            },
      veCreated =
        Just
          Created
            { createdValue = UTCTime (fromGregorian 2018 03 07) (timeOfDayToTime (TimeOfDay 18 00 33)),
              createdOther = OtherParams (S.fromList [])
            },
      veDescription =
        Just
          Description
            { descriptionValue = "",
              descriptionAltRep = Nothing,
              descriptionLanguage = Nothing,
              descriptionOther = OtherParams (S.fromList [])
            },
      veGeo = Nothing,
      veLastMod =
        Just
          LastModified
            { lastModifiedValue = UTCTime (fromGregorian 2018 03 07) (timeOfDayToTime (TimeOfDay 18 00 52)),
              lastModifiedOther = OtherParams (S.fromList [])
            },
      veLocation =
        Just
          Location
            { locationValue = "",
              locationAltRep = Nothing,
              locationLanguage = Nothing,
              locationOther = OtherParams (S.fromList [])
            },
      veOrganizer = Nothing,
      vePriority =
        Priority
          { priorityValue = 0,
            priorityOther = OtherParams (S.fromList [])
          },
      veSeq =
        Sequence
          { sequenceValue = 1,
            sequenceOther = OtherParams (S.fromList [])
          },
      veStatus =
        Just
          ConfirmedEvent {eventStatusOther = OtherParams (S.fromList [])},
      veSummary =
        Just
          Summary
            { summaryValue = "Laundry Pick-up/drop-off",
              summaryAltRep = Nothing,
              summaryLanguage = Nothing,
              summaryOther = OtherParams (S.fromList [])
            },
      veTransp =
        Opaque {timeTransparencyOther = OtherParams (S.fromList [])},
      veUrl = Nothing,
      veRecurId = Nothing,
      veRRule =
        S.fromList
          [ RRule
              { rRuleValue = recur,
                rRuleOther = OtherParams (S.fromList [])
              }
          ],
      veDTEndDuration =
        Just
          ( Left
              DTEndDateTime
                { dtEndDateTimeValue =
                    ZonedDateTime
                      { dateTimeFloating = LocalTime (fromGregorian 2018 03 12) (TimeOfDay 12 00 00),
                        dateTimeZone = "Europe/Zurich"
                      },
                  dtEndOther = OtherParams (S.fromList [])
                }
          ),
      veAttach = S.fromList [],
      veAttendee = S.fromList [],
      veCategories = S.fromList [],
      veComment = S.fromList [],
      veContact = S.fromList [],
      veExDate = S.fromList [],
      veRStatus = S.fromList [],
      veRelated = S.fromList [],
      veResources = S.fromList [],
      veRDate = S.fromList [],
      veAlarms = S.fromList [],
      veOther = S.fromList []
    }

recur :: Recur
recur =
  Recur
    { recurFreq = Weekly,
      recurUntilCount = Nothing,
      recurInterval = 1,
      recurBySecond = [],
      recurByMinute = [],
      recurByHour = [],
      recurByDay = [Right Monday],
      recurByMonthDay = [],
      recurByYearDay = [],
      recurByWeekNo = [],
      recurByMonth = [],
      recurBySetPos = [],
      recurWkSt = Monday
    }
