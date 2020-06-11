{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.Regression.GermanSpec
  ( spec,
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import
import Smos.Data
import Test.Hspec
import Text.ICalendar.Types as ICal

spec :: Spec
spec = do
  it "works for my regression event about learning german" $
    let iEnv =
          ImportEnv
            { importEnvStartDay = fromGregorian 2020 06 04,
              importEnvRecurrentLimit = fromGregorian 2020 07 07,
              importEnvTimeZone = TimeZone {timeZoneMinutes = 120, timeZoneSummerOnly = True, timeZoneName = "CEST"},
              importEnvNamedTimeZones =
                M.fromList
                  [ ( "America/Los_Angeles",
                      VTimeZone
                        { vtzId =
                            TZID
                              { tzidValue = "America/Los_Angeles",
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
                                              { dateTimeFloating = LocalTime (fromGregorian 1970 11 01) (TimeOfDay 02 00 00)
                                              },
                                          dtStartOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetTo =
                                      UTCOffset
                                        { utcOffsetValue = -28800,
                                          utcOffsetOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetFrom =
                                      UTCOffset
                                        { utcOffsetValue = -25200,
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
                                                    recurByDay = [Left (1, Sunday)],
                                                    recurByMonthDay = [],
                                                    recurByYearDay = [],
                                                    recurByWeekNo = [],
                                                    recurByMonth = [11],
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
                                            { tzNameValue = "PST",
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
                                              { dateTimeFloating = LocalTime (fromGregorian 1970 03 08) (TimeOfDay 02 00 00)
                                              },
                                          dtStartOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetTo =
                                      UTCOffset
                                        { utcOffsetValue = -25200,
                                          utcOffsetOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetFrom =
                                      UTCOffset
                                        { utcOffsetValue = -28800,
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
                                                    recurByDay = [Left (2, Sunday)],
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
                                            { tzNameValue = "PDT",
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
                                    otherValue = "America/Los_Angeles",
                                    otherParams = OtherParams (S.fromList [])
                                  }
                              ]
                        }
                    ),
                    ( "Australia/Sydney",
                      VTimeZone
                        { vtzId =
                            TZID
                              { tzidValue = "Australia/Sydney",
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
                                              { dateTimeFloating = LocalTime (fromGregorian 1970 04 05) (TimeOfDay 03 00 00)
                                              },
                                          dtStartOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetTo =
                                      UTCOffset
                                        { utcOffsetValue = 36000,
                                          utcOffsetOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetFrom =
                                      UTCOffset
                                        { utcOffsetValue = 39600,
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
                                                    recurByDay = [Left (1, Sunday)],
                                                    recurByMonthDay = [],
                                                    recurByYearDay = [],
                                                    recurByWeekNo = [],
                                                    recurByMonth = [4],
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
                                            { tzNameValue = "AEST",
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
                                              { dateTimeFloating = LocalTime (fromGregorian 1970 10 04) (TimeOfDay 02 00 00)
                                              },
                                          dtStartOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetTo =
                                      UTCOffset
                                        { utcOffsetValue = 39600,
                                          utcOffsetOther = OtherParams (S.fromList [])
                                        },
                                    tzpTZOffsetFrom =
                                      UTCOffset
                                        { utcOffsetValue = 36000,
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
                                                    recurByDay = [Left (1, Sunday)],
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
                                            { tzNameValue = "AEDT",
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
                                    otherValue = "Australia/Sydney",
                                    otherParams = OtherParams (S.fromList [])
                                  }
                              ]
                        }
                    ),
                    ( "Europe/Amsterdam",
                      VTimeZone
                        { vtzId =
                            TZID
                              { tzidValue = "Europe/Amsterdam",
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
                                    otherValue = "Europe/Amsterdam",
                                    otherParams = OtherParams (S.fromList [])
                                  }
                              ]
                        }
                    ),
                    ( "Europe/Brussels",
                      VTimeZone
                        { vtzId =
                            TZID
                              { tzidValue = "Europe/Brussels",
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
                                    otherValue = "Europe/Brussels",
                                    otherParams = OtherParams (S.fromList [])
                                  }
                              ]
                        }
                    ),
                    ( "Europe/Zurich",
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
                  ],
              importEnvDebug = False
            }
        iFunc = processEvent regressionLearnGermanVEvent
     in case runReader iFunc iEnv of
          Nothing -> expectationFailure "Should have gotten entries"
          Just (Node actual _) -> actual `shouldBe` regressionLearningGermanEntry

regressionLearningGermanEntry :: Entry
regressionLearningGermanEntry =
  (newEntry (Header "Learn German for 30min"))
    { entryTimestamps =
        M.fromList
          [ ( TimestampName {timestampNameText = "BEGIN"},
              TimestampLocalTime
                ( LocalTime
                    (fromGregorian 2020 06 06)
                    (TimeOfDay 14 00 00)
                )
            ),
            ( TimestampName {timestampNameText = "END"},
              TimestampLocalTime
                ( LocalTime
                    (fromGregorian 2020 06 06)
                    (TimeOfDay 14 30 00)
                )
            )
          ]
    }

regressionLearnGermanVEvent :: VEvent
regressionLearnGermanVEvent =
  VEvent
    { veDTStamp =
        DTStamp
          { dtStampValue = UTCTime (fromGregorian 2020 06 07) (timeOfDayToTime (TimeOfDay 16 35 39)),
            dtStampOther = OtherParams (S.fromList [])
          },
      veUID =
        UID
          { uidValue = "2cs904ah87sfpn7c113er75727@google.com",
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
                  { dateTimeFloating = LocalTime (fromGregorian 2020 06 06) (TimeOfDay 14 00 00),
                    dateTimeZone = "Europe/Zurich"
                  },
              dtStartOther = OtherParams (S.fromList [])
            },
      veCreated =
        Just
          Created
            { createdValue = UTCTime (fromGregorian 2020 06 04) (timeOfDayToTime (TimeOfDay 07 04 26)),
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
            { lastModifiedValue = UTCTime (fromGregorian 2020 06 06) (timeOfDayToTime (TimeOfDay 09 44 16)),
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
          { sequenceValue = 2,
            sequenceOther = OtherParams (S.fromList [])
          },
      veStatus =
        Just
          ConfirmedEvent {eventStatusOther = OtherParams (S.fromList [])},
      veSummary =
        Just
          Summary
            { summaryValue = "Learn German for 30min",
              summaryAltRep = Nothing,
              summaryLanguage = Nothing,
              summaryOther = OtherParams (S.fromList [])
            },
      veTransp =
        Opaque {timeTransparencyOther = OtherParams (S.fromList [])},
      veUrl = Nothing,
      veRecurId =
        Just
          RecurrenceIdDateTime
            { recurrenceIdDateTime =
                ZonedDateTime
                  { dateTimeFloating = LocalTime (fromGregorian 2020 06 06) (TimeOfDay 11 30 00),
                    dateTimeZone = "Europe/Zurich"
                  },
              recurrenceIdRange = Nothing,
              recurrenceIdOther = OtherParams (S.fromList [])
            },
      veRRule = S.fromList [],
      veDTEndDuration =
        Just
          ( Left
              DTEndDateTime
                { dtEndDateTimeValue =
                    ZonedDateTime
                      { dateTimeFloating = LocalTime (fromGregorian 2020 06 06) (TimeOfDay 14 30 00),
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
