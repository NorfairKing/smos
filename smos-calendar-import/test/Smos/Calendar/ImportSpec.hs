{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.ImportSpec
  ( spec,
  )
where

import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import
import Test.Hspec
import Text.ICalendar.Types as ICal

spec :: Spec
spec = do
  describe "resolveLocalTimeWithVTimeZone" $ do
    it "works for my regression event about learning german" $ do
      let lt = LocalTime (fromGregorian 2020 06 06) (TimeOfDay 14 00 00)
      let mtz = TimeZone {timeZoneMinutes = 120, timeZoneSummerOnly = True, timeZoneName = "CEST"}
      let vtz =
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
      let expected = LocalTime (fromGregorian 2020 06 06) (TimeOfDay 14 00 00)
      resolveLocalTimeWithVTimeZone lt vtz mtz `shouldBe` expected
    it "works for my regression event about learning german" $ do
      let lt = LocalTime (fromGregorian 2020 06 08) (TimeOfDay 10 00 00)
      let mtz = TimeZone {timeZoneMinutes = 120, timeZoneSummerOnly = True, timeZoneName = "CEST"}
      let vtz =
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
      let expected = LocalTime (fromGregorian 2020 06 08) (TimeOfDay 10 00 00)
      resolveLocalTimeWithVTimeZone lt vtz mtz `shouldBe` expected
