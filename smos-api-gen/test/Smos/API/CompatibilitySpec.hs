{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.API.CompatibilitySpec
  ( spec,
  )
where

import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.DirForest as DF
import qualified Data.Map as M
import Data.Mergeful as Mergeful
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Path
import Smos.API
import Smos.API.Gen ()
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Next
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "GetAPIVersion" $
    outputGoldenTest "api-version" apiVersion
  describe "PostRegister" $
    describe "can still parse old Register inputs" $
      inputGoldenTest "register" $
        Register
          { registerUsername = Username "user",
            registerPassword = "password"
          }
  describe "PostLogin" $
    describe "can still parse old Login inputs" $
      inputGoldenTest "login" $
        Login
          { loginUsername = Username "user",
            loginPassword = "password"
          }
  describe "PostSync" $
    describe "can still parse old Login inputs" $ do
      inputGoldenTest "sync" $
        Smos.API.SyncRequest
          { syncRequestItems =
              Mergeful.SyncRequest
                { syncRequestNewItems =
                    M.fromList
                      [ ([relfile|example.smos|], SyncFile {syncFileContents = "contents"})
                      ],
                  syncRequestKnownItems =
                    M.fromList
                      [ ([relfile|example2.smos|], initialServerTime)
                      ],
                  syncRequestKnownButChangedItems =
                    M.fromList
                      [ ( [relfile|example3.smos|],
                          Mergeful.Timed (SyncFile {syncFileContents = "contents3"}) initialServerTime
                        )
                      ],
                  syncRequestDeletedItems =
                    M.fromList
                      [ ([relfile|example4.smos|], initialServerTime)
                      ]
                }
          }
      outputGoldenTest "sync" $
        Mergeful.SyncResponse
          { syncResponseClientAdded =
              M.fromList
                [ ( [relfile|example5.smos|] :: Path Rel File,
                    ClientAddition
                      { clientAdditionId = [relfile|example5.smos|] :: Path Rel File,
                        clientAdditionServerTime = initialServerTime
                      }
                  )
                ],
            syncResponseClientChanged =
              M.fromList
                [ ([relfile|example6.smos|], initialServerTime)
                ],
            syncResponseClientDeleted = S.singleton [relfile|example7.smos|],
            syncResponseServerAdded =
              M.fromList
                [ ( [relfile|example8.smos|],
                    Mergeful.Timed (SyncFile {syncFileContents = "contents8"}) initialServerTime
                  )
                ],
            syncResponseServerChanged =
              M.fromList
                [ ( [relfile|example9.smos|],
                    Mergeful.Timed (SyncFile {syncFileContents = "contents9"}) initialServerTime
                  )
                ],
            syncResponseServerDeleted = S.singleton [relfile|example10.smos|],
            syncResponseConflicts =
              M.fromList
                [ ( [relfile|example11.smos|],
                    Mergeful.Timed (SyncFile {syncFileContents = "contents11"}) initialServerTime
                  )
                ],
            syncResponseConflictsClientDeleted =
              M.fromList
                [ ( [relfile|example12.smos|],
                    Mergeful.Timed (SyncFile {syncFileContents = "contents12"}) initialServerTime
                  )
                ],
            syncResponseConflictsServerDeleted = S.singleton [relfile|example13.smos|]
          }
  let smosFileExample = SmosFile [Node (newEntry "hello") []]
  describe "GetListSmosFiles" $ outputGoldenTest "files" $ DF.singletonFile [relfile|example14.smos|] smosFileExample
  describe "GetSmosFile" $ outputGoldenTest "file-get" smosFileExample
  describe "PutSmosFile" $ inputGoldenTest "file-put" smosFileExample
  describe "GetNextActionReport" $
    outputGoldenTest "report/next" $
      NextActionReport
        { nextActionReportEntries =
            [ NextActionEntry
                { nextActionEntryTodoState = Nothing,
                  nextActionEntryHeader = "hello",
                  nextActionEntryFilePath = [relfile|example14.smos|]
                }
            ]
        }
  describe "GetAgendaReport" $
    outputGoldenTest "report/agenda" $
      AgendaReport
        { agendaReportPast = [],
          agendaReportPresent =
            AgendaTodayReport
              { agendaTodayReportEntries =
                  []
              },
          agendaReportFuture = []
        }
  describe "GetListBackups" $
    outputGoldenTest
      "backups"
      [ BackupInfo
          { backupInfoUUID = Typed.UUID (UUID.fromWords 0 1 2 3),
            backupInfoTime = UTCTime (fromGregorian 2021 04 16) 8765,
            backupInfoSize = 543
          }
      ]
  describe "GetBackup" $ do
    inputGoldenTest "backup-get" $ Typed.UUID (UUID.fromWords 1 2 3 4)
  describe "PostBackup" $ do
    outputGoldenTest "backup-post" $ Typed.UUID (UUID.fromWords 2 3 4 5)

outputGoldenTest :: (Show a, Eq a, ToJSON a, FromJSON a) => FilePath -> a -> Spec
outputGoldenTest fp val =
  it "outputs the version the same way as before" $
    pureGoldenJSONValueFile ("test_resources/" ++ fp ++ "/output.json") val

inputGoldenTest :: forall a. (Validity a, Show a, Eq a, FromJSON a, ToJSON a) => FilePath -> a -> Spec
inputGoldenTest fp current = do
  it "output" $
    pureGoldenJSONValueFile ("test_resources/" ++ fp ++ "/input.json") current
  scenarioDir ("test_resources/" ++ fp ++ "/old-input") $ \p ->
    it "can still parse the old input" $ do
      bs <- SB.readFile p
      case JSON.eitherDecode (LB.fromStrict bs) of
        Left err -> expectationFailure err
        Right r -> shouldBeValid (r :: a)
