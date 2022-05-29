module Smos.Server.Looper.BackupGarbageCollectorSpec where

import Control.Monad
import Control.Monad.State.Strict
import Data.GenValidity.Persist ()
import Data.Int
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Database.Persist.Sql
import Smos.Server.DB
import Smos.Server.Gen ()
import Smos.Server.Looper.BackupGarbageCollector
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "decideBackupsToDelete" $ do
    it "only ever selects backups to delete, from the list" $
      forAllValid $ \now ->
        forAllValid $ \backups ->
          let backupsToDelete = decideBackupsToDelete now defaultPeriods backups
              ids = S.fromList $ map fst backups
           in shouldSatisfyNamed
                backupsToDelete
                ("`S.isSubsetOf` " <> show ids)
                (`S.isSubsetOf` ids)

    it "works with this single interval where a backup falls outside the interval" $
      let t y m d = UTCTime (fromGregorian y m d) 0
          periods =
            [ (nominalDay * 3, 3)
            ]
          backups =
            [ (toSqlKey 1, t 2022 01 01),
              (toSqlKey 2, t 2022 01 02),
              (toSqlKey 3, t 2022 01 03), -- <-- keeping this one
              (toSqlKey 4, t 2022 01 04), -- <-- keeping this one
              (toSqlKey 5, t 2022 01 05) -- <-- keeping this one
            ]
          expectedBackupsToDelete =
            S.fromList
              [ toSqlKey 1,
                toSqlKey 2
              ]
          actualBackupsToDelete = decideBackupsToDelete (t 2022 01 05) periods backups
       in actualBackupsToDelete `shouldBe` expectedBackupsToDelete

    it "works with this single example" $
      let t y m d = UTCTime (fromGregorian y m d) 0
          periods =
            [ (nominalDay * 3, 2)
            ]
          backups =
            [ (toSqlKey 1, t 2022 01 01), -- <-- keeping this one
              (toSqlKey 2, t 2022 01 02), -- <-- keeping this one
              (toSqlKey 3, t 2022 01 03)
            ]
          expectedBackupsToDelete =
            S.fromList
              [ toSqlKey 1
              ]
          actualBackupsToDelete = decideBackupsToDelete (t 2022 01 04) periods backups
       in actualBackupsToDelete `shouldBe` expectedBackupsToDelete

    it "works on this example" $
      let t y m d = UTCTime (fromGregorian y m d) 0
          periods =
            [ (nominalDay * 2, 1),
              (nominalDay * 7, 1),
              (nominalDay * 14, 1),
              (nominalDay * 30, 1)
            ]
          backups =
            [ (toSqlKey 1, t 2022 01 01),
              (toSqlKey 2, t 2022 01 02),
              (toSqlKey 3, t 2022 01 03), --  <- keep this one
              (toSqlKey 4, t 2022 01 04),
              (toSqlKey 5, t 2022 01 05),
              (toSqlKey 6, t 2022 01 06),
              (toSqlKey 7, t 2022 01 07),
              (toSqlKey 8, t 2022 01 08),
              (toSqlKey 9, t 2022 01 09),
              (toSqlKey 10, t 2022 01 10),
              (toSqlKey 11, t 2022 01 11),
              (toSqlKey 12, t 2022 01 12),
              (toSqlKey 13, t 2022 01 13),
              (toSqlKey 14, t 2022 01 14),
              (toSqlKey 15, t 2022 01 15),
              (toSqlKey 16, t 2022 01 16),
              (toSqlKey 17, t 2022 01 17),
              (toSqlKey 18, t 2022 01 18),
              (toSqlKey 19, t 2022 01 19), -- <- keep this one
              (toSqlKey 20, t 2022 01 20),
              (toSqlKey 21, t 2022 01 21),
              (toSqlKey 22, t 2022 01 22),
              (toSqlKey 23, t 2022 01 23),
              (toSqlKey 24, t 2022 01 24),
              (toSqlKey 25, t 2022 01 25),
              (toSqlKey 26, t 2022 01 26), -- <- keep this one
              (toSqlKey 27, t 2022 01 27),
              (toSqlKey 28, t 2022 01 28),
              (toSqlKey 29, t 2022 01 29),
              (toSqlKey 30, t 2022 01 30),
              (toSqlKey 31, t 2022 01 31) -- <-- keeping this one
            ]
          expectedBackupsToKeep =
            S.fromList
              [ toSqlKey 3,
                toSqlKey 19,
                toSqlKey 26,
                toSqlKey 31
              ]
          actualBackupsToDelete = decideBackupsToDelete (t 2022 02 01) periods backups
          actualBackupsToKeep = S.fromList (map fst backups) `S.difference` actualBackupsToDelete
       in actualBackupsToKeep `shouldBe` expectedBackupsToKeep

    iterationSpec

data S = S
  { sDateTime :: !UTCTime,
    sNextBackupId :: !BackupId,
    sBackups :: !(Set (BackupId, UTCTime))
  }
  deriving (Show)

beginState :: Day -> S
beginState d =
  S
    { sDateTime = UTCTime d 0,
      sNextBackupId = toSqlKey 0,
      sBackups = S.empty
    }

nextTime :: NominalDiffTime -> State S ()
nextTime diff = modify $ \s -> s {sDateTime = addUTCTime diff $ sDateTime s}

nextBackupId :: State S BackupId
nextBackupId = do
  next <- gets sNextBackupId
  modify $ \s -> s {sNextBackupId = toSqlKey $ succ $ fromSqlKey $ sNextBackupId s}
  pure next

makeBackup :: State S ()
makeBackup = do
  dateTime <- gets sDateTime
  next <- nextBackupId
  modify $ \s ->
    s
      { sBackups =
          S.insert
            (next, dateTime)
            (sBackups s)
      }

collectGarbage :: [(NominalDiffTime, Word)] -> State S ()
collectGarbage periods = modify $ \s ->
  s
    { sBackups =
        let backups = S.toList $ sBackups s
            backupsToKeep = decideBackupsToKeep (sDateTime s) periods backups
         in S.filter (\(bid, _) -> bid `S.member` backupsToKeep) (sBackups s)
    }

iteration :: NominalDiffTime -> [(NominalDiffTime, Word)] -> State S ()
iteration diff periods = do
  nextTime diff
  makeBackup
  collectGarbage periods

iterationSpec :: Spec
iterationSpec = do
  it "works with iterations when keeping 3 backups in the last 3 days" $ do
    let endState =
          flip execState (beginState (fromGregorian 2022 01 30)) $
            replicateM_ 9 $ iteration nominalDay [(nominalDay * 3, 3)]
    let b :: Int64 -> Int -> (BackupId, UTCTime)
        b i d = (toSqlKey i, UTCTime (fromGregorian 2022 02 d) 0)
    sBackups endState
      `shouldBe` S.fromList
        [ b 6 06,
          b 7 07,
          b 8 08
        ]
  it "works with iterations when keeping a backup every other day and one per 4 days" $ do
    let endState =
          flip execState (beginState (fromGregorian 2022 01 30)) $
            replicateM_ 12 $
              iteration
                nominalDay
                [ (nominalDay * 2, 1),
                  (nominalDay * 4, 1)
                ]
    let b :: Int64 -> Int -> (BackupId, UTCTime)
        b i d = (toSqlKey i, UTCTime (fromGregorian 2022 02 d) 0)
    sBackups endState
      `shouldBe` S.fromList
        [ b 8 08,
          b 10 10
        ]
  it "works with the default period for a day" $ do
    let endState =
          flip execState (beginState (fromGregorian 2022 05 27)) $
            replicateM_ 23 $
              iteration
                3600 -- Backup every hour
                defaultPeriods
    let b :: Int64 -> Int -> (BackupId, UTCTime)
        b i h = (toSqlKey i, UTCTime (fromGregorian 2022 05 27) (fromIntegral (h * 3600)))
    sBackups endState
      `shouldBe` S.fromList
        [ b 0 01,
          b 6 07,
          b 12 13,
          b 18 19,
          b 19 20,
          b 20 21,
          b 21 22,
          b 22 23
        ]

  it "works with the default period for a year" $ do
    let endState =
          flip execState (beginState (fromGregorian 2022 01 01)) $
            replicateM_ (365 * 24 - 1) $
              iteration
                3600 -- Backup every hour
                defaultPeriods
    let b :: Int64 -> Int -> Int -> Int -> (BackupId, UTCTime)
        b i m d h = (toSqlKey i, UTCTime (fromGregorian 2022 m d) (fromIntegral (h * 3600)))
    sBackups endState
      `shouldBe` S.fromList
        [ b 0000 01 01 01,
          b 0840 02 05 01,
          b 1680 03 12 01,
          b 2520 04 16 01,
          b 3360 05 21 01,
          b 4200 06 25 01,
          b 5040 07 30 01,
          b 5880 09 03 01,
          b 6720 10 08 01,
          b 7392 11 05 01,
          b 7560 11 12 01,
          b 7728 11 19 01,
          b 7896 11 26 01,
          b 8064 12 03 01,
          b 8232 12 10 01,
          b 8400 12 17 01,
          b 8568 12 24 01,
          b 8592 12 25 01,
          b 8616 12 26 01,
          b 8640 12 27 01,
          b 8664 12 28 01,
          b 8688 12 29 01,
          b 8712 12 30 01,
          b 8736 12 31 01,
          b 8742 12 31 07,
          b 8748 12 31 13,
          b 8754 12 31 19,
          b 8755 12 31 20,
          b 8756 12 31 21,
          b 8757 12 31 22,
          b 8758 12 31 23
        ]
