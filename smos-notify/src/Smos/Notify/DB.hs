{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Notify.DB
  ( module Smos.Notify.DB,
    module Database.Persist,
    module Database.Persist.Sql,
  )
where

import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Path
import Smos.Data

share
  [mkPersist sqlSettings, mkMigrate "migrateNotify"]
  [persistLowerCase|

SentNotification
    hash Int
    time UTCTime

    UniqueSentNotification hash

    deriving Show
    deriving Eq
    deriving Generic
|]
