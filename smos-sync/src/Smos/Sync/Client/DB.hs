{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

module Smos.Sync.Client.DB
  ( module Smos.Sync.Client.DB,
    module Database.Persist,
    module Database.Persist.Sql,
  )
where

import Data.Mergeful.Timed
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Path
import Smos.API

share
  [mkPersist sqlSettings, mkMigrate "syncClientAutoMigration"]
  [persistLowerCase|

ClientFile
    path (Path Rel File)
    sha256 SHA256
    time ServerTime

    UniquePath path

    deriving Show
    deriving Eq
|]
