{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.OptParse.Gen where

import Smos.Data.Gen ()
import Smos.Keys.Gen ()
import Smos.OptParse.Types
import Smos.Report.OptParse.Gen ()
import Smos.Types.Gen ()
import TestImport

instance GenValid KeyConfig where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeyConfigs where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid FileKeyConfigs where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid BrowserKeyConfigs where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ReportsKeyConfigs where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid HelpKeyConfigs where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeybindingsConfiguration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Configuration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
