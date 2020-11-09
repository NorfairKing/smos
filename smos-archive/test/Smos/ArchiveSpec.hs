module Smos.ArchiveSpec
  ( spec,
  )
where

import Smos.Archive
import Smos.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  describe "prepareToArchive" $ do
    it "produces valid smos files" $ producesValidsOnValids2 prepareToArchive
