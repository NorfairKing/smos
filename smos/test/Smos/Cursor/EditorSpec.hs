module Smos.Cursor.EditorSpec where

import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos.Data
import Smos.Data.Gen ()
import Smos.Types
import Test.Syd

import Test.Syd.Validity
import UnliftIO.Resource

spec :: Spec
spec =
  modifyMaxSuccess (`div` 10) $
    modifyMaxShrinks (const 1) $ do
      describe "startEditorCursor" $
        it "works on any valid smos file" $
          forAllValid $ \sf ->
            forAllValid $ \rp ->
              withSystemTempDir "smos-test" $ \tdir -> do
                let p = tdir </> rp
                writeSmosFile p sf
                runResourceT $ do
                  errOrCursor <- startEditorCursor (StartingFile p)
                  case errOrCursor of
                    Nothing -> liftIO $ expectationFailure "Locking should have been possible"
                    Just (Left err) -> liftIO $ expectationFailure err
                    Just (Right _) -> pure ()
