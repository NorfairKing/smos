module Smos.ASCIInemaSpec (spec) where

import Control.Monad
import Path
import Path.IO
import Smos.ASCIInema
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
  fs <- runIO $ do
    examplesDir <- resolveDir' "examples"
    snd <$> listDirRecur examplesDir
  forM_ fs $ \f ->
    it ("'Just works' for this example: " <> fromAbsFile f) $
      withArgs ["record", fromAbsFile f] smosASCIInema
