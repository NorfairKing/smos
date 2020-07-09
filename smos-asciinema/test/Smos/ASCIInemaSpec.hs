module Smos.ASCIInemaSpec (spec) where

import Control.Monad
import Data.List
import Path
import Path.IO
import Smos.ASCIInema
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
  fs <- runIO $ do
    examplesDir <- resolveDir' "examples"
    filter ((== ".yaml") . fileExtension) . filter (not . hidden) . snd <$> listDirRecur examplesDir
  forM_ fs $ \f ->
    it ("'Just works' for this example: " <> fromAbsFile f) $
      withArgs ["record", fromAbsFile f, "--wait", "0.2"] smosASCIInema

hidden :: Path r File -> Bool
hidden f = ".swp" `isSuffixOf` toFilePath f
