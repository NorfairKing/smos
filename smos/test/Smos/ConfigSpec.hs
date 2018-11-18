module Smos.ConfigSpec where

import TestImport

import Smos.OptParse.Types
import Smos.Report.OptParse

spec :: Spec
spec = do
    dhallFiles <- runIO $ getResourcesWithExtension ".dhall"
    describe "Dhall" $
        forM_ dhallFiles $ \df ->
            it (fromAbsFile df) $ do
                void $
                    parseDhallConfig df configurationDefaults configurationType

getResourcesWithExtension :: String -> IO [Path Abs File]
getResourcesWithExtension ext = do
    resourcesDir <- resolveDir' $ "test_resources/config/" ++ drop 1 ext
    fs <- snd <$> listDirRecur resourcesDir
    pure $ filter ((== ext) . fileExtension) fs
